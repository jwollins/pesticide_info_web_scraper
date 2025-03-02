## web scraping pesticide toxicity data 
## J Collins




setwd(rstudioapi::getActiveProject())

getwd()



#________________________________________________________________________####
# PACKAGES ####


library(rvest)
library(dplyr)
library(purrr) # For list filtering
library(stringr) # replace characters
library(ggplot2) # plotting 
library(tidyr)
library(ggpubr)






#________________________________________________________________________####

# ENV FATE SCRAPER ####



scrape_all_tables <- function(active_ingredient) {
  # Base URL for the pesticide's report page
  base_url <- "https://sitem.herts.ac.uk/aeru/ppdb/en/Reports/"
  
  # Add pesticide IDs
  pesticide_id <- switch(active_ingredient,
                         "2-chloroethylphosphonic acid" = "274",
                         "Azoxystrobin" = "123",
                         "Bixafen" = "1250",
                         "Boscalid" = "86",
                         "Chlormequat" = "3210",
                         "Chlorotoluron" = "151",
                         "Clomazone" = "168",
                         "Diflufenican" = "235",
                         "Fenpicoxamid" = "3073",
                         "Ferric Phosphate" = "1478",
                         "Florasulam" = "322",
                         "Flufenacet" = "331",
                         "Fluopyram" = "1362",
                         "Fluroxypyr" = "347",
                         "Fluxapyroxad" = "2002",
                         "Folpet" = "354",
                         "Glyphosate" = "373",
                         "Halauxifen-methyl" = "2630",
                         "Imazamox" = "392",
                         "Lambda-cyhalothrin" = "415",
                         "Mefentrifluconazole" = "3098",
                         "Pendimethalin" = "511",
                         "Picloram" = "525",
                         "Propaquizafop" = "546",
                         "Prothioconazole" = "559",
                         "Pyraclostrobin" = "564",
                         "Tebuconazole" = "610",
                         "Tribenuron-methyl" = "655",
                         "Trinexapac-ethyl" = "672",
                         NULL)
  
  if (is.null(pesticide_id)) {
    message("Pesticide ID not found.")
    return(NULL)
  }
  
  # Construct the full URL
  full_url <- paste0(base_url, pesticide_id, ".htm")
  
  # Read the HTML content of the page
  page <- tryCatch({
    read_html(full_url)
  }, error = function(e) {
    message("Error: Unable to open connection to the website.")
    return(NULL)
  })
  
  if (is.null(page)) return(NULL)
  
  # Extract all tables on the page
  all_tables <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Filter out empty tables (0 rows or 0 columns)
  filtered_tables <- all_tables %>%
    keep(~ nrow(.x) > 0 && ncol(.x) > 0)
  
  # Return the list of non-empty tables
  return(filtered_tables)
}




#*******************************************************************************


#*******************************************************************************
## ~ List treatment pesticides ####


# List of active ingredients you want to scrape data for
pesticides <- c("2-CEPA (Ethephon)",
                "Azoxystrobin",
                "Bixafen",
                "Boscalid",
                "Chlormequat",
                "Chlorotoluron",
                "Clomazone",
                "Diflufenican",
                "Fenpicoxamid",
                "Ferric Phosphate",
                "Florasulam",
                "Flufenacet",
                "Fluopyram",
                "Fluroxypyr",
                "Fluxapyroxad",
                "Folpet",
                "Glyphosate",
                "Halauxifen-methyl",
                "Imazamox",
                "Lambda-cyhalothrin",
                "Mefentrifluconazole",
                "Pendimethalin",
                "Picloram",
                "Propaquizafop",
                "Prothioconazole",
                "Pyraclostrobin",
                "Tebuconazole",
                "Tribenuron-methyl",
                "Trinexapac-ethyl")


# this is the list of all active ingredients used in the experiment for all treatments






#________________________________________________________________________####
# RUN THE SCRAPER ####



# run the scraper 
# Initialize an empty list to store data for all pesticides
fate_indicies_list <- list()

# Loop through each pesticide and scrape data

for (pesticide in pesticides) {
  # Scrape data for the current pesticide
  result <- scrape_all_tables(pesticide)
  
  
  # Debugging message: print the result for inspection
  print(paste("Scraped data for:", pesticide))
  
  # Store the result in the list with the pesticide name as the key
  if (!is.null(result)) {
    fate_indicies_list[[pesticide]] <- result
    print(paste("Data for", pesticide, "added to the list"))
  } else {
    print(paste("No valid data found for", pesticide))
  }
}



# can't find it easily? Use this function to search for a term and rename the tibble

rename_tibble_by_phrase <- function(tibble_list, phrase_to_find, new_tibble_name) {
  # Loop through each tibble in the list
  for (i in seq_along(tibble_list)) {
    tibble <- tibble_list[[i]]
    
    # Check if the phrase exists in any cell of the tibble
    if (any(grepl(phrase_to_find, tibble, ignore.case = TRUE))) {
      # Rename the tibble at the matching index to "Fate indices"
      names(tibble_list)[i] <- new_tibble_name
      return(tibble_list)  # Return updated list after renaming
    }
  }
  
  # If the phrase is not found, return the original list with a message
  message("Phrase not found in any tibbles.")
  return(tibble_list)
}





#*******************************************************************************
## ~ fate indices ####



# name the tibble where the target data is found

for (i in seq_along(fate_indicies_list)) {
  # Rename the tibble containing "GUS leaching potential index" to "Fate indices"
  fate_indicies_list[[i]] <- rename_tibble_by_phrase(
    tibble_list = fate_indicies_list[[i]], 
    phrase_to_find = "GUS leaching potential index", 
    new_tibble_name = "Fate indices"
  )
}



#*******************************************************************************
## ~ Degradation ####


# name the tibble where the target data is found

for (i in seq_along(fate_indicies_list)) {
  # Rename the tibble containing "GUS leaching potential index" to "Fate indices"
  fate_indicies_list[[i]] <- rename_tibble_by_phrase(
    tibble_list = fate_indicies_list[[i]], 
    phrase_to_find = "Soil degradation", 
    new_tibble_name = "Degradation"
  )
}


# Check the names of the updated tibble list
names(fate_indicies_list$Florasulam)

print(fate_indicies_list$Florasulam[21])





#________________________________________________________________________####
# COMBINE DATA FROM SINGLE VARIABLES ####

# Combine all "Fate indices" tibbles into a single data frame


## ~ Fate indices ####

pesticide_fate_df <- fate_indicies_list %>%
  map(~ .x[["Fate indices"]]) %>%  # Extract the "Fate indices" tibble from each pesticide
  discard(is.null) %>%             # Remove NULL elements (if "Fate indices" doesn't exist)
  imap(~ mutate(.x, Pesticide = .y)) %>%  # Add the Pesticide name (list name) to each tibble
  bind_rows()  # Combine all tibbles into one data frame


# Rearrange columns to make "Pesticide" the first column
pesticide_fate_df <- pesticide_fate_df %>%
  select(Pesticide, everything())

# Step 2: Adjust column names (make first row the header)
colnames(pesticide_fate_df) <- pesticide_fate_df[1, ]  # Set the first row as column names
pesticide_fate_df <- pesticide_fate_df[-1, ]  # Remove the first row

# View the updated data frame
head(pesticide_fate_df)


colnames(pesticide_fate_df) <- c("Pesticide", 
                                 "Property_1", "Property_2", "Property_3", 
                                 "Value_1",  "Value_2", "Value_3",
                                 "Source_quality_score", "Interpretation")

# Fix column names if necessary
colnames(pesticide_fate_df) <- make.names(colnames(pesticide_fate_df), unique = TRUE)








#*******************************************************************************
## ~ DEGRADATION ####



env_degration_df <- fate_indicies_list %>%
  map(~ .x[["Degradation"]]) %>%  # Extract the "Fate indices" tibble from each pesticide
  discard(is.null) %>%             # Remove NULL elements (if "Fate indices" doesn't exist)
  imap(~ mutate(.x, Pesticide = .y)) %>%  # Add the Pesticide name (list name) to each tibble
  bind_rows()  # Combine all tibbles into one data frame


# Rearrange columns to make "Pesticide" the first column
env_degration_df <- env_degration_df %>%
  select(Pesticide, everything())

# Step 2: Adjust column names (make first row the header)
colnames(env_degration_df) <- env_degration_df[1, ]  # Set the first row as column names
env_degration_df <- env_degration_df[-1, ]  # Remove the first row

# View the updated data frame
head(env_degration_df)


colnames(env_degration_df) <- c("Pesticide", 
                                 "Property_1", "Property_2", "Property_3", 
                                 "Value_1",  "Value_2", "Value_3",
                                 "Source_quality_score", "Interpretation")

# make colnames unique 
colnames(env_degration_df) <- make.names(colnames(env_degration_df), unique = TRUE)





#________________________________________________________________________####
## save df's  ####


write.csv(x = pesticide_fate_df, file = "sym_link_pesticide_data/data/pesticide_data/PPD_pesticide_dat.csv")

write.csv(x = env_degration_df, file = "sym_link_pesticide_data/data/pesticide_data/PPD_env_degradation_dat.csv")





