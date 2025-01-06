## web scraping pesticide toxicity data 
## J Collins

setwd(dir = "~/OneDrive - Harper Adams University/")

getwd()

#*******************************************************************************
# PACKAGES ####


library(rvest)
library(dplyr)
library(purrr) # For list filtering
library(stringr) # replace characters
library(ggplot2) # plotting 
library(tidyr)
library(ggpubr)



#*******************************************************************************
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



#*******************************************************************************
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





#*******************************************************************************
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







#*******************************************************************************
## EXRACT DATA FROM METADATA ####

## ~~ GUS leaching ####

# Filter the desired rows
GUS_leaching_potential_index <- pesticide_fate_df %>%
  filter(Property_1 == "GUS leaching potential index")

# View the result
print(GUS_leaching_potential_index)



## ~~ Birds - Acute LD₅₀ (mg kg⁻¹) ####

# Filter the desired rows
birds_ld50 <- pesticide_fate_df %>%
  filter(Property_1 == "Birds - Acute LD₅₀ (mg kg⁻¹)")

# View the result
print(birds_ld50)


## ~~ Mammals - Acute oral LD₅₀ (mg kg⁻¹) ####

# Filter the desired rows
mammals_ld50 <- pesticide_fate_df %>%
  filter(Property_1 == "Mammals - Acute oral LD₅₀ (mg kg⁻¹)")

# View the result
print(mammals_ld50)


## ~~ Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) ####

worm_lc50 <- pesticide_fate_df %>%
  filter(Property_1 == "Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹)")

# View the result
print(worm_lc50)


## ~~ Collembola ####

collembola_lc50 <- pesticide_fate_df %>%
  filter(Property_1 == "Collembola")

# View the result
print(collembola_lc50)

collembola_lc50 <- filter(collembola_lc50, Value_1 != "Acute LC₅₀ (mg kg⁻¹)")



## ~~ Bees ####

bees_ld50 <- pesticide_fate_df %>%
  filter(Property_1 == "Honeybees (Apis spp.)", Property_3 == "Contact acute LD₅₀ (worst case from 24, 48 and 72 hour values - μg bee⁻¹)")

# View the result
print(bees_ld50)


## ~~ fish ####

fish_lc50 <- pesticide_fate_df %>%
  filter(Property_1 == "Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹)", 
         Property_3 == "Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹)")

# View the result
print(fish_lc50)









# ~ Degradation ####


## ~~ Soil degradation (days) (typical) ####

soil_dt50 <- env_degration_df %>%
  filter(Property_1 == "Soil degradation (days) (aerobic)", 
         Property_3 == "DT₅₀ (typical)")

# View the result
print(soil_dt50)

## ~~ Soil degradation (days) (field) ####

soil_dt50_field <- env_degration_df %>%
  filter(Property_1 == "Soil degradation (days) (aerobic)", 
         Property_3 == "DT₅₀ (field)")

# View the result
print(soil_dt50_field)


## ~~ Soil degradation (dt50) (lab) ####

soil_dt50_lab <- env_degration_df %>%
  filter(Property_1 == "Soil degradation (days) (aerobic)", 
         Property_3 == "DT₅₀ (lab at 20 °C)")

# View the result
print(soil_dt50_lab)













# ## ~ ecotoxicology ###
# 
# # Initialize an empty list to store data for all pesticides
# ecotox_list <- list()
# 
# # Loop through each pesticide and scrape data
# for (pesticide in pesticides) {
#   # Scrape data for the current pesticide
#   result <- scrape_ecotox(pesticide)
#   
#   
#   # Debugging message: print the result for inspection
#   print(paste("Scraped data for:", pesticide))
#   
#   # Store the result in the list with the pesticide name as the key
#   if (!is.null(result)) {
#     ecotox_list[[pesticide]] <- result
#     print(paste("Data for", pesticide, "added to the list"))
#   } else {
#     print(paste("No valid data found for", pesticide))
#   }
# }









#*******************************************************************************
# # NAME TIBBLES ###
# 
# 
# 
# ## ~ env fate ###
# 
# # Define the desired names for the tibbles
# desired_names <- c(
#   "ENVIRONMENTAL FATE", 
#   "Degradation", 
#   "Soil adsorption and mobility", 
#   "Fate indices"
# )
# 
# # Assign the desired names to each pesticide in the env_fate_list
# env_fate_list <- lapply(env_fate_list, function(pesticide_tibble_list) {
#   # Check if the list of tibbles matches the expected number of desired names
#   if (length(pesticide_tibble_list) == length(desired_names)) {
#     names(pesticide_tibble_list) <- desired_names
#   }
#   return(pesticide_tibble_list)
# })









# ## ~ ecotoxicology ###
# 
# # Define the desired names for the tibbles
# desired_names <- c(
#   "Terrestrial ecotoxicology", 
#   "Aquatic ecotoxicology"
# )
# 
# # Assign the desired names to each pesticide in the env_fate_list
# ecotox_list <- lapply(ecotox_list, function(pesticide_tibble_list) {
#   # Check if the list of tibbles matches the expected number of desired names
#   if (length(pesticide_tibble_list) == length(desired_names)) {
#     names(pesticide_tibble_list) <- desired_names
#   }
#   return(pesticide_tibble_list)
# })






#*******************************************************************************
# COLNAMES IN TIBBLES ###
# 
# # remove colnames and make row1 the colnames
# 
# print(fate_indicies_list$Azoxystrobin["Fate indices"])
# 
# ## ~ env fate ###
# 
# # Apply the transformation to each tibble in env_fate_list
# env_fate_list <- lapply(env_fate_list, function(pesticide_tibble_list) {
#   pesticide_tibble_list <- lapply(pesticide_tibble_list, function(tibble) {
#     # Convert the first row to column names
#     colnames(tibble) <- as.character(tibble[1, ])
#     
#     # Remove the first row after it becomes the header
#     tibble <- tibble[-1, ]
#     
#     # Return the updated tibble
#     return(tibble)
#   })
#   return(pesticide_tibble_list)
# })






# ## ~ ecotoxicology ###
# 
# # Apply the transformation to each tibble in env_fate_list
# ecotox_list <- lapply(ecotox_list, function(pesticide_tibble_list) {
#   pesticide_tibble_list <- lapply(pesticide_tibble_list, function(tibble) {
#     # Convert the first row to column names
#     colnames(tibble) <- as.character(tibble[1, ])
#     
#     # Remove the first row after it becomes the header
#     tibble <- tibble[-1, ]
#     
#     # Return the updated tibble
#     return(tibble)
#   })
#   return(pesticide_tibble_list)
# })




#*******************************************************************************
# EXTRACT VARIABLE GROUP OF INTEREST ###


#*******************************************************************************
## ~ ENV FATE ###

#*******************************************************************************
### ~~ degradation ###


# # Extract all 'Degradation' tibbles and bind them together
# pesticide_degradation_df <- env_fate_list %>%
#   map(~ .x[[2]]) %>%      # Extract the "Degradation" tibble from each pesticide
#   discard(is.null) %>%                 # Remove any NULL elements (if "Degradation" doesn't exist)
#   bind_rows(.id = "Pesticide")         # Bind them together and add a "Pesticide" column
# 
# 
# # set the column names here 
# 
# colnames(pesticide_degradation_df) <- c("Pesticide", "Property_1", "Property_2", 
#                                         "Property_3", "Value", "Source_quality_score", 
#                                         "Interpretation")
# 
# 
# # drop some of the columns with no useful data 
# pesticide_degradation_df <- pesticide_degradation_df %>%
#   dplyr::select(Pesticide, Property_1, Property_2, Property_3, Value, Source_quality_score, Interpretation)





#*******************************************************************************
# ### ~~ fate indies ###
# 
# # Extract all tibbles and bind them together
# 
# pesticide_fate_df <- env_fate_list %>%
#   map(~ .x[["Fate indices"]]) %>%      # Extract the "?" tibble from each pesticide
#   discard(is.null) %>%                 # Remove any NULL elements (if "Degradation" doesn't exist)
#   bind_rows(.id = "Pesticide")         # Bind them together and add a "Pesticide" column
# 
# 
# # set the column names here 
# 
# colnames(pesticide_fate_df) <- c("Pesticide", "Property_1", "Property_2", 
#                                         "Property_3", "Value", "Source_quality_score", 
#                                         "Interpretation")
# 
# 
# # drop some of the columns with no useful data 
# pesticide_degradation_df <- pesticide_degradation_df %>%
#   dplyr::select(Pesticide, Property_1, Property_2, Property_3, Value, Source_quality_score, Interpretation)




  
  



#*******************************************************************************
## ~ ECOTOXICOLOGY ###



#*******************************************************************************
# ### ~~ Terrestrial ecotoxicology ###
# 
# 
# # Extract all 'Degradation' tibbles and bind them together
# pesticide_ecotox_df <- ecotox_list %>%
#   map(~ .x[[1]]) %>%      # Extract the "Degradation" tibble from each pesticide
#   discard(is.null) %>%                 # Remove any NULL elements (if "Degradation" doesn't exist)
#   bind_rows(.id = "Pesticide")         # Bind them together and add a "Pesticide" column
# 
# 
# # set the column names here 
# 
# colnames(pesticide_ecotox_df) <- c("Pesticide", "Property_1", "Property_2", 
#                                         "Property_3", "Value", "Source_quality_score", 
#                                         "Interpretation")
# 
# 
# # drop some of the columns with no useful data 
# pesticide_ecotox_df <- pesticide_ecotox_df %>%
#   dplyr::select(Pesticide, Property_1, Property_2, Property_3, Value, Source_quality_score, Interpretation)








#*******************************************************************************
# EXTRACT VARIABLE SUB-GROUP OF INTEREST ###




## ~ ENV FATE ###

#*******************************************************************************
# ### ~~ soil degradation ###
# 
# 
# 
# 
# # filter to just the soil data 
# soil_degradation_df <- filter(.data = pesticide_degradation_df, Property_1 == "Soil degradation (days) (aerobic)")
# 
# # remove any of the "note" rows
# soil_degradation_df <- filter(.data = soil_degradation_df, Property_2 != "Note")
# 
# # there are some duplicated columns which move the "value" col. Remove duplicates and set to "value"
# soil_degradation_df$Value <- if_else(condition = soil_degradation_df$Value == "DT₅₀ (typical)", 
#                                true = soil_degradation_df$Source_quality_score, 
#                                false = soil_degradation_df$Value)
# 
# 
# # Merge Property_1, Property_2, and Property_3 into one column for better info
# soil_degradation_df <- soil_degradation_df %>%
#   unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)







#*******************************************************************************
## ~ ECOTOX ###



#*******************************************************************************
# ### ~~ Mammals ###
# 
# colnames(pesticide_ecotox_df)
# 
# unique(mammal_ecotox_df$Value)
# 
# # Filter rows where a specific column contains "Mammals"
# mammal_ecotox_df <- pesticide_ecotox_df %>%
#   filter(str_detect(Property_1, "Mammals"))
# 
# 
# # there are some duplicated columns which move the "value" col. Remove duplicates and set to "value"
# mammal_ecotox_df$Value <- if_else(condition = mammal_ecotox_df$Value == "(mg kg⁻¹)", 
#                                      true = mammal_ecotox_df$Source_quality_score, 
#                                      false = mammal_ecotox_df$Value)
# 
# 
# # Merge Property_1, Property_2, and Property_3 into one column for better info
# mammal_ecotox_df <- mammal_ecotox_df %>%
#   unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)









#*******************************************************************************
# PIVOT DFS ####

# Pivot the dataframe for merging


#*******************************************************************************
## ~ ENV FATE PIVOT ####


### ~~ Soil Degradation typical ####


# Merge Property_1, Property_2, and Property_3 into one column for better info
soil_dt50 <- soil_dt50 %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(soil_dt50)

# Remove unwanted columns
soil_dt50 <- soil_dt50 %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`)

print(soil_dt50)

# Now proceed with reshaping the data
soil_dt50 <- soil_dt50 %>%
  pivot_wider(names_from = Property, values_from = Value_2)

print(soil_dt50)



### ~~ Soil Degradation field ####


# Merge Property_1, Property_2, and Property_3 into one column for better info
soil_dt50_field <- soil_dt50_field %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(soil_dt50_field)

# Remove unwanted columns
soil_dt50_field <- soil_dt50_field %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`)

# Now proceed with reshaping the data
soil_dt50_field <- soil_dt50_field %>%
  pivot_wider(names_from = Property, values_from = Value_1)



### ~~ Soil Degradation lab ####


# Merge Property_1, Property_2, and Property_3 into one column for better info
soil_dt50_lab <- soil_dt50_lab %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(soil_dt50_lab)

# Remove unwanted columns
soil_dt50_lab <- soil_dt50_lab %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`)

# Now proceed with reshaping the data
soil_dt50_lab <- soil_dt50_lab %>%
  pivot_wider(names_from = Property, values_from = Value_1)





# # Explicitly use dplyr's select function to remove the unwanted columns
# soil_degradation_df <- dplyr::select(soil_degradation_df, -Source_quality_score, -Interpretation)
# 
# glimpse(soil_degradation_df)
# 
# # Now proceed with reshaping the data
# soil_degradation_df_wide <- soil_degradation_df %>%
#   pivot_wider(names_from = Property, values_from = Value)
# 
# # # View the reshaped dataframe
# # glimpse(soil_degradation_df_wide)








#*******************************************************************************
## ~ ECOTOX PIVOT ####



#*******************************************************************************
### ~~ Mammals ####

# Merge Property_1, Property_2, and Property_3 into one column for better info
mammals_ld50 <- mammals_ld50 %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(mammals_ld50)

# Remove unwanted columns
mammals_ld50 <- mammals_ld50 %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
                          -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
                          -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

colnames(mammals_ld50)

mammals_ld50 <- mammals_ld50 %>%
  distinct(Pesticide, Property, .keep_all = TRUE) %>%
  pivot_wider(names_from = Property, values_from = Value_1)




### ~~ Earthworm LC50 ####


# Merge Property_1, Property_2, and Property_3 into one column for better info
worm_lc50 <- worm_lc50 %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(worm_lc50)

# Remove unwanted columns
worm_lc50 <- worm_lc50 %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

# Now proceed with reshaping the data
worm_lc50 <- worm_lc50 %>%
  pivot_wider(names_from = Property, values_from = Value_1)



### ~~ Collembola LC50 ####


# Merge Property_1, Property_2, and Property_3 into one column for better info
collembola_lc50 <- collembola_lc50 %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(collembola_lc50)

# Remove unwanted columns
collembola_lc50 <- collembola_lc50 %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

# Now proceed with reshaping the data
collembola_lc50 <- collembola_lc50 %>%
  pivot_wider(names_from = Property, values_from = Value_1)




### ~~ Bees LD50 ####

# Merge Property_1, Property_2, and Property_3 into one column for better info
bees_ld50 <- bees_ld50 %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(bees_ld50)

# Remove unwanted columns
bees_ld50 <- bees_ld50 %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

glimpse(bees_ld50)

# Now proceed with reshaping the data
bees_ld50 <- bees_ld50 %>%
  pivot_wider(names_from = Property, values_from = Value_2)



### ~~ Birds LD50 ####

glimpse(birds_ld50)

# Merge Property_1, Property_2, and Property_3 into one column for better info
birds_ld50 <- birds_ld50 %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(birds_ld50)

# Remove unwanted columns
birds_ld50 <- birds_ld50 %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

glimpse(birds_ld50)

# Now proceed with reshaping the data
birds_ld50 <- birds_ld50 %>%
  pivot_wider(names_from = Property, values_from = Value_1)



### ~~ Fish LC50 ####

glimpse(fish_lc50)

# Merge Property_1, Property_2, and Property_3 into one column for better info
fish_lc50 <- fish_lc50 %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(fish_lc50)

# Remove unwanted columns
fish_lc50 <- fish_lc50 %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

glimpse(fish_lc50)

# Now proceed with reshaping the data
fish_lc50 <- fish_lc50 %>%
  pivot_wider(names_from = Property, values_from = Value_1)




### ~~ GUS leaching ####

print(GUS_leaching_potential_index)

# Merge Property_1, Property_2, and Property_3 into one column for better info
GUS_leaching_potential_index <- GUS_leaching_potential_index %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(GUS_leaching_potential_index)

# Remove unwanted columns
GUS_leaching_potential_index <- GUS_leaching_potential_index %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

# Now proceed with reshaping the data
GUS_leaching_potential_index <- GUS_leaching_potential_index %>%
  pivot_wider(names_from = Property, values_from = Value_2)











#*******************************************************************************
# JOIN DATA ####


### ~~ load data ####

# read application data in 
usage_dat <- read.csv(file = "~/OneDrive - Harper Adams University/Data/LCA/data/processed_data/summary_normalised_LCA_data.csv")

usage_dat <- usage_dat %>%
  mutate(
    year = case_when(
      crop == "Spring beans" ~ 2022,
      crop == "Winter wheat" ~ 2023,
      crop %in% c("Oilseed Rape", "Spring Barley") ~ 2024,
      TRUE ~ NA_real_  # Default case if none of the conditions are met
    )
  ) %>%
  select(year, everything())  # Reorder columns to make `year` the first column





### ~~ make the combined df ####

# Assuming Pesticide in soil_degradation_df corresponds to ai_name in usage_dat

# Example: Selecting specific columns to join
combined_dat <- usage_dat %>%
  left_join(soil_dt50 %>% select(Pesticide, `Soil degradation (days) (aerobic) Soil degradation (days) (aerobic) DT₅₀ (typical)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(soil_dt50_field %>% select(Pesticide, `Soil degradation (days) (aerobic) DT₅₀ (field) DT₅₀ (field)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(soil_dt50_lab %>% select(Pesticide, `Soil degradation (days) (aerobic) DT₅₀ (lab at 20 °C) DT₅₀ (lab at 20 °C)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(mammals_ld50 %>% select(Pesticide, `Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(worm_lc50 %>% select(Pesticide,  `Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(GUS_leaching_potential_index %>% 
              select(Pesticide,`GUS leaching potential index GUS leaching potential index GUS leaching potential index`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(collembola_lc50 %>% 
              select(Pesticide,`Collembola Chronic NOEC (mg kg⁻¹) Chronic NOEC (mg kg⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(bees_ld50 %>% 
              select(Pesticide, `Honeybees (Apis spp.) Honeybees (Apis spp.) Contact acute LD₅₀ (worst case from 24, 48 and 72 hour values - μg bee⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(birds_ld50 %>% 
              select(Pesticide, `Birds - Acute LD₅₀ (mg kg⁻¹) Birds - Acute LD₅₀ (mg kg⁻¹) Birds - Acute LD₅₀ (mg kg⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(fish_lc50 %>% 
              select(Pesticide, `Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹) Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹) Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹)`), 
            by = c("ai_name" = "Pesticide")) 







### ~~ prep joined data ####

# remove "-" character and set to NA
combined_dat <- combined_dat %>%
  mutate(across(where(is.character), ~na_if(., "-")))

# Remove ">" symbols from character columns
combined_dat <- combined_dat %>%
  mutate(across(where(is.character), ~str_replace_all(., ">", "")))

# set cols as numeric cols
combined_dat <- combined_dat %>%
  mutate(across(5:ncol(combined_dat), ~as.numeric(.), .names = "{col}"))





#*******************************************************************************
# CALCULATE LOAD INDEXS ####

# add equation parameters
application_rate <- combined_dat$sum_normalized_rate_kg_ha
DT50 <- combined_dat$`Soil degradation (days) (aerobic) DT₅₀ (field) DT₅₀ (field)`
LD50 <- combined_dat$`Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹)`


## ~ PLI ####

combined_dat$pesticide_load_index <- application_rate * (DT50 / LD50)


## ~ TLI ####

combined_dat$toxic_load_index <- application_rate * (1 / LD50)


## ~ worm risk index ####

combined_dat$worm_risk_index <- application_rate / combined_dat$`Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹)`

## ~ bee risk index ####

combined_dat$bee_risk_index <- application_rate / combined_dat$`Honeybees (Apis spp.) Honeybees (Apis spp.) Contact acute LD₅₀ (worst case from 24, 48 and 72 hour values - μg bee⁻¹)`

## ~ collembola risk index ####

# combined_dat$collembola_risk_index <- application_rate / combined_dat$`Collembola Chronic NOEC (mg kg⁻¹) Chronic NOEC (mg kg⁻¹)`

## ~ mammal risk index ####

combined_dat$mammal_risk_index <- application_rate / combined_dat$`Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹)`

## ~ birds risk index ####
combined_dat$birds_risk_index <- application_rate / combined_dat$`Birds - Acute LD₅₀ (mg kg⁻¹) Birds - Acute LD₅₀ (mg kg⁻¹) Birds - Acute LD₅₀ (mg kg⁻¹)`

## ~ fish risk index ####
combined_dat$fish_risk_index <- application_rate / combined_dat$`Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹) Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹) Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹)`


#*******************************************************************************
# SAVE DATA ####

write.csv(x = combined_dat, file = "Data/agronomy/data/pesticide_data/pesticide_properties_data.csv")



#*******************************************************************************
# SUMMARY TABLES ####


## ~ PLI ####

#PLI by treamtent and year
pli_sum <- combined_dat %>%
  group_by(treatment, year) %>%
  summarise(
    n = n(),
    sum = round(x = sum(pesticide_load_index, na.rm = TRUE), digits = 2)
  )


# total PLI for each treatment
total_pli_sum <- combined_dat %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    sum = round(x = sum(pesticide_load_index, na.rm = TRUE), digits = 2)
  )



## ~ TLI ####

#PLI by treamtent and year
tli_sum <- combined_dat %>%
  group_by(treatment, year) %>%
  summarise(
    n = n(),
    sum = round(x = sum(toxic_load_index, na.rm = TRUE), digits = 4)
  )


# total PLI for each treatment
total_tli_sum <- combined_dat %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    sum = round(x = sum(toxic_load_index, na.rm = TRUE), digits = 4)
  )




## ~ worm risk index ####
worm_risk_index_sum <- combined_dat %>%
  group_by(treatment, year) %>%
  summarise(
    n = n(),
    sum = round(x = sum(worm_risk_index, na.rm = TRUE), digits = 4)
  )


## ~ bee risk index ####
bee_risk_index_sum <- combined_dat %>%
  group_by(treatment, year) %>%
  summarise(
    n = n(),
    sum = round(x = sum(bee_risk_index, na.rm = TRUE), digits = 4)
  )

## ~ mammal risk index ####
mammal_risk_index_sum <- combined_dat %>%
  group_by(treatment, year) %>%
  summarise(
    n = n(),
    sum = round(x = sum(mammal_risk_index, na.rm = TRUE), digits = 4)
  )

## ~ bee risk index ####
collembola_risk_index_sum <- combined_dat %>%
  group_by(treatment, year) %>%
  summarise(
    n = n(),
    sum = round(x = sum(collembola_risk_index, na.rm = TRUE), digits = 4)
  )

## ~ birds risk index ####
birds_risk_index_sum <- combined_dat %>%
  group_by(treatment, year) %>%
  summarise(
    n = n(),
    sum = round(x = sum(birds_risk_index, na.rm = TRUE), digits = 4)
  )

## ~ fish risk index ####
fish_risk_index_sum <- combined_dat %>%
  group_by(treatment, year) %>%
  summarise(
    n = n(),
    sum = round(x = sum(fish_risk_index, na.rm = TRUE), digits = 4)
  )





## ~ GUS index ####

#PLI by treamtent and year
GUS_index_sum <- combined_dat %>%
  group_by(treatment, year) %>%
  summarise(
    n = n(),
    sum = round(x = sum(`GUS leaching potential index GUS leaching potential index GUS leaching potential index`, na.rm = TRUE), digits = 4)
  )





#*******************************************************************************
# PLOTS ####


#*******************************************************************************
## ~ soil DT50 plots ####



### ~~ general soil half life plot ####

# Assuming 'combined_dat' has a column for 'Pesticide' and 'Soil degradation'
a <- ggplot(combined_dat, 
            aes(x = ai_name, 
                y = `Soil degradation (days) (aerobic) Soil degradation (days) (aerobic) DT₅₀ (typical)`, 
             fill = ai_name)) +
  geom_col() +  # Use geom_col() when you already have the values
  theme_minimal() +
   ylim(0,800) +
  labs(
    title = expression("Typical" ~ DT[50] ~ "(days)"),
    x = "Pesticide Active Ingrdient",
    y = expression("Soil Degradation Time " ~ DT[50] ~ "(days)"),
    fill = "Active Ingrdient"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()  # Optionally remove x-axis ticks
  )
  # theme(axis.text.x = element_text(angle = -45, hjust = 0))  # Rotate x-axis labels if necessary

a

# Assuming 'combined_dat' has a column for 'Pesticide' and 'Soil degradation'
b <- ggplot(combined_dat, 
       aes(x = ai_name, 
           y = `Soil degradation (days) (aerobic) DT₅₀ (field) DT₅₀ (field)`, 
        fill = ai_name)) +
  geom_col() +  # Use geom_col() when you already have the values
  theme_minimal() + 
  ylim(0,800) +
  labs(
    title = expression("Field" ~ DT[50] ~ "(days)"),
    x = "Pesticide Active Ingrdient",
    y = expression("Soil Degradation Time " ~ DT[50] ~ "(days)"),
    fill = "Active Ingrdient"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()  # Optionally remove x-axis ticks
  )
  # theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 6))  # Rotate x-axis labels if necessary

b

ggsave(filename = "Data/agronomy/plots/ai_degradation_soil.png", width = 10, height = 4)


# Assuming 'combined_dat' has a column for 'Pesticide' and 'Soil degradation'
c <- ggplot(combined_dat, aes(x = ai_name, 
                         y = `Soil degradation (days) (aerobic) DT₅₀ (lab at 20 °C) DT₅₀ (lab at 20 °C)`, 
                         fill = ai_name)) +
  geom_col() +  # Use geom_col() when you already have the values
  theme_minimal() +
  ylim(0,800) +
  labs(
    title = expression("Lab at 20 °C" ~ DT[50] ~ "(days)"),
    x = "Pesticide Active Ingrdient",
    y = expression("Soil Degradation Time " ~ DT[50] ~ "(days)"),
    fill = "Active Ingrdient"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()  # Optionally remove x-axis ticks
  )
  # theme(axis.text.x = element_text(angle = -45, hjust = 0,))  # Rotate x-axis labels if necessary

c

ggarrange(a,b,c, 
          ncol = 3, 
          nrow = 1, 
          labels = c("A","B","C"), 
          common.legend = TRUE, 
          legend = "bottom", align = "h")

ggsave(filename = "Data/agronomy/plots/ai_soil_degradation_all.png", width = 10, height = 5.5)





## ~ leaching potential ####

ggplot(combined_dat, aes(x = ai_name, 
                         y = `GUS leaching potential index GUS leaching potential index GUS leaching potential index`, 
                         fill = ai_name)) +
  geom_col() +  # Use geom_col() when you already have the values
  theme_minimal() +
  ylim(0,9) +
  labs(
    title = expression("Lab at 20 °C" ~ DT[50] ~ "(days)"),
    x = "Pesticide Active Ingrdient",
    y = expression("Soil Degradation Time " ~ DT[50] ~ "(days)"),
    fill = "Active Ingrdient"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()  # Optionally remove x-axis ticks
  )
# theme(axis.text.x = element_text(angle = -45, hjust = 0,))  # Rotate x-axis labels if necessary











#*******************************************************************************
## ~ TOXICITY ####



### ~~ mammals ####

# Assuming 'combined_dat' has a column for 'Pesticide' and 'Soil degradation'
a <- ggplot(combined_dat, 
            aes(x = ai_name, 
                y = `Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹)`, 
                fill = ai_name)) +
  geom_col() +  # Use geom_col() when you already have the values
  theme_minimal() +
  # ylim(0,800) +
  labs(
    title = expression("Mammals - Acute oral" ~ LD[50] ~ (mg ~ kg^{-1})),
    x = "Pesticide Active Ingrdient",
    y = expression(LD[50] ~ (mg ~ kg^{-1})),
    fill = "Active Ingrdient"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()  # Optionally remove x-axis ticks
  )
# theme(axis.text.x = element_text(angle = -45, hjust = 0))  # Rotate x-axis labels if necessary

a




### ~~ worms ####

# Assuming 'combined_dat' has a column for 'Pesticide' and 'Soil degradation'
b <- ggplot(combined_dat, 
            aes(x = ai_name, 
                y = `Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹)`, 
                fill = ai_name)) +
  geom_col() +  # Use geom_col() when you already have the values
  theme_minimal() +
  # ylim(0,800) +
  labs(
    title = expression("Earthworms - Acute oral" ~ LD[50] ~ (mg ~ kg^{-1})),
    x = "Pesticide Active Ingrdient",
    y = expression(LD[50] ~ (mg ~ kg^{-1})),
    fill = "Active Ingrdient"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()  # Optionally remove x-axis ticks
  )
# theme(axis.text.x = element_text(angle = -45, hjust = 0))  # Rotate x-axis labels if necessary

b




#*******************************************************************************
## ~ by treatment ####


### ~~ degradation in soil ####

ggplot(combined_dat, 
            aes(x = ai_name, 
                y = `Soil degradation (days) (aerobic) DT₅₀ (field) DT₅₀ (field)`, 
                fill = ai_name)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = expression("Field" ~ DT[50] ~ "(days)"),
    x = "Pesticide Active Ingredient",
    y = expression("Soil Degradation Time " ~ DT[50] ~ "(days)"),
    fill = "Active Ingredient"
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(),
    legend.position = "bottom") +
  facet_wrap(~ year + treatment, ncol = 6)  # Facet by the treatment column

ggsave(filename = "Data/agronomy/plots/ai_degradation_soil_by_treatment_year.png", width = 10, height = 6)




### ~~ worm risk ####

ggplot(combined_dat, 
       aes(x = ai_name, 
           y = worm_risk_index, 
           fill = ai_name)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = expression("Field" ~ DT[50] ~ "(days)"),
    x = "Pesticide Active Ingredient",
    y = expression("Soil Degradation Time " ~ DT[50] ~ "(days)"),
    fill = "Active Ingredient"
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(),
    legend.position = "bottom") +
  facet_wrap(~ year + treatment, ncol = 6)  # Facet by the treatment column





#*******************************************************************************
## ~ PLI plots ####

# plot PLI for each AI x treatment
ggplot(combined_dat, 
       aes(x = ai_name, 
           y = pesticide_load_index, 
           fill = ai_name)) +
  geom_col() +
  theme_classic() +
  labs(
    title = expression("Pesticide Load Index (PLI)"),
    x = "Pesticide Active Ingredient",
    y = expression("PLI"),
    fill = "Active Ingredient"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()) +
  facet_wrap(~ treatment)

ggsave(filename = "Data/agronomy/plots/pesticide_load_index.png", width = 10, height = 5)



# plot PLI by treatment and year
pli_plot <-
  ggplot(data = pli_sum, 
       aes(x = treatment, 
           y = sum, 
           fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Pesticide Load Index (PLI)",
    subtitle = "Pesticide Load Index (PLI)", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

pli_plot





#*******************************************************************************
## ~ mammal ecotox ####



a <- ggplot(combined_dat, 
            aes(x = ai_name, 
                y = `Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹)`, 
                fill = ai_name)) +
  geom_col() +  # Use geom_col() when you already have the values
  theme_minimal() +
  # ylim(0,800) +
  labs(
    title = expression("Mammal Acute Oral" ~ LD[50] ~ (mg ~ kg^{-1})),
    x = "Pesticide Active Ingrdient",
    y = expression(LD[50] ~ (mg ~ kg^{-1})),
    fill = "Active Ingrdient"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()  # Optionally remove x-axis ticks
  )
# theme(axis.text.x = element_text(angle = -45, hjust = 0))  # Rotate x-axis labels if necessary

a







#*******************************************************************************
## ~ TLI plots ####


# plot TLI for each AI x treatment
ggplot(combined_dat, 
       aes(x = ai_name, 
           y = toxic_load_index, 
           fill = ai_name)) +
  geom_col() +
  theme_classic() +
  labs(
    title = expression("Toxic Load Index (TLI)"),
    x = "Pesticide Active Ingredient",
    y = expression("TLI"),
    fill = "Active Ingredient"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()) +
  facet_wrap(~ treatment)

ggsave(filename = "Data/agronomy/plots/toxic_load_index.png", width = 10, height = 5)



# plot PLI by treatment and year
tli_plot <-
  ggplot(data = tli_sum, 
         aes(x = treatment, 
             y = sum, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    subtitle = "Toxic Load Index (TLI)", 
    x = "Treatment",
    y = "TLI",
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

tli_plot


# plot PLI by treatment and year
pli_plot <-
  ggplot(data = pli_sum, 
         aes(x = treatment, 
             y = sum, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    subtitle = "Pesticide Load Index (PLI)", 
    x = "Treatment",
    y = "PLI",
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

pli_plot



## ~ GUS index ####

# plot worm index by treatment and year
gus_plot <-
  ggplot(data = GUS_index_sum, 
         aes(x = treatment, 
             y = sum, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    subtitle = "Groundwater Ubiquity Score (GUS) Index", 
    x = "Treatment",
    y = "GUS Index",
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

gus_plot

ggarrange(pli_plot, tli_plot, gus_plot, 
          ncol = 3, 
          nrow = 1, 
          common.legend = TRUE, 
          legend = "bottom", labels = c("A","B","C"))



## ~~ save PLI, TLI, AND GUS PLOT ####

ggsave(filename = "Data/agronomy/plots/pli_tli_gus.png", width = 10, height = 4)





#******************************************************************************
## ~~ eco risk plot by treatment x year ####


## ~ worm risk index ####

# plot worm index by treatment and year
worm_plot <-
  ggplot(data = worm_risk_index_sum, 
         aes(x = treatment, 
             y = sum, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Earthworm Risk Index",
    subtitle = "Earthworm Risk Index", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

worm_plot


## ~ mammal risk index ####

# plot worm index by treatment and year
mammal_plot <-
  ggplot(data = mammal_risk_index_sum, 
         aes(x = treatment, 
             y = sum, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Mammal Risk Index",
    subtitle = "Mammal Risk Index", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

mammal_plot



## ~ collembola risk index ####

# plot worm index by treatment and year
collembola_plot <-
  ggplot(data = collembola_risk_index_sum, 
         aes(x = treatment, 
             y = sum, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    subtitle = "Collembola Risk Index",
    x = "Treatment",
    y = "Risk Index",
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 
collembola_plot


## ~ birds risk index ####

# plot worm index by treatment and year
birds_plot <-
  ggplot(data = birds_risk_index_sum, 
         aes(x = treatment, 
             y = sum, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    subtitle = "Birds Risk Index",
    x = "Treatment",
    y = "Risk Index",
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 
birds_plot



## ~ bee risk index ####

names(bees_ld50)

# plot worm index by treatment and year
bee_plot <-
  ggplot(data = bee_risk_index_sum, 
         aes(x = treatment, 
             y = sum, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    subtitle = "Honey Bee (Apis spp.) Risk Index",
    x = "Treatment",
    y = "Risk Index",
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 
bee_plot


## ~ fish risk index ####

names(fish_lc50)

# plot worm index by treatment and year
fish_plot <-
  ggplot(data = fish_risk_index_sum, 
         aes(x = treatment, 
             y = sum, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    subtitle = "Freshwater Fish Risk Index",
    x = "Treatment",
    y = "Risk Index",
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 
fish_plot


ggarrange(worm_plot, collembola_plot, bee_plot, mammal_plot, birds_plot, fish_plot, 
          ncol = 3, 
          nrow = 2, 
          common.legend = TRUE, 
          legend = "bottom", 
          labels = c("A","B","C","E","F","G"))

ggsave(filename = "Data/agronomy/plots/ecotox_risk_indexes.png", width = 10, height = 6)



