## web scraping pesticide toxicity data 
## J Collins

setwd(dir = "~/OneDrive - Harper Adams University/")


#*******************************************************************************
## 01 Packages 
library(rvest)
library(dplyr)
library(dplyr)
library(purrr)


#*******************************************************************************
## 02 SCRAPER FUNCTION ####

library(rvest)
library(purrr) # For list filtering

# Function to scrape "ENVIRONMENTAL FATE" section up to "ECOTOXICOLOGY"
scrape_environmental_fate <- function(active_ingredient) {
  # Base URL for the pesticide's report page
  base_url <- "https://sitem.herts.ac.uk/aeru/ppdb/en/Reports/"
  
  # Add pesticide IDs
  pesticide_id <- switch(active_ingredient,
                         "Folpet" = "354",
                         "Azoxystrobin" = "123",
                         "Tebuconazole" = "610",
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
  
  # Locate the "ENVIRONMENTAL FATE" section
  env_fate_node <- page %>%
    html_nodes(xpath = "//td[contains(@class, 'report_section_title') and text()='ENVIRONMENTAL FATE']")
  
  if (length(env_fate_node) == 0) {
    message("ENVIRONMENTAL FATE section not found.")
    return(NULL)
  }
  
  # Locate the "ECOTOXICOLOGY" section
  ecotox_node <- page %>%
    html_nodes(xpath = "//td[contains(@class, 'report_section_title') and text()='ECOTOXICOLOGY']")
  
  if (length(ecotox_node) == 0) {
    message("ECOTOXICOLOGY section not found.")
    return(NULL)
  }
  
  # Extract tables between "ENVIRONMENTAL FATE" and "ECOTOXICOLOGY"
  section_tables <- env_fate_node %>%
    html_nodes(xpath = "./following::table[preceding::td[contains(text(), 'ENVIRONMENTAL FATE')] and following::td[contains(text(), 'ECOTOXICOLOGY')]]") %>%
    html_table(fill = TRUE)
  
  # Filter out empty tables (0 rows or 0 columns)
  filtered_tables <- section_tables %>%
    keep(~ nrow(.x) > 0 && ncol(.x) > 0)
  
  return(filtered_tables)
}

# # Example usage: Scrape tables from "ENVIRONMENTAL FATE" up to "ECOTOXICOLOGY" for Folpet
# pesticide_name_list <- c("Folpet", "Azoxystrobin")
# 
# scraped_data <- scrape_up_to_ecotoxicology("Azoxystrobin")
# 
# # Inspect the result
# print(scraped_data)






#*******************************************************************************
## 03 RUN THE SCRAPER ####

# Initialize an empty list to store data for all pesticides
env_fate_list <- list()

# List of active ingredients you want to scrape data for
pesticides <- c("Folpet", "Azoxystrobin", "Tebuconazole")

# Loop through each pesticide and scrape data
for (pesticide in pesticides) {
  # Scrape data for the current pesticide
  result <- scrape_environmental_fate(pesticide)
  
  
  # Debugging message: print the result for inspection
  print(paste("Scraped data for:", pesticide))
  
  # Store the result in the list with the pesticide name as the key
  if (!is.null(result)) {
    env_fate_list[[pesticide]] <- result
    print(paste("Data for", pesticide, "added to the list"))
  } else {
    print(paste("No valid data found for", pesticide))
  }
}


#*******************************************************************************
# NAME TIBBLES ####

# Define the desired names for the tibbles
desired_names <- c(
  "ENVIRONMENTAL FATE", 
  "Degradation", 
  "Soil adsorption and mobility", 
  "Fate indices", 
  "Known soil metabolites", 
  "Other known metabolites"
)

# Assign the desired names to each pesticide in the env_fate_list
env_fate_list <- lapply(env_fate_list, function(pesticide_tibble_list) {
  # Only apply the names if the list has the expected number of tibbles
  if (length(pesticide_tibble_list) == length(desired_names)) {
    names(pesticide_tibble_list) <- desired_names
  }
  return(pesticide_tibble_list)
})

# Check the names
print(names(env_fate_list$Folpet))
print(names(env_fate_list$Azoxystrobin))
print(names(env_fate_list$Tebuconazole))



#*******************************************************************************

print(env_fate_list$Folpet[1])

library(dplyr)

# Apply the transformation to each tibble in env_fate_list
env_fate_list <- lapply(env_fate_list, function(pesticide_tibble_list) {
  pesticide_tibble_list <- lapply(pesticide_tibble_list, function(tibble) {
    # Convert the first row to column names
    colnames(tibble) <- as.character(tibble[1, ])
    
    # Remove the first row after it becomes the header
    tibble <- tibble[-1, ]
    
    # Return the updated tibble
    return(tibble)
  })
  return(pesticide_tibble_list)
})

names(env_fate_list$Folpet)

# Check the result for Folpet to confirm the transformation
print(env_fate_list$Folpet[["Degradation"]])





#*******************************************************************************
# EXTRACTION OF VARIABLE OF INTEREST 

# Initialize an empty list to store the extracted data
soil_degradation <- list()

# Loop through each pesticide in the env_fate_list
for (pesticide in names(env_fate_list)) {
  # Check if the "Degradation" section exists for the pesticide
  if ("Degradation" %in% names(env_fate_list[[pesticide]])) {
    # Extract the 4th row and first 4 columns from the "Degradation" tibble
    degradation_rows <- env_fate_list[[pesticide]][["Degradation"]][2:8, 1:4]
    
    # Add the extracted row to the list, along with the pesticide name
    soil_degradation[[pesticide]] <- degradation_rows
  }
}

# Convert the list of extracted rows into a dataframe
soil_degradation_df <- bind_rows(soil_degradation, .id = "Pesticide")

# View the resulting dataframe
print(soil_degradation_df)













