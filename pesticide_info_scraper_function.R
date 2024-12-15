## web scraping pesticide toxicity data 
## J Collins

setwd(dir = "~/OneDrive - Harper Adams University/")

## 01 Packages 
library(rvest)
library(dplyr)


## 02 SCRAPER FUNCTION ####

library(dplyr)
library(purrr)

# Example function to scrape PPDB and return the data as a list of tables
scrape_ppdb <- function(active_ingredient) {
  # Create the direct URL for the pesticide's report page
  base_url <- "https://sitem.herts.ac.uk/aeru/ppdb/en/Reports/"
  
  # Add more pesticides and their corresponding IDs
  pesticide_id <- switch(active_ingredient,
                         "Folpet" = "354",
                         "Azoxystrobin" = "123",  # Replace with actual ID for Azoxystrobin
                         "Tebuconazole" = "456",  # Replace with actual ID for Tebuconazole
                         NULL)  # Return NULL if the pesticide is not found
  
  # Check if pesticide_id is valid
  if (is.null(pesticide_id)) {
    message("Pesticide ID not found.")
    return(NULL)
  }
  
  search_url <- paste0(base_url, pesticide_id, ".htm")
  
  # Try to read the HTML content of the pesticide page
  search_page <- tryCatch({
    read_html(search_url)
  }, error = function(e) {
    message("Error: Unable to open connection to the website.")
    return(NULL)
  })
  
  # If the page was successfully retrieved, proceed to parse the content
  if (!is.null(search_page)) {
    # Scrape all the tables on the page
    tables <- search_page %>% html_nodes("table") %>% html_table(fill = TRUE)
    
    # Remove empty tables
    tables_cleaned <- tables %>%
      purrr::keep(~ nrow(.x) > 0 & !all(is.na(.x)))  # Keep only non-empty tables
    
    return(tables_cleaned)
  }
  
  # If no data was found, return NULL
  return(NULL)
}



## 03 RUN THE SCRAPER ####

# List of active ingredients you want to scrape data for
pesticides <- c("Folpet", "Azoxystrobin", "Tebuconazole")

# Loop through each pesticide and scrape data
for (pesticide in pesticides) {
  # Scrape data for the current pesticide
  result <- scrape_ppdb(pesticide)
  
  # Debugging message: print the result for inspection
  print(paste("Scraped data for:", pesticide))
  print(result)
  
  # Check if result is a data frame and if it has rows
  if (is.data.frame(result) && nrow(result) > 0) {
    # Assign each result as a separate data frame with the pesticide name
    assign(paste0("data_", pesticide), result)
    print(paste0("Data frame for ", pesticide, " created"))
  } else {
    # Print a message if the result is invalid
    print(paste("No valid data found for", pesticide))
  }
}

# Check the results
ls()  # This will show the names of the data frames created (data_Folpet, data_Azoxystrobin, etc.)





## 03 RUN THE SCRAPER ####

# List of active ingredients you want to scrape data for
pesticides <- c("Folpet", "Azoxystrobin", "Tebuconazole")

# Loop through each pesticide and scrape data
all_results <- lapply(pesticides, scrape_ppdb)

class(all_results)

# Use lapply to combine all tibbles within the list into a single tibble for each pesticide
combined_results <- lapply(all_results, function(x) {
  # Bind all tibbles within each list entry into one tibble
  dplyr::bind_rows(x)
})

# Now each element of combined_results will be a single tibble per pesticide
# Optionally, you can name each element of the list based on the pesticide name for easier identification
names(combined_results) <- pesticides

# Inspect one of the combined tibbles
glimpse(combined_results$Folpet)















