## web scraping pesticide toxicity data 
## J Collins

setwd(dir = "~/OneDrive - Harper Adams University/")

## 01 Packages 
library(rvest)
library(dplyr)


## 02 SCRAPER FUNCTION ####

scrape_ppdb <- function(active_ingredient) {
  # Create the direct URL for the pesticide's report page
  base_url <- "https://sitem.herts.ac.uk/aeru/ppdb/en/Reports/"
  pesticide_id <- switch(active_ingredient,
                         "Folpet" = "354",
                         "Azoxystrobin" = "54",  
                         "Tebuconazole" = "610",
                         "Glyphosate" = "373",
                         NULL)
  
  if (is.null(pesticide_id)) {
    message("Pesticide ID not found.")
    return(NULL)
  }
  
  search_url <- paste0(base_url, pesticide_id, ".htm")
  
  # Print the search URL to verify it's correct
  print(paste("Searching for:", active_ingredient))
  print(paste("Search URL:", search_url))
  
  # Try to read the HTML content of the pesticide page
  search_page <- tryCatch({
    read_html(search_url)
  }, error = function(e) {
    message("Error: Unable to open connection to the website.")
    return(NULL)
  })
  
  # If the page was successfully retrieved, proceed to parse the content
  if (!is.null(search_page)) {
    # Example: Scrape the title of the page (modify based on actual content structure)
    page_title <- search_page %>% html_node("title") %>% html_text()
    print(paste("Page Title:", page_title))
    
    # Scrape all the tables on the page
    tables <- search_page %>% html_nodes("table") %>% html_table(fill = TRUE)
    
    # Check the structure of the extracted tables
    print(paste("Number of tables found:", length(tables)))
    
    # Remove empty tables
    tables_cleaned <- tables %>%
      purrr::keep(~ nrow(.x) > 0 & !all(is.na(.x)))  # Keep only non-empty tables
    
    print(paste("Number of non-empty tables:", length(tables_cleaned)))
    
    # Return the cleaned list of tables
    return(tables_cleaned)
  }
  
  # If no data was found, return NULL
  return(NULL)
}


## 03 RUN THE SCRAPER ####

# List of active ingredients you want to scrape data for
pesticides <- c("Folpet", "Azoxystrobin", "Tebuconazole")

# Loop through each pesticide and scrape data
all_results <- lapply(pesticides, scrape_ppdb)

# Inspect results for each pesticide
all_results











