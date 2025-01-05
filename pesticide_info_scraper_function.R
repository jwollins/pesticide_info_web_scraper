## web scraping pesticide toxicity data 
## J Collins

setwd(dir = "~/OneDrive - Harper Adams University/")


#*******************************************************************************
# PACKAGES ####


library(rvest)
library(dplyr)
library(purrr) # For list filtering
library(stringr) # replace characters
library(ggplot2) # plotting 
library(tidyr)



#*******************************************************************************
# SCRAPER FUNCTIONS ####



# Function to scrape "ENVIRONMENTAL FATE" section up to "ECOTOXICOLOGY"
scrape_environmental_fate <- function(active_ingredient) {
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
# List treatment pesticides ####


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




#*******************************************************************************
# RUN THE SCRAPER ####

# Initialize an empty list to store data for all pesticides
env_fate_list <- list()

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
  "Fate indices"
)

# # Check the structure of 'Fenpicoxamid' in the list to understand how it is organized
# str(env_fate_list$Diflufenican[2])
# print(env_fate_list$Diflufenican[2])


# # Check if the number of tibbles in 'Fenpicoxamid' matches the length of 'desired_names'
# if (length(env_fate_list$Diflufenican) == length(desired_names)) {
#   names(env_fate_list$Diflufenican) <- desired_names
# }

# # Check the names after applying
# print(names(env_fate_list$Diflufenican))
# 
# 
# print(env_fate_list$Diflufenican[2])
# 
# View(env_fate_list$Diflufenican[2])

# Assign the desired names to each pesticide in the env_fate_list
env_fate_list <- lapply(env_fate_list, function(pesticide_tibble_list) {
  # Check if the list of tibbles matches the expected number of desired names
  if (length(pesticide_tibble_list) == length(desired_names)) {
    names(pesticide_tibble_list) <- desired_names
  }
  return(pesticide_tibble_list)
})

# View(env_fate_list$Diflufenican["Degradation"])
# 
# # Check the names for one pesticide to verify
# print(names(env_fate_list$Diflufenican))




#*******************************************************************************
# COLNAMES IN TIBBLES ####


# print(env_fate_list$Diflufenican[2])
# 
# View(env_fate_list$Diflufenican["Degradation"])

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

# colnames(env_fate_list$Diflufenican[2])
# 
# print(env_fate_list$Diflufenican[2])
# 
# View(env_fate_list$Diflufenican[2])
# 
# colnames(env_fate_list$Diflufenican["Degradation" ])




#*******************************************************************************
# EXTRACTION OF VARIABLE OF INTEREST ####


#*******************************************************************************
## degradation ####


# names(env_fate_list$Diflufenican)
# 
# print(env_fate_list$Diflufenican["Degradation"])

# Extract all 'Degradation' tibbles and bind them together
pesticide_degradation_df <- env_fate_list %>%
  map(~ .x[[2]]) %>%      # Extract the "Degradation" tibble from each pesticide
  discard(is.null) %>%                 # Remove any NULL elements (if "Degradation" doesn't exist)
  bind_rows(.id = "Pesticide")         # Bind them together and add a "Pesticide" column

# # Print the resulting dataframe
# print(pesticide_degradation_df)




# set the column names here 

colnames(pesticide_degradation_df) <- c("Pesticide", "Property_1", "Property_2", 
                                        "Property_3", "Value", "Source_quality_score", 
                                        "Interpretation")


  
  
  
  

#*******************************************************************************
# #*degradation old code problems!!!
# # Initialize an empty list to store the extracted data
# pesticide_degradation <- list()
# 
# # Loop through each pesticide in the env_fate_list
# for (pesticide in names(env_fate_list)) {
#   # Check if the "Degradation" section exists for the pesticide
#   if ("Degradation" %in% names(env_fate_list[[pesticide]])) {
#     # Extract the 4th row and first 4 columns from the "Degradation" tibble
#     degradation_rows <- env_fate_list[[pesticide]][["Degradation"]]
#     
#     # Print the structure of the rows to inspect the data
#     print(str(degradation_rows))
#     
#     # Add the extracted row to the list, along with the pesticide name
#     pesticide_degradation[[pesticide]] <- degradation_rows
#   }
# }


# print(pesticide_degradation$Diflufenican[2])
# 
# # Convert the list of extracted rows into a dataframe
# pesticide_degradation_df <- bind_rows(pesticide_degradation, .id = "Pesticide")
# 
# print(pesticide_degradation$Diflufenican[4])
# 
# colnames(pesticide_degradation_df)
# 
# # Apply the new column names
# colnames(pesticide_degradation_df) <- c("Pesticide", "Property_1", "Property_2", 
#                                         "Property_3", "Value", "Source_quality_score", 
#                                         "Interpretation")


# # Now, use dplyr::select to keep the desired columns
# library(dplyr)



# drop some of the columns with no useful data 
pesticide_degradation_df <- pesticide_degradation_df %>%
  dplyr::select(Pesticide, Property_1, Property_2, Property_3, Value, Source_quality_score, Interpretation)

# # View the updated data frame
# print(pesticide_degradation_df)


#*******************************************************************************
### soil degradation ####



# View the resulting dataframe
# print(pesticide_degradation_df)

# filter to just the soil data 
soil_degradation_df <- filter(.data = pesticide_degradation_df, Property_1 == "Soil degradation (days) (aerobic)")

# print(soil_degradation_df)

# remove any of the "note" rows
soil_degradation_df <- filter(.data = soil_degradation_df, Property_2 != "Note")

# there are some duplicated columns which move the "value" col. Remove duplicates and set to "value"
soil_degradation_df$Value <- if_else(condition = soil_degradation_df$Value == "DT₅₀ (typical)", 
                               true = soil_degradation_df$Source_quality_score, 
                               false = soil_degradation_df$Value)


# Merge Property_1, Property_2, and Property_3 into one column for better info
soil_degradation_df <- soil_degradation_df %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)




#*******************************************************************************
# Pivot the dataframe for merging

# Explicitly use dplyr's select function to remove the unwanted columns
soil_degradation_df <- dplyr::select(soil_degradation_df, -Source_quality_score, -Interpretation)

glimpse(soil_degradation_df)

# Now proceed with reshaping the data
soil_degradation_df_wide <- soil_degradation_df %>%
  pivot_wider(names_from = Property, values_from = Value)

# # View the reshaped dataframe
# glimpse(soil_degradation_df_wide)





#*******************************************************************************
# TREATEMENTS ####

# read application data in 
usage_dat <- read.csv(file = "~/OneDrive - Harper Adams University/Data/LCA/data/processed_data/summary_normalised_LCA_data.csv")

# glimpse(usage_dat)
# 
# # View the reshaped dataframe
# glimpse(soil_degradation_df_wide)



# Assuming Pesticide in soil_degradation_df corresponds to ai_name in usage_dat
combined_dat <- usage_dat %>%
  left_join(soil_degradation_df_wide, by = c("ai_name" = "Pesticide"))

# # Check the structure of the combined dataframe
# glimpse(combined_dat)

# remove "-" character and set to NA
combined_dat <- combined_dat %>%
  mutate(across(where(is.character), ~na_if(., "-")))

# remove any symbols 
combined_dat <- combined_dat %>%
  mutate(across(where(is.character), ~str_replace(., ">1000", "1000")))

# set cols as numeric cols
combined_dat <- combined_dat %>%
  mutate(across(8:13, ~as.numeric(.), .names = "{col}"))



#*******************************************************************************
# PLOTS ####


#*******************************************************************************
## general soil half life plot ####

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

ggsave(filename = "Data/agronomy/plots/ai_soil_degradation_all.png", width = 12, height = 5.5)





#*******************************************************************************
## by treatment ####


b <- ggplot(combined_dat, 
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
  theme(
    axis.text.x = element_text(angle = -45, hjust = 0, size = 6)  # Rotate x-axis labels
  ) +
  facet_wrap(~ treatment)  # Facet by the treatment column


b

# ggsave(filename = "Data/agronomy/plots/ai_degradation_soil.png", width = 10, height = 4)


combined_dat$avg_normalized_rate_kg_ha




## metric approach 

combined_dat <- combined_dat %>%
  mutate(rate_dt50 = avg_normalized_rate_kg_ha * `Soil degradation (days) (aerobic) DT₅₀ (field) DT₅₀ (field)`)

ggplot(combined_dat, 
       aes(x = ai_name, 
           y = rate_dt50, 
           fill = ai_name)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = expression("Pesticide Degradation Load (Rate x " ~ DT[50] ~ ")"),
    x = "Pesticide Active Ingredient",
    y = expression("Degradation Load (Rate x " ~ DT[50] ~ ")"),
    fill = "Active Ingredient"
  ) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 6)) +
  facet_wrap(~ treatment)




combined_dat <- combined_dat %>%
  mutate(normalized_dt50 = `Soil degradation (days) (aerobic) DT₅₀ (field) DT₅₀ (field)` / avg_normalized_rate_kg_ha)

pca <- prcomp(combined_dat[, c("avg_normalized_rate_kg_ha", "Soil degradation (days) (aerobic) DT₅₀ (field) DT₅₀ (field)")], 
              scale. = TRUE)
summary(pca)



















