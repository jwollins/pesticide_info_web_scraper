## filter and sort web scraped pesticide toxicity data 
## J Collins


setwd(rstudioapi::getActiveProject())

getwd()



#_________________________________________________________________________####
# Packages ####

library(dplyr)
library(purrr) # For list filtering
library(stringr) # replace characters
library(ggplot2) # plotting 
library(tidyr)
library(ggpubr)


#_________________________________________________________________________####
# Data ####


pesticide_fate_df <- read.csv(file = "sym_link_pesticide_data/data/pesticide_data/PPD_pesticide_dat.csv")


env_degration_df <- read.csv(file = "sym_link_pesticide_data/data/pesticide_data/PPD_env_degradation_dat.csv")



#________________________________________________________________________####
# EXRACT DATA FROM METADATA ####


# ~ BCF bio-accumulation ####

# Filter the desired rows
bcf_l_kg <- pesticide_fate_df %>%
  filter(Property_1 == "Bio-concentration factor",
         Property_2 == "Bio-concentration factor")




# ~ sci_grow mobility  ####

# Filter the desired rows
sci_grow_mobility <- pesticide_fate_df %>%
  filter(Property_1 == "SCI-GROW groundwater index (μg l⁻¹) for a 1 kg ha⁻¹ or 1 l ha⁻¹ application rate",
         Property_2 == "SCI-GROW groundwater index (μg l⁻¹) for a 1 kg ha⁻¹ or 1 l ha⁻¹ application rate")




# ~ GUS leaching ####


# Filter the desired rows
GUS_leaching_potential_index <- pesticide_fate_df %>%
  filter(Property_1 == "GUS leaching potential index")

# View the result
print(GUS_leaching_potential_index)




# ~ Birds ####

# Filter the desired rows
birds_ld50 <- pesticide_fate_df %>%
  filter(Property_1 == "Birds - Acute LD₅₀ (mg kg⁻¹)")

# View the result
print(birds_ld50)



# ~ Aquatic plants ####


aq_plants_ec50 <- pesticide_fate_df %>%
  filter(Property_1 == "Aquatic plants - Acute 7 day EC₅₀, biomass (mg l⁻¹)")



# ~ Mammals  ####

# Filter the desired rows
mammals_ld50 <- pesticide_fate_df %>%
  filter(Property_1 == "Mammals - Acute oral LD₅₀ (mg kg⁻¹)")

mammals_ld50 <- mammals_ld50 %>% distinct()




# ~ Earthworms ####

worm_lc50 <- pesticide_fate_df %>%
  filter(Property_1 == "Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹)")

# View the result
print(worm_lc50)



worm_noec <- pesticide_fate_df %>%
  filter(Property_1 == "Earthworms - Chronic NOEC, reproduction (mg kg⁻¹)")

# View the result
print(worm_noec)







# ~ Algae ####

algae_ec50 <- pesticide_fate_df %>%
  filter(Property_1 == "Algae - Acute 72 hour EC₅₀, growth (mg l⁻¹)")








# ~ Bees ####

bees_ld50 <- pesticide_fate_df %>%
  filter(Property_1 == "Honeybees (Apis spp.)", Property_3 == "Contact acute LD₅₀ (worst case from 24, 48 and 72 hour values - μg bee⁻¹)")

# View the result
print(bees_ld50)




# ~ fish ####

fish_lc50 <- pesticide_fate_df %>%
  filter(Property_1 == "Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹)", 
         Property_3 == "Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹)")

# View the result
print(fish_lc50)


unique(pesticide_fate_df$Property_1)


fish_noec <- pesticide_fate_df %>%
  filter(Property_1 == "Temperate Freshwater Fish - Chronic 21 day NOEC (mg l⁻¹)", 
         Property_3 == "Temperate Freshwater Fish - Chronic 21 day NOEC (mg l⁻¹)")







# ~ Daphnia ####


daphnia_ec50 <- pesticide_fate_df %>%
  filter(Property_1 == "Temperate Freshwater Aquatic invertebrates - Acute 48 hour EC₅₀ (mg l⁻¹)", 
         Property_3 == "Temperate Freshwater Aquatic invertebrates - Acute 48 hour EC₅₀ (mg l⁻¹)")


daphnia_noec <- pesticide_fate_df %>%
  filter(Property_1 == "Temperate Freshwater Aquatic invertebrates - Chronic 21 day NOEC (mg l⁻¹)", 
         Property_3 == "Temperate Freshwater Aquatic invertebrates - Chronic 21 day NOEC (mg l⁻¹)")






# __________ ####
# ~ Degradation ####


# ~ Soil degradation (days) (typical) ####

soil_dt50 <- env_degration_df %>%
  filter(Property_1 == "Soil degradation (days) (aerobic)",
         Property_3 == "DT₅₀ (typical)")

# ~ Soil degradation (days) (field) ####

soil_dt50_field <- env_degration_df %>%
  filter(Property_1 == "Soil degradation (days) (aerobic)", 
         Property_3 == "DT₅₀ (field)")


# ~ Soil degradation (dt50) (lab) ####

soil_dt50_lab <- env_degration_df %>%
  filter(Property_1 == "Soil degradation (days) (aerobic)", 
         Property_3 == "DT₅₀ (lab at 20 °C)")








# __________ ####
# ~ Human health  ####


# ~ Soil degradation (dt50) (lab) ####

clp_class <- pesticide_fate_df %>%
  filter(Property_1 == "CLP classification 2013", 
         Property_2 == "CLP classification 2013")


glimpse(clp_class)


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









#________________________________________________________________________####
# PIVOT DFS ####

# Pivot the dataframe for merging




#___________####

# ENV FATE PIVOT ####


# ~ Soil Degradation typical ####


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



# ~ Soil Degradation field ####


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




# ~ Soil Degradation lab ####


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






# ~ GUS leaching ####

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






# ~ bio accumulation ####

print(bcf_l_kg)

# Merge Property_1, Property_2, and Property_3 into one column for better info
bcf_l_kg <- bcf_l_kg %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(bcf_l_kg)

# Remove unwanted columns
bcf_l_kg <- bcf_l_kg %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

# Now proceed with reshaping the data
bcf_l_kg <- bcf_l_kg %>%
  pivot_wider(names_from = Property, values_from = Value_2)






# ~ sci gro mobility ####

print(sci_grow_mobility)

# Merge Property_1, Property_2, and Property_3 into one column for better info
sci_grow_mobility <- sci_grow_mobility %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(sci_grow_mobility)

# Remove unwanted columns
sci_grow_mobility <- sci_grow_mobility %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

# Now proceed with reshaping the data
sci_grow_mobility <- sci_grow_mobility %>%
  pivot_wider(names_from = Property, values_from = Value_2)




library(stringr)

# Remove any non-printable characters
sci_grow_mobility$sci_grow_cleaned <- str_replace_all(sci_grow_mobility$Value_3, "[[:space:]]*X[[:space:]]*", "e")

# Trim spaces
sci_grow_mobility$sci_grow_cleaned <- str_trim(sci_grow_mobility$sci_grow_cleaned)

sci_grow_mobility$sci_grow_cleaned

sci_grow_mobility$sci_grow_cleaned[is.na(as.numeric(sci_grow_mobility$sci_grow_cleaned))]


sci_grow_mobility$sci_grow_cleaned <- ifelse(test = sci_grow_mobility$sci_grow_cleaned == "Cannot be calculated", yes = NA, no = sci_grow_mobility$sci_grow_cleaned)

print(sci_grow_mobility$sci_grow_cleaned)



# fix the notation issue 

# Fix incorrect scientific notation by replacing "e10-" with "e-"
sci_grow_mobility$sci_grow_cleaned <- gsub("e10-", "e-", sci_grow_mobility$sci_grow_cleaned)

# Convert to numeric
sci_grow_mobility$sci_grow_numeric <- suppressWarnings(as.numeric(sci_grow_mobility$sci_grow_cleaned))

# Check if conversion was successful
summary(sci_grow_mobility$sci_grow_numeric)



# Convert to numeric
sci_grow_mobility$sci_grow_numeric <- suppressWarnings(as.numeric(sci_grow_mobility$sci_grow_cleaned))

head(sci_grow_mobility$sci_grow_numeric, 20)



# sci_grow_mobility$Value_3_numeric <- as.numeric(gsub(" X ", "e", sci_grow_mobility$Value_3))
# 
# print(sci_grow_mobility$Value_3_numeric)
# 
# unique(sci_grow_mobility$Value_3_numeric)
# 
# sci_grow_mobility$Value_3_cleaned <- gsub("\\s*X\\s*", "e", sci_grow_mobility$Value_3) # Normalize spacing around "X"
# sci_grow_mobility$Value_3_numeric <- suppressWarnings(as.numeric(sci_grow_mobility$Value_3_cleaned)) # Convert to numeric, suppress warnings

# sci_grow_mobility[is.na(sci_grow_mobility$Value_3_numeric), "Value_3"]
# 
# 
# charToRaw(sci_grow_mobility$Value_3[1])
# 
# library(stringr)
# 
# # Normalize spaces and non-standard characters
# sci_grow_mobility$Value_3_cleaned <- str_replace_all(sci_grow_mobility$Value_3, "[[:space:]]*X[[:space:]]*", "e")
# 
# # Convert to numeric
# sci_grow_mobility$Value_3_numeric <- suppressWarnings(as.numeric(sci_grow_mobility$Value_3_cleaned))
# 
# 
# sci_grow_mobility[is.na(sci_grow_mobility$Value_3_numeric), "Value_3"]
# 
# options(scipen = 999)
# print(sci_grow_mobility$Value_3_numeric)







#____________####

# ECOTOX PIVOT ####




# ~ Algae EC50 ####

# Merge Property_1, Property_2, and Property_3 into one column for better info
algae_ec50 <- algae_ec50 %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(algae_ec50)

# Remove unwanted columns
algae_ec50 <- algae_ec50 %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

colnames(algae_ec50)

algae_ec50 <- algae_ec50 %>%
  distinct(Pesticide, Property, .keep_all = TRUE) %>%
  pivot_wider(names_from = Property, values_from = Value_1)






# ~ Aquatic plants EC50 ####

# Merge Property_1, Property_2, and Property_3 into one column for better info
aq_plants_ec50 <- aq_plants_ec50 %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(aq_plants_ec50)

# Remove unwanted columns
aq_plants_ec50 <- aq_plants_ec50 %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

colnames(aq_plants_ec50)

aq_plants_ec50 <- aq_plants_ec50 %>%
  distinct(Pesticide, Property, .keep_all = TRUE) %>%
  pivot_wider(names_from = Property, values_from = Value_1)







# ~ Mammals LD50 ####

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




# ~ Earthworm LC50 ####


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



# ~ Earthworm NOEC ####


# Merge Property_1, Property_2, and Property_3 into one column for better info
worm_noec <- worm_noec %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(worm_noec)

# Remove unwanted columns
worm_noec <- worm_noec %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

# Now proceed with reshaping the data
worm_noec <- worm_noec %>%
  pivot_wider(names_from = Property, values_from = Value_1)







# ~ Daphnia ec50 ####


# Merge Property_1, Property_2, and Property_3 into one column for better info
daphnia_ec50 <- daphnia_ec50 %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(daphnia_ec50)

# Remove unwanted columns
daphnia_ec50 <- daphnia_ec50 %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

# Now proceed with reshaping the data
daphnia_ec50 <- daphnia_ec50 %>%
  pivot_wider(names_from = Property, values_from = Value_1)






# ~ Daphnia NOEC ####

# Merge Property_1, Property_2, and Property_3 into one column for better info
daphnia_noec <- daphnia_noec %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(daphnia_noec)

# Remove unwanted columns
daphnia_noec <- daphnia_noec %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

# Now proceed with reshaping the data
daphnia_noec <- daphnia_noec %>%
  pivot_wider(names_from = Property, values_from = Value_1)











# ~ Bees LD50 ####

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



# ~ Birds LD50 ####

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



# ~ Fish LC50 ####

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




# ~ Fish NOEC ####

glimpse(fish_noec)

# Merge Property_1, Property_2, and Property_3 into one column for better info
fish_noec <- fish_noec %>%
  unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(fish_noec)

# Remove unwanted columns
fish_noec <- fish_noec %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

glimpse(fish_noec)

# Now proceed with reshaping the data
fish_noec <- fish_noec %>%
  pivot_wider(names_from = Property, values_from = Value_1)






#_____________####
# human health ####


# ~ CLP ####


glimpse(clp_class)

# # Merge Property_1, Property_2, and Property_3 into one column for better info
# clp_class <- clp_class %>%
#   unite("Property", Property_1, Property_2, Property_3, sep = " ", remove = TRUE)

colnames(clp_class)

# Remove unwanted columns
clp_class <- clp_class %>%
  select(-Interpretation, -`NA.`, -`NA..1`, -`NA..2`, -`NA..3`,
         -`NA..4`, -`NA..5`, -`NA..6`, -`NA..7`, 
         -`NA..8`, -`NA..9`, -`NA..10`, -`NA..11`, -`NA..12`,
         -`NA..13`, -`NA..14`, -`NA..15`, -`NA..16`, -`NA..17`, -`NA..18`,
         -`NA..19`, -`NA..20`, -`NA..21`)

glimpse(clp_class)

# Now proceed with reshaping the data
clp_class <- clp_class %>%
  pivot_wider(names_from = Property_1, values_from = Property_3)



library(stringr)

# Modify the dataset
clp_class <- clp_class %>%
  mutate(
    H_codes = str_extract_all(`CLP classification 2013`, "H\\d{3}"), # Extract all H*** codes
    H_codes = sapply(H_codes, function(x) paste(x, collapse = ", ")), # Convert list to string
    `CLP classification 2013` = str_replace_all(`CLP classification 2013`, "H\\d{3},? ?", "") # Remove H*** codes
  )

# View result
glimpse(clp_class)













#________________________________________________________________________####
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

glimpse(clp_class)

# Example: Selecting specific columns to join
combined_dat <- usage_dat %>%
  
  left_join(soil_dt50 %>% select(Pesticide, `Soil degradation (days) (aerobic) Soil degradation (days) (aerobic) DT₅₀ (typical)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(soil_dt50_field %>% select(Pesticide, `Soil degradation (days) (aerobic) DT₅₀ (field) DT₅₀ (field)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(soil_dt50_lab %>% select(Pesticide, `Soil degradation (days) (aerobic) DT₅₀ (lab at 20 °C) DT₅₀ (lab at 20 °C)`), 
            by = c("ai_name" = "Pesticide")) %>%
  
  left_join(sci_grow_mobility %>% select(Pesticide, sci_grow_numeric), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(bcf_l_kg %>% select(Pesticide, `Bio-concentration factor Bio-concentration factor BCF (l kg⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(GUS_leaching_potential_index %>% 
              select(Pesticide,`GUS leaching potential index GUS leaching potential index GUS leaching potential index`), 
            by = c("ai_name" = "Pesticide")) %>%
  
  left_join(mammals_ld50 %>% select(Pesticide, `Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  
  left_join(worm_lc50 %>% select(Pesticide,  `Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(worm_noec %>% select(Pesticide,  `Earthworms - Chronic NOEC, reproduction (mg kg⁻¹) Earthworms - Chronic NOEC, reproduction (mg kg⁻¹) Earthworms - Chronic NOEC, reproduction (mg kg⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%

  left_join(daphnia_ec50 %>% select(Pesticide, `Temperate Freshwater Aquatic invertebrates - Acute 48 hour EC₅₀ (mg l⁻¹) Temperate Freshwater Aquatic invertebrates - Acute 48 hour EC₅₀ (mg l⁻¹) Temperate Freshwater Aquatic invertebrates - Acute 48 hour EC₅₀ (mg l⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(daphnia_noec %>% select(Pesticide, `Temperate Freshwater Aquatic invertebrates - Chronic 21 day NOEC (mg l⁻¹) Temperate Freshwater Aquatic invertebrates - Chronic 21 day NOEC (mg l⁻¹) Temperate Freshwater Aquatic invertebrates - Chronic 21 day NOEC (mg l⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  
  left_join(fish_lc50 %>% 
              select(Pesticide, `Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹) Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹) Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  left_join(fish_noec %>% 
              select(Pesticide, `Temperate Freshwater Fish - Chronic 21 day NOEC (mg l⁻¹) Temperate Freshwater Fish - Chronic 21 day NOEC (mg l⁻¹) Temperate Freshwater Fish - Chronic 21 day NOEC (mg l⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  
  left_join(bees_ld50 %>% 
              select(Pesticide, `Honeybees (Apis spp.) Honeybees (Apis spp.) Contact acute LD₅₀ (worst case from 24, 48 and 72 hour values - μg bee⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  
  left_join(birds_ld50 %>% 
              select(Pesticide, `Birds - Acute LD₅₀ (mg kg⁻¹) Birds - Acute LD₅₀ (mg kg⁻¹) Birds - Acute LD₅₀ (mg kg⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%

  left_join(algae_ec50 %>% 
              select(Pesticide, `Algae - Acute 72 hour EC₅₀, growth (mg l⁻¹) Algae - Acute 72 hour EC₅₀, growth (mg l⁻¹) Algae - Acute 72 hour EC₅₀, growth (mg l⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>%
  
  left_join(aq_plants_ec50 %>% 
              select(Pesticide, `Aquatic plants - Acute 7 day EC₅₀, biomass (mg l⁻¹) Aquatic plants - Acute 7 day EC₅₀, biomass (mg l⁻¹) Aquatic plants - Acute 7 day EC₅₀, biomass (mg l⁻¹)`), 
            by = c("ai_name" = "Pesticide")) %>% 

left_join(clp_class %>% 
            select(Pesticide, H_codes), 
          by = c("ai_name" = "Pesticide"))


glimpse(combined_dat$H_codes)



### ~~ prep joined data ####

# remove "-" character and set to NA
combined_dat <- combined_dat %>%
  mutate(across(where(is.character), ~na_if(., "-")))

# Remove ">" symbols from character columns
combined_dat <- combined_dat %>%
  mutate(across(where(is.character), ~str_replace_all(., ">", "")))

# # set cols as numeric cols
# combined_dat <- combined_dat %>%
#   mutate(across(5:ncol(combined_dat), ~as.numeric(.), .names = "{col}"))







#________________________________________________________________________####
# Danish PLI ####



#________####
# Env fate indicator ####

names(combined_dat)

library(janitor)

combined_dat <- clean_names(combined_dat)

names(combined_dat)


# ~ set reference values ####

soil_dt50_ref <- 5500 # diquat (5,500 d)

soil_mobility_ref <- 5.13 # flutriafol (5.13)

bioaccumilation_ref <- 5100  # pendimethalin (5,100 l kg−ź)


# ~ set data values ####

field_dt50_val <- combined_dat$soil_degradation_days_aerobic_dt50_field_dt50_field

SCI_GROW_val <- combined_dat$sci_grow_numeric

BCF_val <- combined_dat$bio_concentration_factor_bio_concentration_factor_bcf_l_kg_1


glimpse(combined_dat$avg_normalized_rate_kg_ha)

# Normalize each factor by dividing by its max value
env_fate_indic <- 
  combined_dat %>%
  mutate(
    field_dt50_val = as.numeric(field_dt50_val),
    SCI_GROW_val = as.numeric(SCI_GROW_val),
    BCF_val = as.numeric(BCF_val),
    
    DT50_norm = field_dt50_val / max(field_dt50_val, na.rm = TRUE), 
    SCI_GROW_norm = SCI_GROW_val / max(SCI_GROW_val, na.rm = TRUE),
    BCF_norm = BCF_val / max(BCF_val, na.rm = TRUE),
    
    # Calculate the Environmental Fate Indicator
    Env_Fate_Indicator = DT50_norm + SCI_GROW_norm + BCF_norm
  ) %>%
  select(year, treatment, crop, ai_name, category, avg_normalized_rate_kg_ha,
          DT50_norm, SCI_GROW_norm,
         BCF_norm, Env_Fate_Indicator)

# View the updated dataset
print(env_fate_indic)





# ~ save data ####

write.csv(x = env_fate_indic, file = "sym_link_pesticide_data/data/pesticide_data/danish_pli_env_fate_indicator.csv")






#________####
# Ecotox indicator ####



# ~ set data values ####

fish_lc50_val <- combined_dat$temperate_freshwater_fish_acute_96_hour_lc50_mg_l_1_temperate_freshwater_fish_acute_96_hour_lc50_mg_l_1_temperate_freshwater_fish_acute_96_hour_lc50_mg_l_1
# fish_noec_val < combined_dat$temperate_freshwater_fish_chronic_21_day_noec_mg_l_1_temperate_freshwater_fish_chronic_21_day_noec_mg_l_1_temperate_freshwater_fish_chronic_21_day_noec_mg_l_1

dapnia_ec50_val <- combined_dat$temperate_freshwater_aquatic_invertebrates_acute_48_hour_ec50_mg_l_1_temperate_freshwater_aquatic_invertebrates_acute_48_hour_ec50_mg_l_1_temperate_freshwater_aquatic_invertebrates_acute_48_hour_ec50_mg_l_1
# daphnia_noec_val <- combined_dat$temperate_freshwater_aquatic_invertebrates_chronic_21_day_noec_mg_l_1_temperate_freshwater_aquatic_invertebrates_chronic_21_day_noec_mg_l_1_temperate_freshwater_aquatic_invertebrates_chronic_21_day_noec_mg_l_1

worm_lc50_val <- combined_dat$earthworms_acute_14_day_lc50_mg_kg_1_earthworms_acute_14_day_lc50_mg_kg_1_earthworms_acute_14_day_lc50_mg_kg_1
# worm_noec_val <- combined_dat$earthworms_chronic_noec_reproduction_mg_kg_1_earthworms_chronic_noec_reproduction_mg_kg_1_earthworms_chronic_noec_reproduction_mg_kg_1

birds_ld50_val <- combined_dat$birds_acute_ld50_mg_kg_1_birds_acute_ld50_mg_kg_1_birds_acute_ld50_mg_kg_1

mammals_ld50_val <- combined_dat$mammals_acute_oral_ld50_mg_kg_1_mammals_acute_oral_ld50_mg_kg_1_mammals_acute_oral_ld50_mg_kg_1

algae_ec50_val <- combined_dat$algae_acute_72_hour_ec50_growth_mg_l_1_algae_acute_72_hour_ec50_growth_mg_l_1_algae_acute_72_hour_ec50_growth_mg_l_1

aq_plants_ec50_val <- combined_dat$aquatic_plants_acute_7_day_ec50_biomass_mg_l_1_aquatic_plants_acute_7_day_ec50_biomass_mg_l_1_aquatic_plants_acute_7_day_ec50_biomass_mg_l_1

bees_ld50_val <- combined_dat$honeybees_apis_spp_honeybees_apis_spp_contact_acute_ld50_worst_case_from_24_48_and_72_hour_values_mg_bee_1







# ~ caluclate the sub indicator ####


# Calculate Ecotoxicology Sub-Indicator
ecotox_indic <- 
  combined_dat %>%
  mutate(
    # Convert all relevant variables to numeric to avoid type errors
    fish_lc50_val = as.numeric(fish_lc50_val),
    dapnia_ec50_val = as.numeric(dapnia_ec50_val),
    worm_lc50_val = as.numeric(worm_lc50_val),
    birds_ld50_val = as.numeric(birds_ld50_val),
    mammals_ld50_val = as.numeric(mammals_ld50_val),
    algae_ec50_val = as.numeric(algae_ec50_val),
    aq_plants_ec50_val = as.numeric(aq_plants_ec50_val),
    bees_ld50_val = as.numeric(bees_ld50_val),
    
    # Normalize each toxicity factor using the correct formula (inverted ratio)
    fish_lc50_norm = 1 / (fish_lc50_val / min(fish_lc50_val, na.rm = TRUE)), 
    dapnia_ec50_norm = 1 / (dapnia_ec50_val / min(dapnia_ec50_val, na.rm = TRUE)),
    worm_lc50_norm = 1 / (worm_lc50_val / min(worm_lc50_val, na.rm = TRUE)),
    birds_ld50_norm = 1 / (birds_ld50_val / min(birds_ld50_val, na.rm = TRUE)),
    mammals_ld50_norm = 1 / (mammals_ld50_val / min(mammals_ld50_val, na.rm = TRUE)),
    algae_ec50_norm = 1 / (algae_ec50_val / min(algae_ec50_val, na.rm = TRUE)),
    aq_plants_ec50_norm = 1 / (aq_plants_ec50_val / min(aq_plants_ec50_val, na.rm = TRUE)),
    bees_ld50_norm = 1 / (bees_ld50_val / min(bees_ld50_val, na.rm = TRUE)),
    
    # Apply weighting factors (adjust based on literature)
    fish_weighted = fish_lc50_norm * 2,  # Example: Fish impact weight
    dapnia_weighted = dapnia_ec50_norm * 1.5,  # Daphnia impact
    worm_weighted = worm_lc50_norm * 1.2,  # Earthworm impact
    birds_weighted = birds_ld50_norm * 2,  # Bird impact
    mammals_weighted = mammals_ld50_norm * 2,  # Mammal impact
    algae_weighted = algae_ec50_norm * 1.2,  # Algae impact
    aq_plants_weighted = aq_plants_ec50_norm * 1.3,  # Aquatic plant impact
    bees_weighted = bees_ld50_norm * 3,  # Bees impact (higher importance)
    
    # Compute the Ecotoxicology Load
    EcoTox_Indicator = fish_weighted + dapnia_weighted + worm_weighted + 
      birds_weighted + mammals_weighted + algae_weighted + 
      aq_plants_weighted + bees_weighted
  ) %>%
  select(year, treatment, crop, ai_name, category, avg_normalized_rate_kg_ha,
         fish_weighted, dapnia_weighted, worm_weighted,
         birds_weighted, mammals_weighted, algae_weighted,
         aq_plants_weighted, bees_weighted, EcoTox_Indicator)

# View results
print(ecotox_indic)





# SAVE DATA ####

write.csv(x = ecotox_indic, file = "sym_link_pesticide_data/data/pesticide_data/danish_pli_ecotox_indicator.csv")








#___________####
# Human health ####


# Define human health scores for H-phrases
h_phrase_scores <- list(
  "H302" = 10,  # Harmful if swallowed
  "H315" = 10,  # Skin irritation
  "H319" = 10,  # Serious eye irritation
  "H331" = 50,  # Toxic if inhaled
  "H340" = 100, # May cause genetic defects
  "H350" = 100, # May cause cancer
  "H300" = 100  # Fatal if swallowed
)

# Function to compute human health score for each pesticide
calculate_human_health <- function(h_phrases) {
  # Split the H-phrase string into individual codes
  h_list <- str_split(h_phrases, ",\\s*")[[1]]
  
  # Get the corresponding scores, default to 0 if not in the list
  scores <- sapply(h_list, function(h) h_phrase_scores[[h]] %||% 0)
  
  # Sum the scores
  total_score <- sum(scores, na.rm = TRUE)
  
  # Convert to "pesticide loading points" (divide by 300)
  return(total_score / 300)
}



combined_dat$h_codes

# Apply the function to the dataset
human_health_indic <- combined_dat %>%
  mutate(
    PL_HH = sapply(combined_dat$h_codes, calculate_human_health)
  ) %>%
  select(year, treatment, crop, ai_name, 
         category, avg_normalized_rate_kg_ha, h_codes, PL_HH)

# View results
print(human_health_indic)





# ~ save df ####

write.csv(x = human_health_indic, 
          file = "sym_link_pesticide_data/data/pesticide_data/danish_pli_human_health_indicator.csv")





#________________________________________________________________________####
# Total PLI ####


# ~ Join PLI DF's  ####

names(ecotox_indic)
names(human_health_indic)

total_pli_df <- cbind(env_fate_indic, 
                      ecotox_indic[,7:ncol(ecotox_indic)], 
                      human_health_indic[,7:ncol(human_health_indic)])

names(total_pli_df)

# total PLI
total_pli_df$total_pli <- total_pli_df$Env_Fate_Indicator + total_pli_df$EcoTox_Indicator + total_pli_df$PL_HH


# ~ add rates ####

total_pli_df$env_fate_indic_x_rate <- total_pli_df$Env_Fate_Indicator * total_pli_df$avg_normalized_rate_kg_ha

total_pli_df$ecotox_indic_x_rate <- total_pli_df$EcoTox_Indicator * total_pli_df$avg_normalized_rate_kg_ha

total_pli_df$human_health_indic_x_rate <- total_pli_df$PL_HH * total_pli_df$avg_normalized_rate_kg_ha

total_pli_df$total_pli_x_rate <- total_pli_df$total_pli * total_pli_df$avg_normalized_rate_kg_ha


# ~ save the df ####

write.csv(x = total_pli_df, 
          file = "sym_link_pesticide_data/data/pesticide_data/danish_pli_total_pli_data.csv")







#________________________________________________________________________####
# Proportions ####


glimpse(total_pli_df)


library(dplyr)
library(ggplot2)

# Summarize total usage by category and treatment
category_summary <- total_pli_df %>%
  group_by(treatment, category) %>%  # Group by both treatment and category
  summarise(total_usage = sum(avg_normalized_rate_kg_ha, na.rm = TRUE), .groups = "drop") %>%
  group_by(treatment) %>%  # Group again by treatment to calculate proportions correctly
  mutate(percentage = (total_usage / sum(total_usage)) * 100) %>%
  ungroup()

# View summary
print(category_summary)









#________________________________________________________________________####
# CALCULATE LOAD INDEXS ####


# # add equation parameters
# application_rate <- combined_dat$sum_normalized_rate_kg_ha
# DT50 <- combined_dat$`Soil degradation (days) (aerobic) DT₅₀ (field) DT₅₀ (field)`
# # LD50 <- combined_dat$`Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹)`
# 
# 
# 
# 
# # ~ PLI & TLI ####
# 
# ## ~~ mammals ####
# LD50 <- combined_dat$`Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹)`
# 
# combined_dat$PLI_mammal <- application_rate * (DT50 / LD50)
# 
# combined_dat$TLI_mammal <- application_rate * (1 / LD50)
# 
# 
# 
# ## ~~ worms ####
# LD50 <- combined_dat$`Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹)`
# 
# combined_dat$PLI_earthworms <- application_rate * (DT50 / LD50)
# 
# combined_dat$TLI_earthworms <- application_rate * (1 / LD50)
# 
# 
# 
# 
# 
# ## ~~ bees ####
# LD50 <- combined_dat$`Honeybees (Apis spp.) Honeybees (Apis spp.) Contact acute LD₅₀ (worst case from 24, 48 and 72 hour values - μg bee⁻¹)`
# 
# combined_dat$PLI_bees <- application_rate * (DT50 / LD50)
# 
# combined_dat$TLI_bees <- application_rate * (1 / LD50)
# 
# 
# 
# 
# ## ~~ collembola ####
# LD50 <- combined_dat$`Collembola Chronic NOEC (mg kg⁻¹) Chronic NOEC (mg kg⁻¹)`
# 
# combined_dat$PLI_collembola <- application_rate * (DT50 / LD50)
# 
# combined_dat$TLI_collembola <- application_rate * (1 / LD50)
# 
# 
# 
# ## ~~ birds ####
# LD50 <- combined_dat$`Birds - Acute LD₅₀ (mg kg⁻¹) Birds - Acute LD₅₀ (mg kg⁻¹) Birds - Acute LD₅₀ (mg kg⁻¹)`
# 
# combined_dat$PLI_birds <- application_rate * (DT50 / LD50)
# 
# combined_dat$TLI_birds <- application_rate * (1 / LD50)
# 
# 
# 
# 
# ## ~~ fish ####
# LD50 <- combined_dat$`Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹) Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹) Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹)`
# 
# combined_dat$PLI_fish <- application_rate * (DT50 / LD50)
# 
# combined_dat$TLI_fish <- application_rate * (1 / LD50)
# 
# 
# 
# 
# 
# 
# 
# ## ~ GUS ####
# 
# combined_dat$gus_risk_index <- application_rate * combined_dat$`GUS leaching potential index GUS leaching potential index GUS leaching potential index`
# 
# 
# 
# # ## ~ worm risk index ####
# # 
# # combined_dat$worm_risk_index <- application_rate / combined_dat$`Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹) Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹)`
# # 
# # ## ~ bee risk index ####
# # 
# # combined_dat$bee_risk_index <- application_rate / combined_dat$`Honeybees (Apis spp.) Honeybees (Apis spp.) Contact acute LD₅₀ (worst case from 24, 48 and 72 hour values - μg bee⁻¹)`
# # 
# # ## ~ collembola risk index ####
# # 
# # combined_dat$collembola_risk_index <- application_rate / combined_dat$`Collembola Chronic NOEC (mg kg⁻¹) Chronic NOEC (mg kg⁻¹)`
# # 
# # ## ~ mammal risk index ####
# # 
# # combined_dat$mammal_risk_index <- application_rate / combined_dat$`Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹) Mammals - Acute oral LD₅₀ (mg kg⁻¹)`
# # 
# # ## ~ birds risk index ####
# # combined_dat$birds_risk_index <- application_rate / combined_dat$`Birds - Acute LD₅₀ (mg kg⁻¹) Birds - Acute LD₅₀ (mg kg⁻¹) Birds - Acute LD₅₀ (mg kg⁻¹)`
# # 
# # ## ~ fish risk index ####
# # combined_dat$fish_risk_index <- application_rate / combined_dat$`Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹) Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹) Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹)`
# # 
# 
# 
# 

