## create summary stats for web scraped pesticide toxicity data 
## J Collins


#*******************************************************************************####
# Packages ####
#*******************************************************************************####

library(dplyr)
library(kableExtra)





#*******************************************************************************####
# LOAD DATA ####
#*******************************************************************************####

combined_dat <- read.csv(file = "Data/agronomy/data/pesticide_data/pesticide_properties_data.csv")

combined_dat$year <- factor(combined_dat$year, levels = c(2022, 2023, 2024))

#*******************************************************************************####
# SUMMARY TABLES ####
#*******************************************************************************####


## ~ PLI ####

#PLI by treamtent and year
pli_sum <- combined_dat %>%
  group_by(year, treatment) %>%
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
  group_by(year, treatment) %>%
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
  group_by(year, treatment) %>%
  summarise(
    n = n(),
    sum = round(x = sum(worm_risk_index, na.rm = TRUE), digits = 4)
  )


## ~ bee risk index ####
bee_risk_index_sum <- combined_dat %>%
  group_by(year, treatment) %>%
  summarise(
    n = n(),
    sum = round(x = sum(bee_risk_index, na.rm = TRUE), digits = 4)
  )

## ~ mammal risk index ####
mammal_risk_index_sum <- combined_dat %>%
  group_by(year, treatment) %>%
  summarise(
    n = n(),
    sum = round(x = sum(mammal_risk_index, na.rm = TRUE), digits = 4)
  )

## ~ bee risk index ####
# collembola_risk_index_sum <- combined_dat %>%
#   group_by(year, treatment) %>%
#   summarise(
#     n = n(),
#     sum = round(x = sum(collembola_risk_index, na.rm = TRUE), digits = 4)
#   )

## ~ birds risk index ####
birds_risk_index_sum <- combined_dat %>%
  group_by(year, treatment) %>%
  summarise(
    n = n(),
    sum = round(x = sum(birds_risk_index, na.rm = TRUE), digits = 4)
  )

## ~ fish risk index ####
fish_risk_index_sum <- combined_dat %>%
  group_by(year, treatment) %>%
  summarise(
    n = n(),
    sum = round(x = sum(fish_risk_index, na.rm = TRUE), digits = 4)
  )





## ~ GUS index ####

#PLI by treamtent and year
GUS_index_sum <- combined_dat %>%
  group_by(year, treatment) %>%
  summarise(
    n = n(),
    sum = round(x = sum(gus_risk_index, na.rm = TRUE), digits = 4)
  )






#*******************************************************************************####
# COMBINE THE SUMMARY TABLES ####



# ~~~~~~~ ####
# combined index tables ####

print(pli_sum)
print(tli_sum)
print(GUS_index_sum)


# Combine all three datasets and remove duplicate 'n' columns
summary_table <- pli_sum %>%
  rename(pli_n = n, pli_sum = sum) %>%
  inner_join(
    tli_sum %>%
      rename(tli_sum = sum), # No need to keep the 'n' column here
    by = c("treatment", "year")
  ) %>%
  inner_join(
    GUS_index_sum %>%
      rename(gus_sum = sum), # No need to keep the 'n' column here
    by = c("treatment", "year")
  ) %>%
  select(year, treatment, pli_n, pli_sum, tli_sum, gus_sum) # Retain only necessary columns

# Print the resulting summary table
print(summary_table)


# Convert to LaTeX
summary_table %>%
  kbl(format = "latex", 
      booktabs = TRUE, 
      caption = "Summary Table of Indices by Treatment and Year") %>%
  kable_styling(latex_options = c("hold_position"))






# ~~~~~~~ ####
# combined eco risk tables ####


# Combine all three datasets and remove duplicate 'n' columns
summary_table <- bee_risk_index_sum %>%
  rename(n = n, bee_sum = sum) %>%
  inner_join(
    birds_risk_index_sum %>%
      rename(birds_sum = sum), # No need to keep the 'n' column here
    by = c("treatment", "year")
  ) %>%
  inner_join(
    fish_risk_index_sum %>%
      rename(fish_sum = sum), # No need to keep the 'n' column here
    by = c("treatment", "year")
  ) %>%
  inner_join(
    mammal_risk_index_sum %>%
      rename(mammal_sum = sum), # No need to keep the 'n' column here
    by = c("treatment", "year")
  ) %>%
  inner_join(
    worm_risk_index_sum %>%
      rename(worm_sum = sum), # No need to keep the 'n' column here
    by = c("treatment", "year")
  ) %>%
  select(year, treatment, n, bee_sum, birds_sum, fish_sum, mammal_sum, worm_sum) # Retain only necessary columns



# Print the resulting summary table
print(summary_table)

# Convert to LaTeX
summary_table %>%
  kbl(format = "latex", 
      booktabs = TRUE, 
      caption = "Summary Table of Indices by Treatment and Year") %>%
  kable_styling(latex_options = c("hold_position"))


























