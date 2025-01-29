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



# Format the second column (year) with scientific notation
summary_table <- summary_table %>%
  mutate(
    tli_sum = scientific(tli_sum, digits = 2)
  )


# Convert to LaTeX
summary_table %>%
  kbl(format = "latex", 
      label = "this is the label",
      booktabs = TRUE, 
      caption = "Summary Table of Indices by Treatment and Year", digits = 2) %>%
  kable_styling(latex_options = c("hold_position"))




# Assuming summary_table is your data frame
summary_by_treatment <- summary_table %>%
  group_by(treatment) %>%
  summarise(
    total_n = sum(pli_n),                  # Total of 'n' per treatment
    pli_sum = sum(pli_sum, na.rm = TRUE), # Average 'pli_sum' per treatment
    tli_sum = sum(as.numeric(tli_sum), na.rm = TRUE), # Average 'tli_sum' per treatment (convert to numeric if needed)
    gus_sum = sum(gus_sum, na.rm = TRUE)   # Average 'gus_sum' per treatment
  )

# Print the summary table
print(summary_by_treatment)





# effect size calculation 

# Pivot longer for analysis
effect_sizes <- summary_table %>%
  pivot_longer(
    cols = c(pli_sum, tli_sum, gus_sum), # Specify only numeric columns to pivot
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable, treatment) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(variable) %>%
  summarise(
    cohen_d = {
      M1 <- mean[treatment == "Conservation"]
      M2 <- mean[treatment == "Conventional"]
      SD1 <- sd[treatment == "Conservation"]
      SD2 <- sd[treatment == "Conventional"]
      n1 <- n[treatment == "Conservation"]
      n2 <- n[treatment == "Conventional"]
      SD_pooled <- sqrt(((n1 - 1) * SD1^2 + (n2 - 1) * SD2^2) / (n1 + n2 - 2))
      (M1 - M2) / SD_pooled
    }
  )

print(effect_sizes)








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



# Assuming summary_table is your data frame
summary_by_treatment <- summary_table %>%
  group_by(treatment) %>%
  summarise(
    total_n = sum(n),                  # Total of 'n' per treatment
    bee_sum = sum(round(x = bee_sum, digits = 3), na.rm = TRUE), 
    birds_sum = sum(round(x = birds_sum, digits = 4), na.rm = TRUE), 
    fish_sum = sum(round(fish_sum, digits = 3), na.rm = TRUE),
    mammal_sum = sum(round(x = mammal_sum, digits = 3), na.rm = TRUE),
    worm_sum = sum(round(x = worm_sum, digits = 3), na.rm = TRUE)
  )

# Print the summary table
print(summary_by_treatment)



# effect size calculation 

# Pivot longer for analysis
effect_sizes <- summary_table %>%
  pivot_longer(
    cols = c(bee_sum, birds_sum, fish_sum, mammal_sum, worm_sum), # Specify only numeric columns to pivot
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable, treatment) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(variable) %>%
  summarise(
    cohen_d = {
      M1 <- mean[treatment == "Conservation"]
      M2 <- mean[treatment == "Conventional"]
      SD1 <- sd[treatment == "Conservation"]
      SD2 <- sd[treatment == "Conventional"]
      n1 <- n[treatment == "Conservation"]
      n2 <- n[treatment == "Conventional"]
      SD_pooled <- sqrt(((n1 - 1) * SD1^2 + (n2 - 1) * SD2^2) / (n1 + n2 - 2))
      (M1 - M2) / SD_pooled
    }
  )

print(effect_sizes)





library(scales)

# Format numeric columns with scientific notation
summary_table <- summary_table %>%
  mutate(
    bee_sum = scientific(bee_sum, digits = 2),
    birds_sum = scientific(birds_sum, digits = 2),
    fish_sum = scientific(fish_sum, digits = 2),
    mammal_sum = scientific(mammal_sum, digits = 2),
    worm_sum = scientific(worm_sum, digits = 2)
  )

# Convert to LaTeX
summary_table %>%
  kbl(
    format = "latex", 
    booktabs = TRUE, 
    caption = "Summary Table of Indices by Treatment and Year"
  ) %>%
  kable_styling(latex_options = c("hold_position"))























