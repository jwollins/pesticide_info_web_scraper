## create summary stats for web scraped pesticide toxicity data 
## J Collins



#_________________________________________________________________________####
# Packages ####


library(dplyr)
library(kableExtra)
library(lme4)
library(emmeans)
library(ggplot2)




#_________________________________________________________________________####
# LOAD DATA ####

dat <- read.csv(file = "sym_link_pesticide_data/data/pesticide_data/danish_pli_total_pli_data.csv")

# combined_dat <- read.csv(file = "Data/agronomy/data/pesticide_data/pesticide_properties_data.csv")

dat$year <- factor(dat$year, levels = c(2022, 2023, 2024))

dat$treatment <- factor(dat$treatment, levels = c("Conventional", "Conservation"))



#_________________________________________________________________________####
# Functions ####


source(file = "~/Documents/GitHub/phd_tools/fun_distribution_plots.R")
source(file = "~/Documents/GitHub/phd_tools/fun_glm_diagnostic_plots.R")



#_________________________________________________________________________####
# SUMMARY TABLES ####


# ~ PLI ECO ####

names(dat)

# Calculates mean, sd, se and IC - block
ecotox_indic_x_rate <- 
  dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n = n(),
    sum = sum(ecotox_indic_x_rate, na.rm = TRUE),
    sd = sd(ecotox_indic_x_rate, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(year)

ecotox_indic_x_rate


distribution_plots(data = dat, 
                   variable = dat$ecotox_indic_x_rate, 
                   colour = dat$ecotox_indic_x_rate)

ggsave(filename = "sym_link_pesticide_data/plots/distributions/dist_ecotox_indic_x_rate.png", width = 10, height = 2.25)


glm_model <- glmer(formula = ecotox_indic_x_rate +1 ~ treatment + (1 | year) + (1 | crop), 
                   data = dat, 
                   family = Gamma(link = "log"))


# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_pesticide_data/plots/model_diagnostics/ecotox_indic_x_rate.png", 
       width = 10, height = 3.5)





# ~ PLI FATE ####

names(dat)

# Calculates mean, sd, se and IC - block
env_fate_indic_x_rate <- 
  dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n = n(),
    sum = sum(env_fate_indic_x_rate, na.rm = TRUE),
    sd = sd(env_fate_indic_x_rate, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(year)

env_fate_indic_x_rate


distribution_plots(data = dat, 
                   variable = dat$env_fate_indic_x_rate, 
                   colour = dat$env_fate_indic_x_rate)

ggsave(filename = "sym_link_pesticide_data/plots/distributions/dist_env_fate_indic_x_rate.png", 
       width = 10, height = 2.25)


glm_model <- glmer(formula = env_fate_indic_x_rate +1 ~ treatment + (1 | year) + (1 | crop), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)


# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_pesticide_data/plots/model_diagnostics/env_fate_indic_x_rate.png", 
       width = 10, height = 3.5)








# ~ PLI HH ####

# Calculates mean, sd, se and IC - block
human_health_indic_x_rate <- 
  dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n = n(),
    sum = sum(human_health_indic_x_rate, na.rm = TRUE),
    sd = sd(human_health_indic_x_rate, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(year)

human_health_indic_x_rate


distribution_plots(data = dat, 
                   variable = dat$human_health_indic_x_rate, 
                   colour = dat$human_health_indic_x_rate)

ggsave(filename = "sym_link_pesticide_data/plots/distributions/dist_human_health_indic_x_rate.png", 
       width = 10, height = 2.25)


glm_model <- glmer(formula = human_health_indic_x_rate +1 ~ treatment + (1 | year) + (1 | crop), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)


# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_pesticide_data/plots/model_diagnostics/human_health_indic_x_rate.png", 
       width = 10, height = 3.5)









# ~ PLI ####

names(dat)

# Calculates mean, sd, se and IC - block
total_pli_x_rate <- 
  dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n = n(),
    sum = sum(total_pli_x_rate, na.rm = TRUE),
    sd = sd(total_pli_x_rate, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(year)

total_pli_x_rate


distribution_plots(data = dat, 
                   variable = dat$total_pli_x_rate, 
                   colour = dat$total_pli_x_rate)

ggsave(filename = "sym_link_pesticide_data/plots/distributions/dist_total_pli_x_rate.png", 
       width = 10, height = 2.25)


glm_model <- glmer(formula = total_pli_x_rate +1 ~ treatment + (1 | year) + (1 | crop), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)


# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_pesticide_data/plots/model_diagnostics/total_pli_x_rate.png", 
       width = 10, height = 3.5)












#_________________________________________________________________________####
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








# ____________ ####
# combined eco risk tables ####

glimpse(pli_bees_sum)

summary_table <- pli_bees_sum %>%
  rename(n = n, bee_sum = sum) %>%
  # mutate(n_bees = n) %>%  # Preserve 'n' before joining
  # select(-n) %>%  # Remove the original 'n' to avoid confusion
  inner_join(
    pli_birds_sum %>%
      rename(birds_sum = sum), 
    by = c("treatment", "year")
  ) %>%
  inner_join(
    pli_fish_sum %>%
      rename(fish_sum = sum), 
    by = c("treatment", "year")
  ) %>%
  inner_join(
    pli_mammal_sum %>%
      rename(mammal_sum = sum), 
    by = c("treatment", "year")
  ) %>%
  inner_join(
    pli_worms_sum %>%
      rename(worm_sum = sum), 
    by = c("treatment", "year")
  ) %>%
  inner_join(
    pli_collembola_sum %>%
      rename(collembola_sum = sum), 
    by = c("treatment", "year")
  ) %>%
  select(year, treatment, bee_sum, birds_sum, fish_sum, mammal_sum, worm_sum, collembola_sum) 



# Print the resulting summary table
print(summary_table)



# Assuming summary_table is your data frame
summary_by_treatment <- summary_table %>%
  group_by(treatment) %>%
  summarise(
    # total_n = sum(n),                  # Total of 'n' per treatment
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







#_______________________________________________####
# stats & distributions ####


library(lme4)
library(janitor)
library(emmeans)

dat <- clean_names(combined_dat)

source(file = "~/Documents/GitHub/phd_tools/fun_distribution_plots.R")
source(file = "~/Documents/GitHub/phd_tools/fun_glm_diagnostic_plots.R")




# ~ soil dt50 ####

shapiro.test(dat$soil_degradation_days_aerobic_dt_field_dt_field)
bartlett.test(dat$soil_degradation_days_aerobic_dt_field_dt_field, 
              g = dat$treatment)

distribution_plots(data = dat, 
                   variable = dat$soil_degradation_days_aerobic_dt_field_dt_field, 
                   colour = combined_dat$treatment)

ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/dist_soil_dt50.png", 
       width = 10, height = 2.25)


kruskal.test(soil_degradation_days_aerobic_dt_field_dt_field ~ treatment, data = dat)


# dat <- dat %>%
#   mutate(soil_degradation_days_aerobic_dt_field_dt_field = soil_degradation_days_aerobic_dt_field_dt_field + 0.1)
# 
# 

# glm_model <- 
#   glmer(formula = dat$soil_degradation_days_aerobic_dt_field_dt_field ~ 
#                      treatment + (1 | year) + (1 | crop), 
#                    data = dat, 
#         family = Gamma(link = "log"))
# 
# # View summary
# summary(glm_model)
# 
# # Run pairwise comparisons for the 'Treatment' factor
# pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)
# 
# # View the results of pairwise comparisons
# summary(pairwise_comparisons)
# 
# 
# diagnostic_plots_glm(model = glm_model)
# 
# ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/model_diag_soildt50_glmer.png", 
#        width = 10, height = 3.5)
# 
# 
# # get the contrast in non-inverse scale 
# 
# # Regrid emmeans to response scale
# em_response <- regrid(emmeans(glm_model, ~ treatment, type = "response"))
# 
# # Now calculate contrasts on the response scale
# contrast_response <- contrast(em_response, method = "pairwise")
# 
# # Display result
# summary(contrast_response)




# ~ GUS ####

shapiro.test(dat$gus_leaching_potential_index_gus_leaching_potential_index_gus_leaching_potential_index)
bartlett.test(dat$gus_leaching_potential_index_gus_leaching_potential_index_gus_leaching_potential_index, 
              g = dat$treatment)

distribution_plots(data = dat, 
                   variable = dat$gus_leaching_potential_index_gus_leaching_potential_index_gus_leaching_potential_index, 
                   colour = combined_dat$treatment)

ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/dist_gus.png", 
       width = 10, height = 2.25)






lm_model <-
  lmer(formula = dat$gus_leaching_potential_index_gus_leaching_potential_index_gus_leaching_potential_index ~
          treatment + (1 | year) + (1 | crop),
        data = dat)

# View summary
summary(lm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(lm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/model_diag_soildt50_glmer.png",
       width = 10, height = 3.5)









# ~ worm PLI ####

print(pli_worms_sum)

shapiro.test(dat$pli_earthworms)
bartlett.test(dat$pli_earthworms, 
              g = dat$treatment)

distribution_plots(data = dat, 
                   variable = dat$pli_earthworms, 
                   colour = combined_dat$treatment)

ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/dist_worms.png", 
       width = 10, height = 2.25)


kruskal.test(pli_earthworms ~ treatment, data = dat)

# dat <- dat %>%
#   mutate(pli_earthworms = pli_earthworms + 0.1)
# 
# glm_model <- 
#   glmer(formula = dat$pli_earthworms ~ 
#           treatment + (1 | year) + (1 | crop), 
#         data = dat, 
#         family = Gamma(link = "log"))
# 
# # View summary
# summary(glm_model)
# 
# # Run pairwise comparisons for the 'Treatment' factor
# pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)
# 
# # View the results of pairwise comparisons
# summary(pairwise_comparisons)
# 
# 
# diagnostic_plots_glm(model = glm_model)
# 
# ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/model_diag_worms_glmer.png", 
#        width = 10, height = 3.5)




# ~ bees PLI ####

print(pli_bees_sum) %>% arrange(treatment)

shapiro.test(dat$pli_bees)
bartlett.test(dat$pli_bees, 
              g = dat$treatment)

distribution_plots(data = dat, 
                   variable = log(dat$pli_bees), 
                   colour = combined_dat$treatment)

ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/dist_bees.png", 
       width = 10, height = 2.25)


kruskal.test(pli_bees ~ treatment, data = dat)


# dat <- dat %>%
#   mutate(pli_bees = pli_bees + 5)
# 
# glm_model <- 
#   glmer(formula = log(dat$pli_bees) ~ 
#           treatment + (1 | year) + (1 | crop), 
#         data = dat, 
#         family = Gamma(link = "log"))
# 
# # View summary
# summary(glm_model)
# 
# # Run pairwise comparisons for the 'Treatment' factor
# pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)
# 
# # View the results of pairwise comparisons
# summary(pairwise_comparisons)
# 
# 
# # get the contrast in non-inverse scale 
# 
# # Regrid emmeans to response scale
# em_response <- regrid(emmeans(glm_model, ~ treatment, type = "response"))
# 
# # Now calculate contrasts on the response scale
# contrast_response <- contrast(em_response, method = "pairwise")
# 
# # Display result
# summary(contrast_response)
# 
# 
# diagnostic_plots_glm(model = glm_model)
# 
# ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/model_diag_worms_glmer.png", 
#        width = 10, height = 3.5)





# ~ birds PLI ####

print(pli_birds_sum) %>% arrange(treatment)

shapiro.test(dat$pli_birds)
bartlett.test(dat$pli_birds, 
              g = dat$treatment)

distribution_plots(data = dat, 
                   variable = dat$pli_birds, 
                   colour = combined_dat$treatment)

ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/dist_birds.png", 
       width = 10, height = 2.25)


kruskal.test(pli_birds ~ treatment, data = dat)

# # dat <- dat %>%
# #   mutate(pli_bees = pli_birds + 20)
# 
# lm_model <- 
#   lmer(formula = dat$pli_birds ~ 
#           treatment + (1 | year) + (1 | crop), 
#         data = dat)
# 
# glm_model <- glm(dat$pli_birds ~ treatment, data = dat, family = gaussian())
# 
# 
# # View summary
# summary(glm_model)
# 
# # Run pairwise comparisons for the 'Treatment' factor
# pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)
# 
# # View the results of pairwise comparisons
# summary(pairwise_comparisons)
# 
# 
# # get the contrast in non-inverse scale 
# 
# # Regrid emmeans to response scale
# em_response <- regrid(emmeans(glm_model, ~ treatment, type = "response"))
# 
# # Now calculate contrasts on the response scale
# contrast_response <- contrast(em_response, method = "pairwise")
# 
# # Display result
# summary(contrast_response)
# 
# 
# diagnostic_plots_glm(model = glm_model)
# 
# ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/model_diag_birds_glmer.png", 
#        width = 10, height = 3.5)





# ~ mammal PLI ####

print(pli_mammal_sum) %>% arrange(treatment)

shapiro.test(dat$pli_mammal)
bartlett.test(dat$pli_mammal, 
              g = dat$treatment)

distribution_plots(data = dat, 
                   variable = dat$pli_mammal, 
                   colour = combined_dat$treatment)

ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/dist_mammal.png", 
       width = 10, height = 2.25)


kruskal.test(pli_mammal ~ treatment, data = dat)


# dat <- dat %>%
#   mutate(pli_mammal = pli_mammal + 20)
# 
# glm_model <- glmer(formula = pli_mammal ~ treatment + (1 | year) + (1 | crop), 
#                    data = dat, 
#                    family = Gamma(link = "inverse"))
# 
# 
# # View summary
# summary(glm_model)
# 
# # Run pairwise comparisons for the 'Treatment' factor
# pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)
# 
# # View the results of pairwise comparisons
# summary(pairwise_comparisons)
# 
# 
# # get the contrast in non-inverse scale 
# 
# # Regrid emmeans to response scale
# em_response <- regrid(emmeans(glm_model, ~ treatment, type = "response"))
# 
# # Now calculate contrasts on the response scale
# contrast_response <- contrast(em_response, method = "pairwise")
# 
# # Display result
# summary(contrast_response)
# 
# 
# diagnostic_plots_glm(model = glm_model)
# 
# ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/model_diag_mammals_glmer.png", 
#        width = 10, height = 3.5)






# ~ fish PLI ####

print(pli_fish_sum) %>% arrange(treatment)

shapiro.test(dat$pli_fish)
bartlett.test(dat$pli_fish, 
              g = dat$treatment)

distribution_plots(data = dat, 
                   variable = log(dat$pli_fish)+10, 
                   colour = combined_dat$treatment)

ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/distributions/dist_fish.png", 
       width = 10, height = 2.25)


kruskal.test(pli_fish ~ treatment, data = dat)


