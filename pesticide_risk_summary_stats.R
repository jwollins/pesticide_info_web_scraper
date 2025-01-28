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


#*******************************************************************************####
# PLOTS ####


#*******************************************************************************####
# ~~~~~~~~~~~ ####
## ~ soil DT50 plots ####


names(combined_dat)


### ~~ general soil half life plot ####

# Assuming 'combined_dat' has a column for 'Pesticide' and 'Soil degradation'
a <- ggplot(combined_dat, 
            aes(x = ai_name, 
                y = Soil.degradation..days...aerobic..Soil.degradation..days...aerobic..DT....typical., 
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
                y = Soil.degradation..days...aerobic..DT....field..DT....field., 
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
                              y = Soil.degradation..days...aerobic..DT....lab.at.20..C..DT....lab.at.20..C., 
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
# ~~~~~~~~~~~ ####
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



### ~~ birds ####

# Assuming 'combined_dat' has a column for 'Pesticide' and 'Soil degradation'
c <- ggplot(combined_dat, 
            aes(x = ai_name, 
                y = `Birds - Acute LD₅₀ (mg kg⁻¹) Birds - Acute LD₅₀ (mg kg⁻¹) Birds - Acute LD₅₀ (mg kg⁻¹)`, 
                fill = ai_name)) +
  geom_col() +  # Use geom_col() when you already have the values
  theme_minimal() +
  # ylim(0,800) +
  labs(
    title = expression("Birds - Acute" ~ LD[50] ~ (mg ~ kg^{-1})),
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

c

### ~~ fish ####

# Assuming 'combined_dat' has a column for 'Pesticide' and 'Soil degradation'
d <- ggplot(combined_dat, 
            aes(x = ai_name, 
                y = `Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹) Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹) Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹)`, 
                fill = ai_name)) +
  geom_col() +  # Use geom_col() when you already have the values
  theme_minimal() +
  # ylim(0,800) +
  labs(
    title = expression("Freshwater Fish - Acute 96 hr" ~ LC[50] ~ (mg ~ l^{-1})),
    x = "Pesticide Active Ingrdient",
    y = expression(LC[50] ~ (mg ~ l^{-1})),
    fill = "Active Ingrdient"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()  # Optionally remove x-axis ticks
  )
# theme(axis.text.x = element_text(angle = -45, hjust = 0))  # Rotate x-axis labels if necessary

d


### ~~ Bees ####

# Plot title using expression
plot_title <- expression(
  "Honeybees (" * italic("Apis spp.") * ") Contact acute LD" [50] *
    " (" * mu * "g bee" ^ -1 * ")"
)

# Assuming 'combined_dat' has a column for 'Pesticide' and 'Soil degradation'
e <- ggplot(combined_dat, 
            aes(x = ai_name, 
                y = `Honeybees (Apis spp.) Honeybees (Apis spp.) Contact acute LD₅₀ (worst case from 24, 48 and 72 hour values - μg bee⁻¹)`, 
                fill = ai_name)) +
  geom_col() +  # Use geom_col() when you already have the values
  theme_minimal() +
  # ylim(0,800) +
  labs(
    title = plot_title,
    x = "Pesticide Active Ingrdient",
    y = expression(
      "LD" [50] *
        " (" * mu * "g bee" ^ -1 * ")"
    ),
    fill = "Active Ingrdient"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()  # Optionally remove x-axis ticks
  )
# theme(axis.text.x = element_text(angle = -45, hjust = 0))  # Rotate x-axis labels if necessary

e



ggarrange(a,b,c,d,e, 
          ncol = 3, 
          nrow = 2, 
          common.legend = TRUE, 
          legend = "bottom", 
          labels = c("A","B","C","D","E"))

ggsave(filename = "Data/agronomy/plots/fig_ecotox_pesticide_values.png", width = 13, height = 8)



#*******************************************************************************
# ~~~~~~~~~~~ ####
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








## ~ PLI PLOT ####

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












#*******************************************************************************####
# BARPLOTS ####


#******************************************************************************####
# ~~~~~~~~~~~ ####
## ~ general indexes by treatment year ####



### ~~ TLI barplots ####

# plot TLI by treatment and year
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


### ~~ PLI barplots ####

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



### ~~ GUS index ####

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
# ~~~~~~~~~~~ ####
## ~ eco risk plot by treatment x year ####


### ~~ worm risk index ####

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


### ~~ mammal risk index ####

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



### ~~ collembola risk index ####

# # plot worm index by treatment and year
# collembola_plot <-
#   ggplot(data = collembola_risk_index_sum, 
#          aes(x = treatment, 
#              y = sum, 
#              fill = treatment)) + 
#   geom_bar(stat = "identity", 
#            color = "black", 
#            position = "dodge") + 
#   labs(
#     subtitle = "Collembola Risk Index",
#     x = "Treatment",
#     y = "Risk Index",
#     caption = "") +
#   theme_bw() +
#   scale_fill_manual(values=c("turquoise3","tomato2"), 
#                     name = "Treatment") +
#   theme(strip.text.x = element_text(size = 12, 
#                                     color = "black", 
#                                     face = "bold.italic"), 
#         legend.position = "bottom", 
#         axis.text.x = element_blank(), 
#         axis.title.x = element_blank()) +
#   facet_wrap(~ year, 
#              ncol = 4, 
#              scales = 'free_x') 
# collembola_plot


### ~~ birds risk index ####

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



### ~~ bee risk index ####

names(bees_ld50)

# Plot title using expression
plot_title <- expression("Honey Bee (" * italic("Apis spp.") * ") Risk Index")

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
    subtitle = plot_title,
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


### ~~ fish risk index ####

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


ggarrange(worm_plot, bee_plot, mammal_plot, birds_plot, fish_plot, 
          ncol = 3, 
          nrow = 2, 
          common.legend = TRUE, 
          legend = "bottom", 
          labels = c("A","B","C","E","F","G"))

ggsave(filename = "Data/agronomy/plots/ecotox_risk_indexes.png", width = 10, height = 6)

