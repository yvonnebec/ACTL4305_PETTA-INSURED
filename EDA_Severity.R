
#################
# Severity EDA
#################

#
# Import library
#

install.packages("devtools")
library(devtools)
devtools::install_github('Mikata-Project/ggthemr')
library(ggthemr)
ggthemr('fresh')

library(rpart)
library(tidyverse)
library(lubridate)

#
# Import Data set
#
Data_full <- read.csv("actl4305_final_df_v8.csv")
View(Data_full)
Data_claims <- Data_full %>% 
  mutate(average_claim_amount = cumulative_claim_amount / num_claims) %>% 
  filter(num_claims > 0)

#################
# Severity EDA  #
#################

#######################
# Pet Characteristics #
#######################

###----------------PET GENDER----------------###
average_claim_severity_gender <- Data_claims %>%
  group_by(pet_gender) %>%
  summarise(average_claim_severity = mean(average_claim_amount, na.rm = TRUE))

# Create the bar plot
severity_pet_gender <- ggplot(average_claim_severity_gender, aes(x = pet_gender, y = average_claim_severity, fill = pet_gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Pet Gender", x = "Pet Gender", y = "Average Claim Severity") +
  theme_minimal() + theme(legend.position = "none") +
  theme(text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))


###----------------PET De Sexed----------------###

average_claim_severity_desex <- Data_claims %>%
  group_by(pet_de_sexed) %>%
  summarise(average_claim_severity = mean(average_claim_amount, na.rm = TRUE))

# Create the bar plot
severity_desexed <- ggplot(average_claim_severity_desex, aes(x = pet_de_sexed, y = average_claim_severity, fill = pet_de_sexed)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Pet Desexed status", x = "Pet Desexed status", y = "Average Claim Severity") +
  theme_minimal() + theme(legend.position = "none") +
  theme(text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5)) 

###----------------PET De Sexed Age----------------###

average_claim_by_desex_age <- Data_claims %>%  
  group_by(pet_de_sexed_age) %>%  
  drop_na(pet_de_sexed_age) %>%  
  summarise(average_claim_severity = mean(average_claim_amount, na.rm = TRUE)) %>%
  mutate(pet_de_sexed_age = factor(pet_de_sexed_age, levels = c("0-3 months", "7-12 months", "1-2 yr", "2+ years", "None", "Not Sure"))) %>%
  arrange(pet_de_sexed_age)

average_claim_by_desex_age


# plot bar plot
severity_pet_desexed_age <- ggplot(average_claim_by_desex_age, aes(x = pet_de_sexed_age, y = average_claim_severity, fill = pet_de_sexed_age)) + 
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Pet Desexed Age", x = "Pet Desexed Age", y = "Average Claim Severity") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5)) 

severity_pet_desexed_age

###----------------PET Age----------------###

Data_full$pet_age_years <- factor(Data_full$pet_age_years, 
                                  levels = c("0-6 months", "7-12 months", "1 years", "2 years", 
                                             "3 years", "4 years", "5 years", "6 years", 
                                             "7 years", "8 years", "9 years", "10 years"))



# Group "9 years" and "10 years" together into "9+ years" using ifelse
Data_full$pet_age_years <- ifelse(Data_full$pet_age_years %in% c("9 years", "10 years"), 
                                  "9+ years", 
                                  Data_full$pet_age_years)

Data_full$pet_age_years <- factor(Data_full$pet_age_years, 
                                  levels = c("0-6 months", "7-12 months", "1 years", "2 years", 
                                             "3 years", "4 years", "5 years", "6 years", 
                                             "7 years", "8 years", "9+ years"))

claim_severity_by_pet_age <- Data_claims %>%
  group_by(pet_age_years) %>%
  summarise(
    average_claim_severity = mean(average_claim_amount, na.rm = TRUE),
    num_pets = n()
  ) %>%
  mutate(
    pet_age_years = factor(
      pet_age_years, 
      levels = c("0-6 months", "7-12 months", sort(unique(pet_age_years[!pet_age_years %in% c("0-6 months", "7-12 months")]))
      ))
  ) %>%
  arrange(pet_age_years)

severity_pet_age <- ggplot(claim_severity_by_pet_age, aes(x = pet_age_years, y = average_claim_severity)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Pet Age Group", x = "Pet Age Group", y = "Average Claim Severity") +
  theme_minimal()

severity_pet_age

########################
# Plan Characteristics #
########################

###-------------------MULTI-PLAN--------------------###

summary_table <- Data_claims %>%
  group_by(is_multi_pet_plan) %>%
  summarise(
    avg_claim_severity = mean(average_claim_amount, na.rm = TRUE),
    median_claim_severity = median(average_claim_amount, na.rm = TRUE)
  )

# Bar plot for average claim severity by multi-pet plan status
severity_multi_plan <- ggplot(summary_table, aes(x = is_multi_pet_plan, y = avg_claim_severity, fill = is_multi_pet_plan)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Multi-Pet Plan Status", x = "Multi-Pet Plan", y = "Average Claim Severity") +
  theme_minimal()

#########################
# Breed Characteristics #
#########################

###---------------BREED Type-----------------###

summary_data <- Data_claims %>%
  group_by(nb_breed_type) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE))

# Bar plot of average claim severity by nb_breed_type
severity_breed_type <- ggplot(summary_data, aes(x = nb_breed_type, y = avg_claim_severity, fill = nb_breed_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Breed Type",
       x = "Breed Type", y = "Average Claim Severity") +
  theme_minimal()


###---------------BREED GROUP-----------------###

avg_claim_severity_breed <- Data_claims %>%
  group_by(breed_group) %>%
  summarise(mean_claim_severity = mean(average_claim_amount, na.rm = TRUE)) %>%
  arrange(mean_claim_severity)

severity_breed_group <- ggplot(avg_claim_severity_breed, aes(x = reorder(breed_group, -mean_claim_severity), y = mean_claim_severity)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Breed Trait", x = "Breed Trait", y = "Average Claim Severity") +
  theme_minimal()

###-------------- BREED SIZE --------------------###

severity_total <- Data_claims %>%
  filter(!is.na(nb_average_breed_size)) %>%
  mutate(nb_average_breed_size_bands = cut(nb_average_breed_size, 
                                           breaks = c(1, 2, 3, 4),
                                           labels = c("1-2", "2-3", "3+"),
                                           include.lowest = TRUE))


avg_severity_bands_breed_size <- severity_total %>%
  group_by(nb_average_breed_size_bands) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE))

severity_breed_size <- ggplot(avg_severity_bands_breed_size, aes(x = nb_average_breed_size_bands, y = avg_claim_severity)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Breed Size", 
       x = "Breed Size", 
       y = "Average Claim Severity") +
  theme_minimal()

###---------------- Number of Breeds x Breed Type -----------------###

severity_breed_trait_size <- ggplot(severity_total, aes(x = nb_average_breed_size_bands, fill = nb_breed_type)) +
  geom_bar(position = "fill") +  
  labs(title = "Proportion of Breed Traits Across Breed Size", 
       x = "Breed Size Bands", 
       y = "Proportion of Traits") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format())

severity_breed_trait_size

###---------------- Number of Breeds -----------------###

avg_claim_severity_breeds <- Data_claims %>%
  filter(!is.na(nb_number_of_breeds)) %>%
  group_by(nb_number_of_breeds) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE))

# Plot a bar graph comparing nb_number_of_breeds for average claim severity
severity_num_breeds <- ggplot(avg_claim_severity_breeds, aes(x = as.factor(nb_number_of_breeds), y = avg_claim_severity)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Number of Breeds", 
       x = "Number of Breeds", 
       y = "Average Claim Severity") +
  theme_minimal()
severity_num_breeds

##########################
# Policy Characteristics #
##########################

###------------------- quote_date --------------------------###
summary_data <- Data_claims %>%
  drop_na(quote_time_group) %>%
  group_by(quote_time_group) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE)) 


# Bar plot of average claim severity by quote time
severity_time_day <- ggplot(summary_data, aes(x = quote_time_group, y = avg_claim_severity, fill = quote_time_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity Quote Time", x = "Quote Time", y = "Average Claim Severity") +
  theme_minimal()
severity_time_day

summary_data <- Data_claims %>%
  mutate(quote_date = as.Date(quote_date))

summary_data <- summary_data %>%
  mutate(quote_month = floor_date(quote_date, "month"))

summary_data <- summary_data %>%
  group_by(quote_month) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE))

severity_claim_over_month <- ggplot(summary_data, aes(x = quote_month, y = avg_claim_severity)) + 
  geom_line(color = "steelblue", size = 1.2) + 
  geom_point(color = "steelblue", size = 3) + 
  labs(title = "Average Claim Severity by Quote Month", 
       x = "Quote Month", 
       y = "Average Claim Severity") + 
  scale_y_continuous(labels = scales::comma) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal()

###------------------- Policy First Inception --------------------------###

severity_claims_by_month <- Data_claims %>% 
  mutate(nb_policy_first_inception_date = as.Date(nb_policy_first_inception_date)) %>% 
  mutate(month = floor_date(nb_policy_first_inception_date, "month")) %>% 
  group_by(month) %>%
  summarise(
    average_claim = mean(average_claim_amount, na.rm = TRUE),
    median_claim = median(average_claim_amount, na.rm = TRUE)
  )

severity_claim_by_month <- ggplot(severity_claims_by_month, aes(x = month)) + 
  geom_line(aes(y = average_claim, color = "Average Claim"), size = 1) + 
  geom_line(aes(y = median_claim, color = "Median Claim"), size = 1, linetype = "dashed") +  
  labs(title = "Average and Median Claim Amount by Month", 
       x = "Month of Policy Inception", 
       y = "Average Claim Severity") + 
  scale_color_manual(name = "Claim Type", values = c("Average Claim" = "steelblue", "Median Claim" = "darkorange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

###------------------- Switcher --------------###

summary_data <- Data_claims %>%
  drop_na(pet_is_switcher) %>%
  group_by(pet_is_switcher) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE)) 


# Bar plot of average claim severity by switcher
severity_is_switcher <- ggplot(summary_data, aes(x = pet_is_switcher, y = avg_claim_severity, fill = pet_is_switcher)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Switcher", x = "Switcher Status", y = "Average Claim Severity") +
  theme_minimal()

##########################
# Policy Characteristics #
##########################

###--------------------CONTRIBUTION--------------------------###

average_claim_by_contribution <- Data_claims %>%
  group_by(nb_contribution) %>%
  summarise(average_claim = mean(average_claim_amount, na.rm = TRUE))

severity_contribution <- ggplot(average_claim_by_contribution, aes(x = factor(nb_contribution), y = average_claim, fill = factor(nb_contribution))) + 
  geom_bar(stat = "identity") +
  labs(title = " Average Claim Severity by Contribution Level", 
       x = "Contribution Level", 
       y = "Average Claim Severity", 
       fill = "Contribution Level") + 
  theme_minimal()

###---------------EXCESS--------------###

mean_excess_claim <- Data_claims %>% 
  group_by(nb_excess_FLAG) %>% 
  summarise(mean_claim_amount = mean(average_claim_amount))

severity_excess <- ggplot(mean_excess_claim, aes(x = nb_excess_FLAG, y = mean_claim_amount, fill = nb_excess_FLAG)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Excess Status", 
       x = "Excess Status", 
       y = "Average Claim Severity", 
       fill = "Excess Status") +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "peru"),
                    labels = c("FALSE" = "No Excess", "TRUE" = "Excess")) +
  scale_x_discrete(labels = c("FALSE" = "No Excess", "TRUE" = "Excess")) + 
  theme_minimal()

severity_excess
###---------------Contribution Excess--------------###

severity_claims <- Data_claims %>%
  mutate(contribution_excess_category = paste0(nb_contribution, ", ", nb_excess))

severity_claims <- severity_claims %>%
  mutate(contribution_excess_category = factor(contribution_excess_category,
                                               levels = c("80, 0", "80, 100", "80, 200",
                                                          "90, 0", "90, 100", "90, 200",
                                                          "100, 0", "100, 100", "100, 200")))

mean_claim_by_contribution_excess <- severity_claims %>%
  group_by(contribution_excess_category) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE))

###------------------- Contribution x Excess  --------------------------###

severity_contribution_excess <- ggplot(mean_claim_by_contribution_excess, 
  aes(x = contribution_excess_category, y = mean_claim_amount, fill = contribution_excess_category)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Average Claim Severity by Contribution and Excess", 
       x = "Contribution & Excess", 
       y = "Average Claim Severity") + 
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

severity_contribution_excess

#########################
# Owner Characteristics #
#########################

###------------------- Address Type --------------------------###

mean_severity_address <- Data_claims %>% 
  group_by(nb_address_type_adj) %>% 
  summarise(mean_address = mean(average_claim_amount))

severity_address_type <- ggplot(mean_severity_address, aes(x = nb_address_type_adj, y = mean_address, fill = nb_address_type_adj)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Address Type"
      x = "Address Type",
       y = "Average Claim Severity",
       fill = "Address Type") +
  theme_minimal()

################################
# Geographical Characteristics #
################################

###------------------- State --------------------------###

mean_claim_by_state <- Data_claims %>%
  group_by(nb_state) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE))

severity_state <- ggplot(mean_claim_by_state, aes(x = nb_state, y = avg_claim_severity, fill = nb_state)) + 
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by State", 
       x = "State", 
       y = "Average Claim Severity", 
       fill = "State") + 
  scale_fill_manual(values = c("steelblue", "peru", "lightsteelblue","coral", "darkslategray", "saddlebrown", "tan", "darkgray")) +  # Custom color palette
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### --------------------------- GENERATIONS ---------------------- ###

# Creating bins for generations based on age ranges
summary_data <- Data_claims %>%
  mutate(
    generation = cut(owner_age_years,
                     breaks = c(0, 24, 39, 54, 74, Inf),
                     labels = c("Gen Z", "Millennials", "Gen X", "Baby Boomers", "Silent Generation"),
                     right = FALSE)
  )

summary_data <- summary_data %>%
  drop_na(generation) %>%
  group_by(generation) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE)) 

# Bar plot of average claim severity by generation
severity_owner_generation <- ggplot(summary_data, aes(x = generation, y = avg_claim_severity, fill = generation)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Generation", x = "Generation", y = "Average Claim Severity") +
  theme_minimal()

############################
# External Characteristics #
############################

###------------------- Median Taxable Income --------------------------###

severity_claims <- Data_claims %>% 
  group_by(average_income_band) %>% 
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE)) 

severity_claims$average_income_band <- factor(
  severity_claims$average_income_band,
  levels = c("20-50k", "50-75k", "75-100k", "100-125k", "125-150k", "150-175k", "175-200k", "200k+")
)

severity_income <- ggplot(severity_claims, aes(x = average_income_band, y = avg_claim_severity)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Severity by Average Income Band",
       x = "Average Income",
       y = "Average Claim Severity") +
  theme_minimal()
severity_income

severity_claims_median <- Data_claims %>% 
  group_by(median_taxable_income) %>% 
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE)) 

severity_claims_median <- severity_claims_median %>%
  mutate(median_income_band = cut(
    median_taxable_income,
    breaks = c(0, 20000, 40000, 60000, 80000, 100000),
    labels = c("0-20k", "20-40k", "40-60k", "60-80k", "80-100k"),
    include.lowest = TRUE
  )) %>%
  group_by(median_income_band) %>% 
  summarise(avg_claim_severity = mean(avg_claim_severity, na.rm = TRUE)) 

severity_median_incme <- ggplot(severity_claims_median, aes(x = median_income_band, y = avg_claim_severity)) +
  geom_col() +
  labs(title = "Average Claim Severity by Median Income Band",
       x = "Median Income Band",
       y = "Average Claim Severity") +
  theme_minimal()


###------------------- Agricultural Land Band --------------------------###

severity_claims_agriculutral_land <- Data_claims %>% 
  group_by(agricultural_land_BAND) %>% 
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE)) 

severity_agriculture <- ggplot(severity_claims_agriculutral_land, aes(x = agricultural_land_BAND, y = avg_claim_severity)) +
  geom_col() +
  labs(title = "Average Claim Severity by Agricultural Land Band",
       x = "Agricultural Land",
       y = "Average Claim Severity") +
  theme_minimal()

###------------------- National Parks --------------------------###

severity_claims_national_parks <- Data_claims %>% 
  group_by(national_parks_BAND) %>% 
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE)) 

severity_national_parks <- ggplot(severity_claims_national_parks, aes(x = national_parks_BAND, y = avg_claim_severity)) +
  geom_col() +
  labs(title = "Average Claim Severity by National Parks Band",
       x = "National Parks",
       y = "Average Claim Severity") +
  theme_minimal()

severity_claims_national_parks <- Data_claims %>% 
  group_by(national_parks_band) %>% 
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE)) 

severity_national_parks <- ggplot(severity_claims_national_parks, aes(x = national_parks_band, y = avg_claim_severity)) +
  geom_col() +
  labs(title = "Average Claim Severity by National Parks Band",
       x = "National Parks",
       y = "Average Claim Severity") +
  theme_minimal()

severity_national_parks
###------------------- Income x State --------------------------###

Data_claims$average_income_band <- factor(
  severity_claims$average_income_band,
  levels = c("20-50k", "50-75k", "75-100k", "100-125k", "125-150k", "150-175k", "175-200k", "200k+")
)

mean_claim_by_state_income <- Data_claims %>%
  group_by(average_income_band, nb_state) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE))

severity_income_state <- ggplot(mean_claim_by_state_income, aes(x = average_income_band, y = avg_claim_severity, fill = nb_state)) +
  geom_col() +
  labs(
    title = "Average Claim Severity by Income Band and State",
    x = "Average Income Band",
    y = "Average Claim Severity",
    fill = "State"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###------------------- Income x National Park --------------------------###

mean_claim_by_national_income <- Data_claims %>%
  group_by(average_income_band, national_parks_BAND) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE))

severity_income_national <- ggplot(mean_claim_by_national_income, aes(x = average_income_band, y = avg_claim_severity, fill = national_parks_BAND)) +
  geom_col() +
  labs(
    title = "Average Claim Severity by Income Band and National Park Band",
    x = "Average Income Band",
    y = "Average Claim Severity",
    fill = "State"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###------------------- Income x Excess --------------------------###

mean_claim_by_excess_income <- Data_claims %>%
  group_by(average_income_band, nb_excess_FLAG) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE))

severity_income_excess <- ggplot(mean_claim_by_excess_income, aes(x = average_income_band, y = avg_claim_severity, fill = nb_excess_FLAG)) +
  geom_col() +
  labs(
    title = "Average Claim Severity by Income Band and Excess Flag",
    x = "Average Income Band",
    y = "Average Claim Severity",
    fill = "Excess Flag"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###------------------- Breed Group x National Park --------------------------###

mean_claim_by_national_breed <- Data_claims %>%
  group_by(breed_group, national_parks_BAND) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE))

severity_breed_national <- ggplot(mean_claim_by_national_breed, aes(x = breed_group, y = avg_claim_severity, fill = national_parks_BAND)) +
  geom_col() +
  labs(
    title = "Average Claim Severity by Breed Group and National Park Band",
    x = "Breed Group",
    y = "Average Claim Severity",
    fill = "National Parks"
  ) +
  theme_minimal()

###------------------- Generations x Income --------------------------###

mean_claim_by_income_generation <- Data_claims %>%
  mutate(
    generation = cut(owner_age_years,
                     breaks = c(0, 24, 39, 54, 74, Inf),
                     labels = c("Gen Z", "Millennials", "Gen X", "Baby Boomers", "Silent Generation"),
                     right = FALSE)
  ) %>% 
  drop_na(generation) %>% 
  group_by(average_income_band, generation) %>%
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE))

severity_income_owner < -ggplot(mean_claim_by_income_generation, aes(x = average_income_band, y = avg_claim_severity, fill = generation)) +
  geom_col() +
  labs(
    title = "Average Claim Severity by Income Band and Owner Generation",
    x = "Income Band",
    y = "Average Claim Severity",
    fill = "Owner Generation Band"
  ) +
  theme_minimal()

###------------------- Breed Severity Band --------------------------###

Data_claims$breed_severity_band <- case_when(
  Data_claims$nb_breed_trait %in% c("bull", "mastiff", "pinscher", "spitz related") ~ "High Severity",
  Data_claims$nb_breed_trait %in% c("cross", "retriever", "hound", "shepherd type", 
                                    "pointer", "spaniel", "teckel",  "sighthound", "collie related") ~ "Moderate Severity",
  Data_claims$nb_breed_trait %in% c("terrier", "setter") ~ "Low Severity",
  Data_claims$nb_breed_trait %in% c("unknown", "Unknown", "traditional", "white fluffy", "water dog") ~ "Unknown"
)

# Convert the new column to factor and check distribution
Data_claims$breed_severity_band <- factor(Data_claims$breed_severity_band, 
                                          levels = c("High Severity", "Moderate Severity", "Low Severity", "Unknown"))

severity_breed_severity <- Data_claims %>%
  group_by(breed_severity_band) %>%
  drop_na(breed_severity_band) %>% 
  summarise(avg_claim_severity = mean(average_claim_amount, na.rm = TRUE))

severity_severity_band <- ggplot(severity_breed_severity, aes(x = breed_severity_band, y = avg_claim_severity)) +
  geom_col() +
  labs(
    title = "Average Claim Severity by Breed Severity",
    x = "Breed Severity",
    y = "Average Claim Severity",
    fill = "Breed Severity"
  ) +
  theme_minimal()
