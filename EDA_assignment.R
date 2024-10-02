# Assignment EDA

# Library Imports
library(tidyverse)
library(lubridate)

# Import data sets
sample_price <- read.csv("Sample_price_output_file.csv")
claims_data <- read.csv("UNSW_claims_data.csv")
earned_data <- read.csv("UNSW_earned_data_adjusted_Sep27.csv")

View(claims_data)

earned_data[earned_data == ""] <- NA
claims_data[claims_data == ""] <- NA

colMeans(is.na(claims_data))
colMeans(is.na(earned_data))
# Notes to self:
# - Model larger and smaller claims differently

#################
# Data Cleaning #
#################

# Create id ~ tenure + exposure_id
claims_data <- claims_data %>% 
  mutate(id = paste(
    as.character(tenure),
    exposure_id, sep="-")
  )

earned_data <- earned_data %>% 
  mutate(id = paste(
    as.character(tenure),
    exposure_id, sep="-")
  )

# Join policy data on earned_data
severity_dataset <- claims_data %>% 
  left_join(earned_data, by="id")

# 13 Duplicates
duplicate_data <- severity_dataset[duplicated(severity_dataset), ]
nrow(duplicate_data)

# Remove duplicate claims and "accidental claims" (0 claim, 0 total claim)
severity_dataset <- severity_dataset %>%
  distinct() %>%  # Remove duplicate rows
  filter(!(claim_paid == 0 & total_claim_amount == 0))


# Filter policies for strictly positive tenure and positive earned_units
# Tenure < 0 implies old policy and therefore no risk since inception isn't
# current
# earned_units == 0 implies not a current risk
severity_dataset <- severity_dataset %>%
  mutate(exposure_id = exposure_id.x) %>% 
  mutate(tenure = tenure.x) %>% 
  select(-exposure_id.x, -exposure_id.y, -exposure_id_1) %>%
  select(-tenure.x, -tenure.y) %>% 
  filter(tenure >= 0, earned_units > 0)


#########################################
# Indicator Function for Excess Applied #
#########################################

# If there's only 1 claim for an exposure_id then excess is applied to that 
# claim

severity_dataset <- severity_dataset %>%
  group_by(exposure_id) %>%
  mutate(excess_applied = ifelse(n() == 1, TRUE, FALSE)) %>%
  ungroup()

# If there's only more than 1 claim, ordering by date and condition,
# the first has excess applied

severity_dataset <- severity_dataset %>%
  group_by(exposure_id, condition_category) %>%
  arrange(claim_start_date) %>%  # Sort by claim_start_date
  mutate(excess_applied = ifelse(row_number() == 1, TRUE, excess_applied)) %>% 
  ungroup()

#######################################
# Concatenating by condition category #
#######################################

# CHECK THIS LOGIC PLS

grouping <- severity_dataset %>%
  group_by(id, condition_category) %>% 
  arrange(claim_start_date) %>% 
  summarise(
    num_claims = n(),  # Count the number of claims
    claim_first_date = min(claim_start_date),  # Get the earliest claim start date
    claim_last_date = max(claim_start_date),   # Get the latest claim start date
    cumulative_claim_paid = sum(claim_paid, na.rm = TRUE),  # Sum claim_paid
    cumulative_total_claim_paid = sum(total_claim_amount, na.rm = TRUE),  # Sum total_claim_paid
    .groups = "keep"
  ) %>% 
  ungroup()

View(grouping)


##############################
# Updating column data types #
##############################

date_columns <- c(
  "claim_start_date",
  "UW_Date",
  "nb_policy_first_inception_date",
  "person_dob",
  "quote_date"
  )



factor_columns <- c("claim_status","condition_category", "claim_id",
                    "pet_gender", "pet_de_sexed", "pet_is_switcher", 
                    "nb_breed_type", "nb_breed_trait", "nb_breed_name_unique",
                    "nb_breed_name_unique_concat", 
                    "nb_address_type_adj", "nb_state", "is_multi_pet_plan",
                    "quote_time_group", "nb_suburb", "nb_postcode",
                    "pet_age_years")


numeric_columns <- c("claim_paid", "total_claim_amount", "pet_age_months",
                     "nb_contribution", "nb_excess", 
                     "nb_contribution_excess",
                     "owner_age_years", "nb_number_of_breeds", 
                     "nb_average_breed_size", "earned_units", "tenure",
                     "X", "row_num")

for (col in date_columns) {
  severity_dataset[[col]] <- as.Date(severity_dataset[[col]])
}

# Apply as.factor to the factor columns
for (col in factor_columns) {
  severity_dataset[[col]] <- as.factor(severity_dataset[[col]])
}

# Apply as.numeric to the numeric columns
for (col in numeric_columns) {
  severity_dataset[[col]] <- as.numeric(severity_dataset[[col]])
}

View(severity_dataset)



#################
# Handling NA's #
#################
colMeans(is.na(severity_dataset))

# Hardcode - mode
# pet_de_sexed_age, pet_de_sexed_age (>70% NAs)

# Predict - values using data imputation ( < 10 % NA's )
# owner_age_years, person_dob, nb_breed_trait
