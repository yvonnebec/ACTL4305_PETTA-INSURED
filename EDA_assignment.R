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
