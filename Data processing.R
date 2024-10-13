# Assignment EDA

# Library Imports
library(tidyverse)
library(lubridate)


# Import data sets
sample_price <- read.csv("Sample_price_output_file.csv")
claims_data <- read.csv("UNSW_claims_data.csv")
earned_data <- read.csv("UNSW_earned_data_adjusted_Sep27.csv")

earned_data[earned_data == ""] <- NA
claims_data[claims_data == ""] <- NA

claims_data <- claims_data[,1:8]

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

# not sure why this code is necessary 
# 
# severity_dataset <- severity_dataset %>%
#   filter(claim_status == "covered_with_exclusions_paid") %>% # Apply condition
#   group_by(exposure_id) %>%
#   mutate(excess_applied = ifelse(n() == 1, TRUE, FALSE)) %>%
#   ungroup()


# If there's only more than 1 claim, ordering by date and condition,
# the first has excess applied

severity_dataset <- severity_dataset %>%
  group_by(exposure_id, condition_category, 
           claim_month = floor_date(as.Date(claim_start_date), "month")) %>%
  arrange(exposure_id, claim_start_date) %>%  # Ensure sorting by claim_start_date
  mutate(excess_applied = ifelse(row_number() == 1, TRUE, FALSE)) %>%  # Set flag for the first claim only
  ungroup()

severity_dataset_backed <- severity_dataset %>%
  mutate(backed_out_claims = case_when(
    # If claim_status is "covered_paid", take total_claim_amount
    claim_status == "covered_paid" ~ total_claim_amount,
    
    # If claim_status is "covered_with_exclusions_paid" and excess is applied
    claim_status == "covered_with_exclusions_paid" & excess_applied == TRUE ~ (claim_paid / (nb_contribution / 100)) + (nb_excess / 1.1),
    
    # If claim_status is "covered_with_exclusions_paid" and no excess is applied
    claim_status == "covered_with_exclusions_paid" & excess_applied == FALSE ~ (claim_paid / (nb_contribution / 100)),
    
    # For all other claim_status values where excess is applied
    excess_applied == TRUE ~ (claim_paid / (nb_contribution / 100)) + (nb_excess / 1.1),
    
    # For all other claim_status values where no excess is applied
    excess_applied == FALSE ~ (claim_paid / (nb_contribution / 100))
  ))

severity_dataset_backed <- severity_dataset_backed %>%
  filter(!(claim_paid == 0))

#######################################
# Concatenating by condition category #
#######################################

severity_claim_data_flat <- severity_dataset_backed %>%
  group_by(id) %>% 
  summarise(
    multiple_claims = ifelse(n() > 1, TRUE, FALSE),
    num_claims = n(),
    num_unique_conditions = n_distinct(condition_category),
    claim_first_date = min(claim_start_date),
    claim_last_date = max(claim_start_date),
    cumulative_claim_amount = sum(backed_out_claims),
    cumulative_claim_paid = sum(claim_paid),
    cumulative_total_claim_amount = sum(total_claim_amount)
    # tenure_difference = max(tenure) - min(tenure), Not sure about tenure diff
  ) %>% 
  ungroup() %>% 
  mutate(exposure_id = substring(id, 3)) %>%
  mutate(tenure = as.numeric(substr(id, 1, 1)))

severity_claim_data_flat <- as.data.frame(severity_claim_data_flat)
###############################
# Merge back into earned data #
###############################

severity_total <- earned_data %>% 
  left_join(severity_claim_data_flat, by = "id") %>% 
  select(-exposure_id.y) %>%
  rename(exposure_id = exposure_id.x) %>% 
  mutate(
    cumulative_claim_paid = ifelse(is.na(cumulative_claim_paid), 0, cumulative_claim_paid),
    cumulative_total_claim_amount = ifelse(is.na(cumulative_total_claim_amount), 0, cumulative_total_claim_amount),
    num_unique_conditions = ifelse(is.na(num_unique_conditions), 0, num_unique_conditions),
    num_claims = ifelse(is.na(num_claims), 0, num_claims),
    multiple_claims = ifelse(is.na(multiple_claims), FALSE, multiple_claims)
  )

#View(severity_total %>% group_by(exposure_id) %>% filter(n() > 5))

##############################
# Updating column data types #
##############################

date_columns <- c(
  "UW_Date",
  "nb_policy_first_inception_date",
  "person_dob",
  "quote_date",
  "quote_date"
)

# If you only need the date part and not the time, you can use as.Date
severity_total$claim_first_date <- as.Date(severity_total$claim_first_date, format="%d/%m/%Y")
severity_total$claim_last_date <- as.Date(severity_total$claim_last_date, format="%d/%m/%Y")


factor_columns <- c("exposure_id", "pet_gender", "pet_de_sexed", "pet_is_switcher", "nb_address_type_adj", 
                    "nb_suburb", "nb_state", "nb_breed_type", "nb_breed_trait", "nb_breed_name_unique",
                    "nb_breed_name_unique_concat", "is_multi_pet_plan", "quote_time_group",
                    "exposure_id_1", "id", "pet_age_years", "pet_de_sexed_age")


integer_columns <- c("num_claims", "num_unique_conditions",
                     "tenure.y")


#Date conversion
severity_total <- severity_total %>%
  mutate(across(all_of(date_columns), as.Date))

#Integer conversion
severity_total <- severity_total %>%
  mutate(across(all_of(integer_columns), as.integer))

#Factor conversion 
severity_total <- severity_total %>%
  mutate(across(all_of(factor_columns), as.factor))

#### Feature engineering ####

# Create a data frame with the mapping of breeds to their respective groups
breed_mapping <- data.frame(
  nb_breed_trait = c(
    "pinscher", "spitz related", "white fluffy", 
    "collie related", "pointer", "retriever", "setter", "shepherd type", "spaniel", "water dog", 
    "brachycephalic", "bull", "mastiff", "hound", "sighthound", 
    "teckel", "terrier", 
    "cross", "traditional", "unknown"
  ),
  breed_group = c(
    rep("Small Dogs (Toy/Companion)", 3),
    rep("Medium/Active Dogs (Working/Sporting)", 7),
    rep("Large/Heavy Dogs (Mastiffs, Bulls)", 5),
    rep("Terriers", 2),
    rep("Cross-breeds/Unknowns", 3)
  )
)

# Join the mapping to the original data frame
severity_total <- severity_total %>%
  left_join(breed_mapping, by = "nb_breed_trait")


#### Excess flag
severity_total <- severity_total %>%
  mutate(nb_excess_FLAG = ifelse(nb_excess > 0, TRUE, FALSE))

class(severity_total$nb_excess_FLAG)

#### Pet age - years

unique(severity_total$pet_age_years)
class(severity_total$pet_age_years)


####  Average_Claim_Paid
severity_total <- severity_total %>% 
  mutate(average_claim_amount = cumulative_claim_amount / num_claims) %>% 
  mutate(average_claim_amount = ifelse(is.na(average_claim_amount), 0, average_claim_amount))


##
## External Dataset
## 2021-2022 Year
## Source: https://data.gov.au/data/dataset/taxation-statistics-2021-22/resource/ea4fd20a-4d97-4fc1-918c-e175cd8db3fd?inner_span=True

median_income <- read.csv("MedianIncome_Postcode.csv", header=TRUE)
colnames(median_income) <- median_income[1,]
median_income <- median_income[-1,]
median_income <- median_income %>% select(Postcode, "Average taxable income or loss3\n$","Median taxable income or loss3\n$")

colnames(median_income) <- c("postcode", "average_taxable_income", "median_taxable_income")

severity_total <- merge(
  severity_total, median_income, by.x = "nb_postcode", by.y = "postcode", all.x = TRUE)

View(severity_total)
###
### Export Severity to CSV
###
write.csv(severity_total, "severity_total.csv", row.names = FALSE)

