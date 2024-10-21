# Assignment EDA

# Library Imports
library(tidyverse)
library(lubridate)


# Import data sets
sample_price <- read.csv("Sample_price_output_file.csv")
claims_data <- read.csv("UNSW_claims_data.csv")
earned_data <- read.csv("UNSW_earned_data_adjusted_Sep27.csv")

View(claims_data)
View(earned_data)
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




View(severity_dataset_backed)

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

View(severity_total %>% group_by(exposure_id) %>% filter(n() > 5))

severity_total <- severity_total %>%
  filter(earned_units > 0) 
##############################
# Updating column data types #
##############################

# date_columns <- c(
#   "UW_Date",
#   "nb_policy_first_inception_date",
#   "person_dob",
#   "quote_date",
#   "quote_date"
# )

# If you only need the date part and not the time, you can use as.Date
severity_total$claim_first_date <- as.Date(severity_total$claim_first_date, format="%d/%m/%Y")
severity_total$claim_last_date <- as.Date(severity_total$claim_last_date, format="%d/%m/%Y")

factor_columns <- c("exposure_id", "pet_gender", "pet_de_sexed", "pet_is_switcher", "nb_address_type_adj", 
                    "nb_suburb", "nb_state", "nb_breed_type", "nb_breed_trait", "nb_breed_name_unique",
                    "nb_breed_name_unique_concat", "is_multi_pet_plan", "quote_time_group",
                    "exposure_id", "id", "pet_age_years")


integer_columns <- c("num_claims", "num_unique_conditions")


#Date conversion
severity_total <- severity_total %>%
  mutate(across(all_of(date_columns), as.Date))

#Integer conversion
severity_total <- severity_total %>%
  mutate(across(all_of(integer_columns), as.integer))

#Factor conversion 
severity_total <- severity_total %>%
  mutate(across(all_of(factor_columns), as.factor))







severity_total$claim_frequency <- severity_total$num_claims/severity_total$earned_units
class(severity_total$claim_frequency)
class(severity_total$earned_units)



library(dplyr)

#### Feature engineering ####

#### Breed mapping

# Define the breed mapping data frame with correct counts
breed_mapping <- data.frame(
  nb_breed_trait = c(
    "pinscher", "spitz related", "white fluffy", 
    "collie related", "pointer", "retriever", "setter", "shepherd type", "spaniel", "water dog", 
    "brachycephalic", "bull", "mastiff", "hound", "sighthound", 
    "teckel", "terrier", 
    "cross", "traditional", "unknown"  # Ensuring each entry has a corresponding group
  ),
  breed_group = c(
    rep("Small Dogs (Toy/Companion)", 3),
    rep("Medium/Active Dogs (Working/Sporting)", 7),
    rep("Large/Heavy Dogs (Mastiffs, Bulls)", 5),
    rep("Terriers", 2),
    "Cross-breeds",    # Map "cross" to "Cross-breeds"
    "Unknown",         # Map "unknown" to "Unknown"
    "Unknown"          # Map explicitly unknown to "Unknown"
  )
)

# Join the mapping to the original data frame
severity_total <- severity_total %>%
  left_join(breed_mapping, by = "nb_breed_trait")

# Map NAs in breed_group to "Unknown" for any rows without breed_trait
severity_total$breed_group[is.na(severity_total$breed_group) & 
                             is.na(severity_total$nb_breed_trait)] <- "Unknown"

table(severity_total$breed_group)

#### Excess flag

severity_total <- severity_total %>%
  mutate(nb_excess_FLAG = ifelse(nb_excess > 0, TRUE, FALSE))

class(severity_total$nb_excess_FLAG)

#### Pet age - years

unique(severity_total$pet_age_years)
class(severity_total$pet_age_years)

severity_total$pet_age_years <- factor(severity_total$pet_age_years, 
                                       levels = c("0-6 months", "7-12 months", "1 years", "2 years", "3 years", 
                                                  "4 years", "5 years", "6 years", "7 years", "8 years", 
                                                  "9 years", "10 years"))


#### Treating de sexed age ####

# Replace NA values with "None"
severity_total$pet_de_sexed_age[is.na(severity_total$pet_de_sexed_age)] <- "None"

# Optionally, convert the variable to a factor (if you need it to be a categorical variable)
severity_total$pet_de_sexed_age <- as.factor(severity_total$pet_de_sexed_age)

# To verify the changes
unique(severity_total$pet_de_sexed_age)

table(severity_total$pet_de_sexed_age)

# Combine similar levels
severity_total$pet_de_sexed_age <- recode_factor(severity_total$pet_de_sexed_age,
                                                 "0-3 mo" = "0-3 months",
                                                 "4-6 mo" = "4-6 months",
                                                 "7-12 mo" = "7-12 months",
                                                 "2+ yr" = "2+ years")

# Reorder the levels in a meaningful order
severity_total$pet_de_sexed_age <- factor(severity_total$pet_de_sexed_age, 
                                          levels = c("None", "0-3 months", "4-6 mo", "7-12 months", 
                                                     "1-2 yr", "2+ years", "Not Sure"))

# Verify the new levels
levels(severity_total$pet_de_sexed_age)

# Manually fix the levels using levels() to ensure renaming works
levels(severity_total$pet_de_sexed_age) <- gsub("4-6 mo", "4-6 months", levels(severity_total$pet_de_sexed_age))

# Reorder levels after renaming
severity_total$pet_de_sexed_age <- factor(severity_total$pet_de_sexed_age, 
                                          levels = c("None", "0-3 months", "4-6 months", "7-12 months", 
                                                     "1-2 yr", "2+ years", "Not Sure"))

levels(severity_total$pet_de_sexed_age)

#### Feature engineering ####

# Replace NA values with 0 for claim-related columns
severity_total$cumulative_claim_amount[is.na(severity_total$cumulative_claim_amount)] <- 0
severity_total$num_claims[is.na(severity_total$num_claims)] <- 0
severity_total$claim_frequency[is.na(severity_total$claim_frequency)] <- 0
severity_total$num_unique_conditions[is.na(severity_total$num_unique_conditions)] <- 0

# Alternatively, you can use a more concise approach
severity_total <- severity_total %>%
  mutate(has_claim = num_claims > 0) 

sum(severity_total$has_claim) 

severity_total <- severity_total %>%
  mutate(owner_age_band = case_when(
    owner_age_years < 20 ~ "Under 20",
    owner_age_years >= 20 & owner_age_years < 30 ~ "20-29",
    owner_age_years >= 30 & owner_age_years < 40 ~ "30-39",
    owner_age_years >= 40 & owner_age_years < 50 ~ "40-49",
    owner_age_years >= 50 & owner_age_years < 60 ~ "50-59",
    owner_age_years >= 60 & owner_age_years < 70 ~ "60-69",
    owner_age_years >= 70 ~ "70+",
    TRUE ~ "Unknown"  # In case of NA or missing values
  ))

severity_total$owner_age_band <- as.factor(severity_total$owner_age_band)


severity_total$nb_breed_trait[is.na(severity_total$nb_breed_trait)] <- "Unknown"

# Ensure it's a factor variable
severity_total$nb_breed_trait <- factor(severity_total$nb_breed_trait)


# Define the mapping
# Define the refined mapping based on GLM results
refined_breed_map <- c("brachycephalic" = "Brachycephalic",
                       "hound" = "Hound",
                       "teckel" = "Teckel",
                       "retriever" = "Sporting",
                       "pointer" = "Sporting",
                       "spaniel" = "Sporting",
                       "setter" = "Sporting",
                       "water dog" = "Sporting",
                       "cross" = "Mixed Breed",
                       "collie related" = "Herding",
                       "shepherd type" = "Shepherd",
                       "mastiff" = "Working",
                       "pinscher" = "Working",
                       "terrier" = "Working",
                       "bull" = "Working",
                       "Unknown" = "Unknown",
                       "unknown" = "Unknown",
                       "spitz related" = "Non-Sporting",
                       "white fluffy" = "Non-Sporting",
                       "traditional" = "Other")

refined_breed_map <- c(
  "teckel" = "Teckel",           # Keep as its own level
  "shepherd type" = "Shepherd",  # Keep as its own level
  "hound" = "Hound",             # Keep as its own level
  "brachycephalic" = "Other",    # Group all other breeds as "Other"
  "retriever" = "Other",
  "pointer" = "Other",
  "spaniel" = "Other",
  "setter" = "Other",
  "water dog" = "Other",
  "cross" = "Other",
  "collie related" = "Other",
  "mastiff" = "Other",
  "pinscher" = "Other",
  "terrier" = "Other",
  "bull" = "Other",
  "spitz related" = "Other",
  "white fluffy" = "Other",
  "traditional" = "Other",
  "Unknown" = "Unknown",         # Keep Unknown as it is
  "unknown" = "Unknown"
)

# Apply the refined mapping
severity_total$breed_MAP <- refined_breed_map[as.character(severity_total$nb_breed_trait)]

# Convert to factor
severity_total$breed_MAP <- factor(severity_total$breed_MAP)

severity_total$breed_MAP

severity_total$breed_MAP[is.na(severity_total$breed_MAP)] <- "Unknown"

# Ensure breed_MAP is still a factor
severity_total$breed_MAP <- factor(severity_total$breed_MAP)

#### External datasets mapping ####

## External Dataset 1.
## 2021-2022 Year
## Source: https://data.gov.au/data/dataset/taxation-statistics-2021-22/resource/ea4fd20a-4d97-4fc1-918c-e175cd8db3fd?inner_span=True

median_income <- read.csv("MedianIncome_Postcode.csv", header=TRUE)
colnames(median_income) <- median_income[1,]
median_income <- median_income[-1,]
median_income <- median_income %>% select(Postcode, "Average taxable income or loss3\n$","Median taxable income or loss3\n$")

colnames(median_income) <- c("postcode", "average_taxable_income", "median_taxable_income")

severity_total <- merge(
  severity_total, median_income, by.x = "nb_postcode", by.y = "postcode", all.x = TRUE)

# Remove commas and convert to numeric for median_taxable_income
severity_total$median_taxable_income <- as.numeric(gsub(",", "", severity_total$median_taxable_income))

# Remove commas and convert to numeric for average_taxable_income
severity_total$average_taxable_income <- as.numeric(gsub(",", "", severity_total$average_taxable_income))


## External Dataset 2.
## Source: Provided

land_environment <- read.csv("EXTERNAL_LANDENVIRONMENT.csv", skip = 6, header = FALSE)
SA4 <- read.csv("postcode_SA4.csv")
SA2 <- read.csv("postcode_SA2.csv")
#View(land_environment)
#View(SA4)

colnames(land_environment) <- land_environment[1, ]
land_environment <- land_environment[-1, ]

# Identify columns with missing names
names(land_environment)[names(land_environment) == ""] <- "missing_name"

# Or if there are any NA column names
names(land_environment)[is.na(names(land_environment))] <- "missing_name"

land_environment <- land_environment %>%
  mutate(across(where(is.character), ~ na_if(., "-")))


land_environment$Year <- as.numeric(land_environment$Year)
land_environment$Code <- as.numeric(land_environment$Code)

land_environment_np <- land_environment %>%
  filter(Year == 2022) %>% select(Code,Label,Year, "National parks (%)") %>%
  rename("National_parks" = "National parks (%)")

# National Park % data and Postcode (2022 data)
merged_data_np <- land_environment_np %>%
  left_join(SA2, by = c("Code" = "SA2_MAINCODE_2011"))

# Area of agricultural land (ha) 2021
land_environment_al <- land_environment %>%
  filter(Year == 2021) %>% select(Code,Label,Year, "Area of agricultural land (ha)") %>%
  rename("Agriculture_land" = "Area of agricultural land (ha)")

merged_data_al <- land_environment_al %>%
  left_join(SA2, by = c("Code" = "SA2_MAINCODE_2011"))

land_data <- merge(merged_data_al, merged_data_np, by = "Code")

land_data <- land_data %>%
  select(-matches("\\.y$"), -Year.x)

colnames(land_data) <- gsub("\\.x$", "", colnames(land_data))

land_data <- land_data %>% distinct()

merged_data <- land_data %>%
  mutate(
    PERCENTAGE = as.numeric(PERCENTAGE) / 100,
    Agriculture_land = as.numeric(gsub(",", "", Agriculture_land)),
    National_parks = as.numeric(National_parks),
    weighted_national_parks = National_parks * PERCENTAGE,
    weighted_agricultural_land = Agriculture_land * PERCENTAGE,
  )

result <- merged_data %>%
  group_by(POSTCODE) %>%
  summarise(
    weighted_national_parks = sum(weighted_national_parks, na.rm = TRUE),
    weighted_agricultural_land = sum(weighted_agricultural_land, na.rm = TRUE),
  )

result$weighted_national_parks <- result$weighted_national_parks / 100


# Banding land data
banded_result <- result %>%
  mutate(
    national_parks_band = cut(weighted_national_parks * 100,
                              breaks = c(-Inf, 0, 5, 10, 25, 50, 75, 100),
                              labels = c("0", "0-5%", "5-10%", "15-25%", "25-50%", "50-75%", "75-100%"),
                              #breaks = c(0, 20, 40, 60, 80, 100),
                              #labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                              include.lowest = TRUE,
                              right = TRUE),
    agricultural_land_band = cut(weighted_agricultural_land,
                                 breaks = c(-Inf, 0, 10000, 100000, 1000000, 10000000, Inf),
                                 labels = c("0", "0-10,000", "10,000-100,000", "100,000-1,000,000", "1,000,000-10,000,000", "10,000,000+"),
                                 include.lowest = TRUE,
                                 right = TRUE)
  )



# severity_total <- severity_total %>%
#  select(-national_parks_band, -agricultural_land_band)

severity_total <- severity_total %>% 
  left_join(banded_result, by = c("nb_postcode" = "POSTCODE")) %>% 
  rename(agricultural_land = weighted_agricultural_land,
         national_parks = weighted_national_parks)

colnames(severity_total)

severity_total$agricultural_land_band[is.na(severity_total$agricultural_land_band)] <- "0"
severity_total$national_parks_band[is.na(severity_total$national_parks_band)] <- "0"

severity_total$national_parks[is.na(severity_total$national_parks)] <- 0
severity_total$agricultural_land[is.na(severity_total$agricultural_land)] <- 0

## External dataset 3. 
## Source: Provided; education

education <- read.csv("Education.csv")


### Filter and rename for all postcodes
education <- education %>%
  filter(Year == 2021) %>%
  rename(Total_fully_engaged_percent = Total.fully.engaged....,
         Completed_year12 = Completed.year.12.or.equivalent....,
         Unemployment_rate = Unemployment.rate....,
         Participation_rate = Participation.rate....
  ) %>%
  mutate(Code = as.numeric(Code),
         Total_fully_engaged_percent = as.numeric(Total_fully_engaged_percent),
         Completed_year12 = as.numeric(Completed_year12),
         Unemployment_rate = as.numeric(Unemployment_rate),
         Participation_rate = as.numeric(Participation_rate)
         
  )

# filter for all necessary variables
education_filtered <- education %>%
  select(Code, Total_fully_engaged_percent, Completed_year12,
         Unemployment_rate, Participation_rate)

# join datasets
merged_data <- education_filtered %>%
  left_join(SA4, by = c("Code" = "SA4_CODE_2011"))

# Convert the percentage column to numeric and apply it to the homelessness rate
merged_data <- merged_data %>%
  mutate(
    PERCENTAGE = as.numeric(PERCENTAGE) / 100,  
    weighted_Total_fully_engaged_percent = Total_fully_engaged_percent * PERCENTAGE,
    weighted_Completed_year12 = Completed_year12 * PERCENTAGE,
    weighted_Unemployment_rate = Unemployment_rate * PERCENTAGE,
    weighted_Participation_rate = Participation_rate * PERCENTAGE
  )



# Group by SA4_CODE_2011 and calculate the average weighted
result <- merged_data %>%
  group_by(POSTCODE) %>%
  summarise(
    avg_weighted_Total_fully_engaged_percent = sum(weighted_Total_fully_engaged_percent, na.rm = TRUE),
    avg_weighted_Completed_year12 = sum(weighted_Completed_year12, na.rm = TRUE),
    avg_weighted_Unemployment_rate = sum(weighted_Unemployment_rate, na.rm = TRUE),
    avg_weighted_Participation_rate = sum(weighted_Participation_rate, na.rm = TRUE)
  )

result <- result %>%
  mutate(POSTCODE = as.integer(POSTCODE))

# Merging result with severity_total based on nb_postcode
severity_total <- severity_total %>%
  left_join(result, by = c("nb_postcode" = "POSTCODE"))

## External dataset 4. 
## Source: Provided; family 

family <- read.csv("Family.csv")


### Filter and rename for all postcodes
family <- family %>%
  filter(Year == 2021) %>%
  rename(
    Averagehouseholdsize = Average.household.size..no..of.persons.,
    Averagefamilysize = Average.family.size..no..of.persons.,
    Socioeconomic_index = SEIFA.Index.of.relative.socio.economic.advantage.and.disadvantage..IRSAD....rank.within.Australia..decile.,
    Mortgageless30percent = Households.where.mortgage.repayments.are.less.than.or.equal.to.30..of.imputed.household.income....,
    Mortgagemore30percent = Households.where.mortgage.repayments.are.more.than.30..of.imputed.household.income....,
    rentmore30percent = Households.where.rent.payments.are.more.than.30..of.imputed.household.income....,
    rentless30percent = Households.where.rent.payments.are.less.than.or.equal.to.30..of.imputed.household.income....
  ) %>%
  mutate(Code = as.numeric(Code),
         Homeless_per10k = as.numeric(gsub(",", "", Homeless_per10k)),
         Averagehouseholdsize = as.numeric((Averagehouseholdsize)),
         Socioeconomic_index = as.numeric(Socioeconomic_index),
         Mortgageless30percent = as.numeric(Mortgageless30percent),
         Mortgagemore30percent = as.numeric(Mortgagemore30percent),
         rentmore30percent = as.numeric(rentmore30percent),
         rentless30percent = as.numeric(rentless30percent)
  )

# Link in mortgage

family_filtered <- family %>%
  select(Code, Homeless_per10k, Averagehouseholdsize, Socioeconomic_index,
         Mortgageless30percent, Mortgagemore30percent, rentmore30percent, rentless30percent)


# Need to look at postcode to SA mapping percentage to pull in
# every field from the data and use it to calc a weight average based on percentage in the SA

merged_data <- family_filtered %>%
  left_join(SA4, by = c("Code" = "SA4_CODE_2011"))

str(merged_data$Homeless_per10k)

# Convert the percentage column to numeric and apply it to the homelessness rate
# put in function?
merged_data <- merged_data %>%
  mutate(
    PERCENTAGE = as.numeric(PERCENTAGE) / 100,  
    weighted_homeless_rate = Homeless_per10k * PERCENTAGE,
    weighted_Averagehouseholdsize = Averagehouseholdsize * PERCENTAGE,
    weighted_socioeconomic_index = Socioeconomic_index * PERCENTAGE,
    weighted_Mortgageless30percent = Mortgageless30percent * PERCENTAGE,
    weighted_Mortgagemore30percent = Mortgagemore30percent * PERCENTAGE,
    weighted_rentmore30percent = rentmore30percent * PERCENTAGE,
    weighted_rentless30percent = rentless30percent * PERCENTAGE
  )

# Group by SA4_CODE_2011 and calculate the average weighted homelessness rate
result <- merged_data %>%
  group_by(POSTCODE) %>%
  summarise(
    avg_weighted_homeless_rate = sum(weighted_homeless_rate, na.rm = TRUE),
    weighted_Averagehouseholdsize = sum(weighted_Averagehouseholdsize, na.rm = TRUE),
    weighted_socioeconomic_index = sum(weighted_socioeconomic_index, na.rm = TRUE),
    weighted_Mortgageless30percent = sum(weighted_Mortgageless30percent, na.rm = TRUE),
    weighted_Mortgagemore30percent = sum(weighted_Mortgagemore30percent, na.rm = TRUE),
    weighted_rentmore30percent = sum(weighted_rentmore30percent, na.rm = TRUE),
    weighted_rentless30percent = sum(weighted_rentless30percent, na.rm = TRUE)
  )

result <- result %>%
  mutate(POSTCODE = as.integer(POSTCODE))

# Merging result with severity_total based on nb_postcode
severity_total <- severity_total %>%
  left_join(result, by = c("nb_postcode" = "POSTCODE"))

## External dataset 5. - Remoteness mapping
## Source: https://matthewproctor.com/australian_postcodes#downloadlinks

australian_postcodes <- read.csv("australian_postcodes.csv")

# Condense australian_postcodes based on postcode with majority logic for all variables
australian_postcodes_filtered <- australian_postcodes %>%
  group_by(postcode) %>%
  summarise(
    electoraterating = names(sort(table(electoraterating), decreasing = TRUE)[1]),  # Majority for electorate
    altitude = if (all(is.na(altitude))) { NA } else { names(sort(table(na.omit(altitude)), decreasing = TRUE)[1]) },  # Majority for altitude
    lat = if (all(is.na(lat))) { NA } else { names(sort(table(na.omit(lat)), decreasing = TRUE)[1]) },  # Most common lat
    long = if (all(is.na(long))) { NA } else { names(sort(table(na.omit(long)), decreasing = TRUE)[1]) }  # Most common long
  ) %>%
  mutate(
    electoraterating = ifelse(electoraterating == "", "Unknown", electoraterating)  # Replace missing electorate with 'Unknown'
  )

# Merge the filtered postcode data into severity_total
severity_total <- severity_total %>%
  left_join(australian_postcodes_filtered, by = c("nb_postcode" = "postcode"))

australian_postcodes_filtered <- australian_postcodes %>%
  group_by(postcode) %>%
  summarise(
    RA_2021_NAME = names(sort(table(RA_2021_NAME), decreasing = TRUE)[1]),  # Majority for electorate
  ) %>%
  mutate(
    RA_2021_NAME = ifelse(RA_2021_NAME == "", "Unknown", RA_2021_NAME)  # Replace missing electorate with 'Unknown'
  )

severity_total <- severity_total %>%
  left_join(australian_postcodes_filtered, by = c("nb_postcode" = "postcode"))

severity_total$electoraterating <- as.factor(severity_total$electoraterating)
severity_total$RA_2021_NAME <- as.factor(severity_total$RA_2021_NAME)


#### Modelling

severity_total <- severity_total %>% mutate(average_claim = cumulative_claim_amount/num_claims)

severity_total <- severity_total %>%
  mutate(pet_is_switcher = ifelse(is.na(pet_is_switcher), "unknown", pet_is_switcher))

severity_total$pet_is_switcher <- recode_factor(severity_total$pet_is_switcher,
                                                "1" = "FALSE",
                                                "2" = "TRUE",
                                                "unknown" = "Unknown")


#### Data manipulation based on modelling and EDA ####
library(dplyr)

# 0. Income

# Create the bands for median_taxable_income
severity_total$median_income_band <- cut(severity_total$median_taxable_income,
                                         breaks = c(0, 70000, Inf),
                                         labels = c("0-70k", "70k+"),
                                         include.lowest = TRUE, right = FALSE)

# Create the bands for average_taxable_income
severity_total$average_income_band <- cut(severity_total$average_taxable_income,
                                          breaks = c(20000, 50000, 75000, 100000, 125000, 150000, 175000, 200000, Inf),
                                          labels = c("20-50k", "50-75k", "75-100k", "100-125k", "125-150k", "150-175k", "175-200k", "200k+"),
                                          include.lowest = TRUE, right = FALSE)

# View the new variables
table(severity_total$median_income_band)
table(severity_total$average_income_band)

# 1. Create rent_BAND as a factor with levels 0-45% and 45%+
severity_total <- severity_total %>%
  mutate(rent_BAND = factor(
    case_when(
      weighted_rentmore30percent <= 45 ~ "0-45%",
      weighted_rentmore30percent > 45 ~ "45%+"
    ),
    levels = c("0-45%", "45%+")
  ))

# Check the table of rent_BAND
print("Table for rent_BAND:")
print(table(severity_total$rent_BAND))

# 2. Band household_size_BAND with levels 2-2.2 and 2.2+
severity_total <- severity_total %>%
  mutate(household_size_BAND = factor(
    case_when(
      weighted_Averagehouseholdsize <= 2.2 ~ "2-2.2",
      weighted_Averagehouseholdsize > 2.2 ~ "2.2+"
    ),
    levels = c("2-2.2", "2.2+")
  ))

# Check the table of household_size_BAND
print("Table for household_size_BAND:")
print(table(severity_total$household_size_BAND))

# 3. Band agricultural_land_BAND by 25k increments (0 to 100k) with proper levels
severity_total <- severity_total %>%
  mutate(agricultural_land_BAND = factor(
    case_when(
      agricultural_land <= 100000 ~ "0-100k",
      TRUE ~ "100k+"
    ),
    levels = c("0-100k", "100k+")
  ))

table(severity_total$agricultural_land_BAND)

# Check the table of agricultural_land_BAND
print("Table for agricultural_land_BAND:")
print(table(severity_total$agricultural_land_BAND))

# 4. Band national_parks_BAND with specified levels
severity_total <- severity_total %>%
  mutate(national_parks_BAND = factor(
    case_when(
      national_parks >= 0 & national_parks <= 0.09 ~ "0-9%",
      national_parks > 0.09 ~ ">9%"
    ),
    levels = c("0-9%", ">9%")
  ))

# Check the table of national_parks_BAND
print("Table for national_parks_BAND:")
print(table(severity_total$national_parks_BAND))

# 5. Band fully_engaged_BAND with specified levels
severity_total <- severity_total %>%
  mutate(fully_engaged_BAND = factor(
    case_when(
      avg_weighted_Total_fully_engaged_percent <= 70 ~ "0-70%",
      avg_weighted_Total_fully_engaged_percent > 70 & avg_weighted_Total_fully_engaged_percent <= 80 ~ "70-80%",
      avg_weighted_Total_fully_engaged_percent > 80 ~ "80-100%"
    ),
    levels = c("0-70%", "70-80%", "80-100%")
  ))

# Check the table of fully_engaged_BAND
print("Table for fully_engaged_BAND:")
print(table(severity_total$fully_engaged_BAND))

#6. Homeless


severity_total <- severity_total %>%
  mutate(avg_weighted_homeless_rate_banded = factor(case_when(
    avg_weighted_homeless_rate <= 100 ~ "0-100",
    avg_weighted_homeless_rate > 100  ~ ">100"
  ), levels = c("0-100", ">100")))



#7. Metropolitan

severity_total <- severity_total %>%
  mutate(electorate_rating_banded = factor(
    case_when(
      electoraterating == "Inner Metropolitan" ~ "Inner Metropolitan",
      TRUE ~ "Outside Inner Metropolitan"
    ),
    levels = c("Inner Metropolitan", "Outside Inner Metropolitan")
  ))

severity_total <- severity_total %>%
  mutate(RA_2021_NAME_banded = factor(
    case_when(
      RA_2021_NAME == "Major Cities of Australia" ~ "Major Cities of Australia",
      TRUE ~ "Outside Major Cities"
    ),
    levels = c("Major Cities of Australia", "Outside Major Cities")
  ))



# Pet age band

severity_total$pet_age_BAND <- cut(severity_total$pet_age_months,
                                   breaks = c(0, 6, 12, 36, Inf),
                                   labels = c("0-6 months", "6-12 months", "1-3 years", 
                                              "3+ years"),
                                   right = FALSE)


# breed size

severity_total <- severity_total %>%
  mutate(nb_average_breed_size_FACTOR = factor(
    case_when(
      nb_average_breed_size == 1 ~ "1",
      nb_average_breed_size > 1 & nb_average_breed_size <= 2 ~ "1-2",
      nb_average_breed_size > 2 & nb_average_breed_size <= 3 ~ "2-3",
      nb_average_breed_size > 3 & nb_average_breed_size <= 4 ~ "3-4",
      TRUE ~ NA_character_  # Set to NA for values greater than 4 or less than 1
    ),
    levels = c("1", "1-2", "2-3", "3-4")  # Set levels for the factor
  ))

table(severity_total$nb_average_breed_size_FACTOR)




#number of breeds

severity_total <- severity_total %>%
  mutate(binned_breeds = ifelse(nb_number_of_breeds == 1, "1", "2+"))

severity_total$binned_breeds <- as.factor(severity_total$binned_breeds)

# Check the result
table(severity_total$pet_age_BAND)


severity_total$nb_contribution_FACTOR <- as.factor(severity_total$nb_contribution)
severity_total$nb_excess_FACTOR <- as.factor(severity_total$nb_excess)

frequency_df <- severity_total

# Define the predictors
predictors <- c("tenure.x", "pet_de_sexed", "pet_age_years", 
                "owner_age_band", "nb_number_of_breeds", "nb_average_breed_size", 
                "breed_group", "is_multi_pet_plan", "nb_excess_FLAG", "majority_remoteness",
                "nb_address_type_adj", "nb_state", "pet_is_switcher")

frequency_df$nb_excess_FACTOR <- as.factor(frequency_df$nb_excess)

predictors <- c(#"pet_gender", 
                #"pet_is_switcher",
                #"pet_de_sexed", 
                #"is_multi_pet_plan",
                "nb_address_type_adj",
                "pet_age_BAND",
                "owner_age_band",
                #"nb_breed_type",
                "nb_average_breed_size_FACTOR",
                "binned_breeds",
                "median_income_band",
                "nb_excess_FACTOR",
                "breed_MAP",
                "nb_contribution_FACTOR",
                "agricultural_land_BAND",
                "national_parks_BAND",
                "electorate_rating_banded", 
                "median_income_band",
                "rent_BAND")


predictors <- c("binned_breeds",
                "breed_MAP",
                "nb_average_breed_size_FACTOR",
                #"nb_breed_type",
                "nb_address_type_adj",
                "nb_contribution_FACTOR",
                "agricultural_land_BAND",
                "national_parks_BAND",
                "electorate_rating_banded", 
                "median_income_band",
                "rent_BAND",
                "median_income_band",
                "nb_excess_FACTOR"
                )


predictors <- c("rentmore30_bands")
na_counts <- sapply(frequency_df[predictors], function(x) sum(is.na(x)))

# Print the NA counts for each predictor
print(na_counts)

# Remove rows with NAs in the predictor variables
frequency_df <- frequency_df %>%
  drop_na(num_claims, all_of(predictors))

# Set a threshold for earned_units (e.g., 1e-6)
frequency_df$earned_units[frequency_df$earned_units < 1e-6] <- 1e-6

frequency_formula <- as.formula(paste("num_claims ~", paste(predictors, collapse = " + ")))

freq_glm <- glm(frequency_formula, family = poisson(link = "log"), data = frequency_df, offset = log(earned_units))


# Summary of the model
summary(freq_glm)



#### num_claims modelling ####


predictors <- c("pet_gender", 
                "pet_de_sexed", 
                "is_multi_pet_plan", 
                "nb_address_type_adj",
                "pet_age_BAND",
                "owner_age_band",
                "nb_number_of_breeds",
                "nb_average_breed_size",
                "electoraterating",
                "median_income_band",
                "nb_excess_FACTOR",
                "breed_MAP",
                "homeless_count_per10k",
                "rent_BAND",
                "agricultural_land_BAND",
                "national_parks_BAND", 
                "fully_engaged_BAND",
                "nb_contribution_FACTOR",
                "num_claims")


num_conditions_df <-  severity_total %>%
  filter(num_claims > 1)

conditions_formula <- as.formula(paste("num_unique_conditions ~", paste(predictors, collapse = " + ")))

cond_glm <- glm(conditions_formula, family = poisson(link = "log"), data = num_conditions_df)

summary(cond_glm)



#### severity modelling

severity_df <-  severity_total %>%
  filter(has_claim == TRUE)

severity_df$average_claim <- severity_df$cumulative_claim_amount/severity_df$num_claims

predictors_sev <- predictors


#### severity variable remappingg ####

# Create a new column to group breeds based on both GLM estimates and breed traits
severity_df$breed_severity_band <- case_when(
  severity_df$nb_breed_trait %in% c("bull", "mastiff", "pinscher", "spitz related") ~ "High Severity",
  severity_df$nb_breed_trait %in% c("cross", "retriever", "hound", "shepherd type", 
                                    "pointer", "spaniel", "teckel",  "sighthound", "collie related") ~ "Moderate Severity",
  severity_df$nb_breed_trait %in% c("terrier", "setter") ~ "Low Severity",
  severity_df$nb_breed_trait %in% c("unknown", "Unknown", "traditional", "white fluffy", "water dog") ~ "Unknown"
)

# Convert the new column to factor and check distribution
severity_df$breed_severity_band <- factor(severity_df$breed_severity_band, 
                                          levels = c("High Severity", "Moderate Severity", "Low Severity", "Unknown"))

# Check the distribution of the new banded column
table(severity_df$breed_severity_band)



# Check the distribution of the new banded column
table(severity_df$breed_severity_band)


severity_df <- severity_df %>%
  mutate(national_parks_sev_band = factor(
    case_when(
      national_parks >= 0 & national_parks < 0.05 ~ "0-5%",
      national_parks >= 0.05 & national_parks < 0.40 ~ "5-40%",
      national_parks >= 0.40 ~ "40%+"
    ),
    levels = c("0-5%", "5-40%", "40%+")
  ))

severity_df <- severity_df %>%
  mutate(national_parks_sev_band = factor(
    case_when(
      national_parks >= 0 & national_parks <= 0.05 ~ "0-5%",
      national_parks > 0.05 ~ ">5%"
    ),
    levels = c("0-5%", ">5%")
  ))



severity_df$median_income_band_sev <- cut(severity_df$median_taxable_income,
                                         breaks = c(0, 50000, 60000, 70000, Inf),
                                         labels = c("0-50k", "5-6", "6-7", "70k+"),
                                         include.lowest = TRUE, right = FALSE)


predictors_sev <- c("agricultural_land_BAND",
                    "national_parks_sev_band",
                    "average_income_band",
                    "breed_severity_band", 
                    "pet_age_BAND",
                    "nb_excess_FACTOR")

# Assuming cumulative_claim_amount is the target variable




severity_formula <- as.formula(paste("cumulative_claim_amount ~", paste(predictors_sev, collapse = " + ")))

# Fit a GLM for cumulative claim amount with a Gamma family and log link
severity_glm <- glm(severity_formula, 
                    family = Gamma(link = "log"), 
                    data = severity_df,
                    offset = log(earned_units))

summary(severity_glm)








#################
# Handling NA's #
#################
colMeans(is.na(severity_dataset))

# Hardcode - impute by mode
# pet_de_sexed_age, pet_de_sexed_age (>70% NAs)

# Predict - using models for data imputation ( < 10 % NA's )
# owner_age_years, person_dob, nb_breed_trait