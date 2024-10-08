# Assignment EDA

# Library Imports
library(tidyverse)
library(lubridate)


# Import data sets
sample_price <- read.csv("Sample_price_output_file.csv")
claims_data <- read.csv("UNSW_claims_data.csv")
earned_data <- read.csv("UNSW_earned_data_adjusted_Sep27.csv")


#View(claims_data)
#View(earned_data)
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

severity_dataset %>%  filter(nb_breed_trait == "bull") %>% count()

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


#View(severity_dataset_backed)

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
    cumulative_claim_amount = ifelse(is.na(cumulative_claim_amount), 0, cumulative_claim_amount),
    cumulative_total_claim_amount = ifelse(is.na(cumulative_total_claim_amount), 0, cumulative_total_claim_amount),
    num_unique_conditions = ifelse(is.na(num_unique_conditions), 0, num_unique_conditions),
    num_claims = ifelse(is.na(num_claims), 0, num_claims),
    multiple_claims = ifelse(is.na(multiple_claims), FALSE, multiple_claims)
  )

# View(severity_total %>% group_by(exposure_id) %>% filter(n() > 5))

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
severity_total$claim_first_date <- as.Date(severity_total$claim_first_date)
severity_total$claim_last_date <- as.Date(severity_total$claim_last_date)

factor_columns <- c("exposure_id", "pet_gender", "pet_de_sexed", "pet_is_switcher", "nb_address_type_adj", 
                    "nb_suburb", "nb_state", "nb_breed_type", "nb_breed_trait", "nb_breed_name_unique",
                    "nb_breed_name_unique_concat", "is_multi_pet_plan", "quote_time_group",
                    "exposure_id_1", "id","pet_age_years", "pet_de_sexed_age")


integer_columns <- c("num_claims", "num_unique_conditions", "tenure.y")


#Date conversion
severity_total <- severity_total %>%
  mutate(across(all_of(date_columns), as.Date))

#Integer conversion
severity_total <- severity_total %>%
  mutate(across(all_of(integer_columns), as.integer))

#Factor conversion 
severity_total <- severity_total %>%
  mutate(across(all_of(factor_columns), as.factor))

#View(severity_total)

#################
# Handling NA's #
#################
missing_percent <- colMeans(is.na(severity_total)) * 100
cols_to_impute <- names(missing_percent[missing_percent > 0 & missing_percent < 20])
cols_high_missing <- names(missing_percent[missing_percent >= 20])

library(rpart)
impute_missing_values <- function(data, target_column) {
  # Determine if the target column is numeric or factor
  is_factor <- is.factor(data[[target_column]])
  method_type <- ifelse(is_factor, "class", "anova")
  
  # Handle dates as before by converting them to numeric
  if (inherits(data[[target_column]], "Date")) {
    data[[paste0(target_column, "_numeric")]] <- as.numeric(data[[target_column]])
    target_column <- paste0(target_column, "_numeric")
    method_type <- "anova"  # Always use "anova" for dates
  }
  
  # Build the decision tree model
  formula <- as.formula(paste(target_column, "~ ."))
  model <- rpart(formula, data = data, method = method_type, na.action = na.omit)
  
  # Predict missing values in the target column
  missing_indices <- is.na(data[[target_column]])
  predicted_values <- predict(model, newdata = data[missing_indices, ], type = ifelse(is_factor, "class", "vector"))
  
  # Replace missing values with predictions
  data[[target_column]][missing_indices] <- predicted_values
  
  # Convert numeric date back to Date type if needed
  if (grepl("_numeric", target_column)) {
    original_column <- sub("_numeric", "", target_column)
    data[[original_column]] <- as.Date(data[[target_column]], origin = "1970-01-01")
    data[[target_column]] <- NULL  # Remove temporary numeric column
  }
  
  return(data)
}
severity_total <- impute_missing_values(severity_total, "person_dob")
# severity_total <- impute_missing_values(severity_total, "owner_age_years")

reference_date <- Sys.Date()
severity_total$owner_age_years[is.na(severity_total$owner_age_years)] <- 
  as.numeric(difftime(reference_date, severity_total$person_dob[is.na(severity_total$owner_age_years)], units = "weeks")) / 52.25

# Impute nb_breed_trait NAs values with the mode
mode_value <- names(sort(table(severity_total[["nb_breed_trait"]]), decreasing = TRUE))[1]
severity_total[["nb_breed_trait"]][is.na(severity_total[["nb_breed_trait"]])] <- mode_value

# Set NAs in pet_de_sexed_age to 0
levels(severity_total$pet_de_sexed_age) <- c(levels(severity_total$pet_de_sexed_age), "0")
severity_total$pet_de_sexed_age[is.na(severity_total$pet_de_sexed_age)] <- "0"

# Set NAs in pet_is_switcher to false
levels(severity_total$pet_is_switcher) <- c(levels(severity_total$pet_is_switcher), "false")
severity_total$pet_is_switcher[is.na(severity_total$pet_is_switcher)] <- "false"


################
# Severity EDA #
################

severity_claims <- severity_total %>% filter(num_claims > 0)

#
# Pet_gender 
#

ggplot(severity_claims, aes(x = pet_gender, y = cumulative_claim_paid)) +
  geom_boxplot() +
  labs(title = "Cumulative Claims by Pet Gender", x = "Pet Gender", y = "Cumulative Claims Paid") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,1000))

ggplot(severity_claims, aes(x = pet_gender, y = log(cumulative_claim_paid))) +
  geom_boxplot() +
  labs(title = "Log-Transformed Cumulative Claims by Pet Gender", x = "Pet Gender", y = "Log(Cumulative Claims + 1)") +
  theme_minimal()

#
# Pet_de_sexed 
#

ggplot(severity_claims, aes(x = pet_de_sexed, y = cumulative_claim_paid)) +
  geom_boxplot() +
  labs(title = "Cumulative Claims by Pet De-sexed Status", x = "De-sexed Status", y = "Cumulative Claims Paid") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,1000))

ggplot(severity_claims, aes(x = pet_de_sexed, y = log(cumulative_claim_paid))) +
  geom_boxplot() +
  labs(title = "Log-Transformed Cumulative Claims by Pet De-sexed Status", x = "De-sexed Status", y = "Log(Cumulative Claims + 1)") +
  theme_minimal()

#
# Pet_de_sexed_age
#

ggplot(severity_claims, aes(x = pet_de_sexed_age, y = cumulative_claim_paid)) +
  geom_point(alpha = 0.5) +
  labs(title = "Cumulative Claims by Pet De-sexed Age", x = "De-sexed Age", y = "Cumulative Claims Paid") +
  theme_minimal()

ggplot(severity_claims, aes(x = pet_de_sexed_age, y = log(cumulative_claim_paid))) +
  geom_point(alpha = 0.5) +
  labs(title = "Log-Transformed Cumulative Claims by Pet De-sexed Age", x = "De-sexed Age", y = "Log(Cumulative Claims + 1)") +
  theme_minimal()

#
# Pet_age_months
#

severity_age_bins <- severity_claims %>%
  mutate(age_bins = cut(pet_age_months, 
                        breaks = seq(0, max(pet_age_months, na.rm = TRUE), by = 5), 
                        include.lowest = TRUE, right = FALSE, 
                        labels = paste0(seq(0, max(pet_age_months, na.rm = TRUE), by = 5)[-length(seq(0, max(pet_age_months, na.rm = TRUE), by = 5))], 
                                        "-", 
                                        seq(5, max(pet_age_months, na.rm = TRUE), by = 5)-1)
  ))

ggplot(severity_age_bins, aes(x = age_bins, y = cumulative_claim_paid)) +
  geom_boxplot() +
  geom_jitter(aes(color = cumulative_claim_paid), width = 0.2, height = 0, alpha = 0.6) +
  labs(title = "Cumulative Claims by Pet Age (Months)", x = "Pet Age (Months)", y = "Cumulative Claims Paid") +
  theme_minimal()

ggplot(severity_age_bins, aes(x = age_bins, y = log(cumulative_claim_paid))) +
  geom_boxplot() +
  geom_jitter(aes(color = log(cumulative_claim_paid + 1)), width = 0.2, height = 0, alpha = 0.6) +
  labs(title = "Log-Transformed Cumulative Claims by Pet Age (Months)", x = "Pet Age (Months)", y = "Log(Cumulative Claims + 1)") +
  theme_minimal()

severity_bins <- severity_claims %>%
  mutate(age_bins = cut(pet_age_months, 
                        breaks = seq(0, max(pet_age_months, na.rm = TRUE), by = 5), 
                        include.lowest = TRUE, right = FALSE, 
                        labels = paste0(seq(0, max(pet_age_months, na.rm = TRUE), by = 5)[-length(seq(0, max(pet_age_months, na.rm = TRUE), by = 5))], 
                                        "-", 
                                        seq(5, max(pet_age_months, na.rm = TRUE), by = 5)-1)
  ))

ggplot(severity_bins, aes(x = age_bins, y = cumulative_claim_paid)) + 
  geom_boxplot() + 
  geom_jitter(aes(color = cumulative_claim_paid), width = 0.2, height = 0, alpha = 0.6) +
  labs(title = "Cumulative Claims Paid by Pet Age (Months)",
       x = "Pet Age (Months)",
       y = "Cumulative Claims Paid") + 
  theme_minimal() + 
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(limits = c(5000,20000))

#
# Nb_contribution
#

severity_total_80 <- severity_total %>% 
  filter(nb_contribution == 80)

severity_total_90 <- severity_total %>% 
  filter(nb_contribution == 90)

severity_total_90 <- severity_total %>% 
  filter(nb_contribution == 100)

severity_total %>%
  group_by(nb_contribution) %>%
  summarise(
    mean_claim = mean(cumulative_claim_paid, na.rm = TRUE),
    count = n()
  )

# Binarize nb_contribution: 100 vs all others
severity_claims <- severity_claims %>%
  mutate(nb_contribution_binary = ifelse(nb_contribution == 100, "100", "Other"))

ggplot(severity_claims, aes(x = nb_contribution_binary, y = cumulative_claim_paid)) +
  geom_boxplot() +
  labs(title = "Cumulative Claims by Contribution Level (Binarized)",
       x = "Contribution Level (Binarized)",
       y = "Cumulative Claims Paid") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,1000))

#
# Nb_excess
#

severity_claims <- severity_claims %>%
  mutate(nb_excess_FLAG = ifelse(nb_excess > 0, TRUE, FALSE))

ggplot(severity_claims, aes(x = nb_excess_FLAG, y = log(cumulative_claim_paid))) +
  geom_boxplot() +
  labs(title = "Cumulative Claims by Excess Contribution Flag",
       x = "Excess Contribution Flag",
       y = "Cumulative Claims Paid") +
  theme_minimal()

#
# Nb_address_type_adj
#

prop_claims_address <- severity_total %>%
  group_by(nb_address_type_adj) %>%
  summarise(
    cumulative_claims = sum(cumulative_claim_amount),
    .groups = 'drop'
  ) %>%
  mutate(
    proportion_claims = cumulative_claims / sum(cumulative_claims)
  )

prop_claims_address_monthly <- severity_total %>%
  group_by(UW_Date, nb_address_type_adj) %>%
  summarise(
    cumulative_claims = sum(cumulative_claim_amount),
    .groups = 'drop'
  ) %>%
  group_by(UW_Date) %>%
  mutate(
    proportion_claims = cumulative_claims / sum(cumulative_claims)
  )

ggplot(prop_claims_address, aes(x = reorder(nb_address_type_adj, -proportion_claims), y = proportion_claims)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Proportion of Claims by Address Type",
       x = "Address Type",
       y = "Proportion of Claims") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(prop_claims_address_monthly, aes(x = UW_Date, y = log(cumulative_claims), color = nb_address_type_adj)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Monthly Log Cumulative Claim Amount by Address Type",
    x = "Underwriting Date",
    y = "Cumulative Claim Amount",
    color = "Address Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(prop_claims_address_monthly, aes(x = UW_Date, y = proportion_claims, color = nb_address_type_adj)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Monthly Proportion of Claims by Address Type",
    x = "Underwriting Date",
    y = "Proportion of Claims",
    color = "Address Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#
# Person_dob
#

severity_total <- severity_total %>%
  mutate(person_dob = as.Date(person_dob)) %>%
  mutate(generation = cut(person_dob,
                          breaks = as.Date(c("1928-01-01", "1946-12-31", "1964-12-31", "1980-12-31", "1996-12-31", "9999-12-31")),
                          labels = c("Silent Generation", "Baby Boomers", "Generation X", "Millennials", "Generation Z"),
                          include.lowest = TRUE,
                          right = TRUE))


# Run analyses on the new generation variable
ggplot(severity_total, aes(x = generation, y = log(cumulative_claim_paid))) +
  geom_boxplot() +
  labs(title = "Cumulative Claims by Generation",
       x = "Generation",
       y = "Cumulative Claims Paid") +
  theme_minimal()

#
# Pet_age_years
#

ggplot(severity_total, aes(x = pet_age_years, y = log(cumulative_claim_paid))) +
  geom_boxplot() +
  labs(title = "Cumulative Claims by Pet Age (Years)", x = "Pet Age (Years)", y = "Cumulative Claims Paid") +
  theme_minimal()


#
# Owner_age_years
#

ggplot(severity_total, aes(x = owner_age_years, y = log(cumulative_claim_paid))) +
  geom_point() +
  labs(title = "Cumulative Claims by Owner Age", x = "Owner Age (Years)", y = "Cumulative Claims Paid") +
  theme_minimal()

#
# Nb_breed_trait


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

# Join breed mapping to the main data frame
severity_claims <- severity_claims %>%
  left_join(breed_mapping, by = "nb_breed_trait")

# Analyze cumulative claims by breed group
ggplot(severity_claims, aes(x = breed_group, y = log(cumulative_claim_paid))) +
  geom_boxplot() +
  labs(title = "Cumulative Claims by Breed Group",
       x = "Breed Group",
       y = "Cumulative Claims Paid") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(severity_claims, aes(x = breed_group, y = log(cumulative_claim_amount))) +
  geom_violin(fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Log-Transformed Cumulative Claim Amount by Breed Type",
    x = "Breed Type",
    y = "Log(Cumulative Claim Amount + 1)"
  ) +
  theme_minimal()

#
# Is_multi_pet_plan
#

ggplot(severity_total, aes(x = as.factor(is_multi_pet_plan), y = log(cumulative_claim_paid))) +
  geom_boxplot() +
  labs(title = "Cumulative Claims by Multi-Pet Plan Status",
       x = "Multi-Pet Plan",
       y = "Cumulative Claims Paid") +
  theme_minimal()


#
# Multiple_claims
#

ggplot(severity_total, aes(x = as.factor(multiple_claims), y = log(cumulative_claim_paid))) +
  geom_boxplot() +
  labs(title = "Cumulative Claims by Single vs. Multiple Claims",
       x = "Multiple Claims",
       y = "Cumulative Claims Paid") +
  theme_minimal()


#
# Nb_breed_trait and Nb_address_type
#
severity_total <- severity_total %>%
  left_join(breed_mapping, by = "nb_breed_trait")

heatmap_data <- severity_total %>%
  group_by(breed_group, nb_address_type_adj) %>%
  summarise(mean_claim = mean(cumulative_claim_paid, na.rm = TRUE), .groups = 'drop')

# Generate the heatmap
ggplot(heatmap_data, aes(x = breed_group, y = nb_address_type_adj)) + 
  geom_tile(aes(fill = mean_claim), color = "white") + 
  scale_fill_gradient(low = "blue", high = "red", name = "Mean Cumulative Claim Amount") + 
  labs(title = "Heatmap of Mean Cumulative Claims by Breed Trait and Address Type",
       x = "Breed Trait",
       y = "Address Type") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#
# Average v Total Monthly Claim over time
#

monthly_data <- severity_claims %>%
  mutate(Month = floor_date(UW_Date, "month")) %>%
  group_by(Month) %>%
  summarise(
    avg_cumulative_claim = mean(cumulative_claim_amount),
    total_cumulative_claim = sum(cumulative_claim_amount)
  )

monthly_data$avg_cumulative_claim
monthly_data$total_cumulative_claim

scale_factor <- max(monthly_data$total_cumulative_claim) / 1000

ggplot(monthly_data, aes(x = Month)) +
  geom_line(aes(y = avg_cumulative_claim, color = "Average Cumulative Claim"), size = 1.2) +
  geom_line(aes(y = total_cumulative_claim / scale_factor, color = "Total Cumulative Claim"), size = 1.2, linetype = "dashed") +
  scale_y_continuous(
    name = "Average Cumulative Claim",
    limits = c(0, 1000),
    breaks = seq(0, 1000, by = 200),
    sec.axis = sec_axis(~ . * scale_factor, name = "Total Cumulative Claim", 
                        breaks = seq(70000, 400000, by = 50000),
                        labels = scales::comma)
  ) +
  scale_color_manual(values = c("Average Cumulative Claim" = "blue", "Total Cumulative Claim" = "red")) +
  labs(
    title = "Monthly Average Claim and Total Monthly Claim Amount",
    x = "UW Date (Month)",
    y = "Average Cumulative Claim Amount",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    legend.position = "top"
  )

#
# Pet age and Owner age
#
heatmap_ownerage_petage <- severity_claims %>% 
  mutate(owner_age_group = cut(owner_age_years, breaks = seq(15, 85, by = 5), include.lowest = TRUE)) %>%
  group_by(owner_age_group, pet_age_years) %>% 
  summarise(mean_claim = mean(cumulative_claim_paid, na.rm = TRUE), .groups = 'drop')

ggplot(heatmap_ownerage_petage, aes(x = owner_age_group, y = pet_age_years)) + 
  geom_tile(aes(fill = mean_claim), color = "white") + 
  scale_fill_gradient(low = "blue", high = "red", name = "Cumulative Claim Amount") + 
  labs(title = "Heatmap of Cumulative Claims by Owner Age and Pet Age", 
       x = "Owner Age Group (Years)", 
       y = "Pet Age (Years)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#
# Owner Age, NB_breed_trait
#
heatmap_ownerage_breedtrait <- severity_claims %>% 
  mutate(owner_age_group = cut(owner_age_years, breaks = seq(15, 85, by = 5), include.lowest = TRUE)) %>%
  group_by(owner_age_group, nb_breed_type) %>% 
  summarise(mean_claim = mean(cumulative_claim_paid, na.rm = TRUE), .groups = 'drop')

ggplot(heatmap_ownerage_breedtrait, aes(x = owner_age_group, y = nb_breed_type)) + 
  geom_tile(aes(fill = mean_claim), color = "white") + 
  scale_fill_gradient(low = "blue", high = "red", name = "Cumulative Claim Amount") + 
  labs(title = "Heatmap of Cumulative Claims by Owner Age and Breed Traits", 
       x = "Owner Age Group (Years)", 
       y = "Breed Traits Group") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#
# NB_Breed_Trait v Pet Age Months
#

heatmap_breed_age <- severity_total %>%
  filter(cumulative_claim_paid < 20000) %>%
  group_by(pet_age_years, nb_breed_type) %>%
  summarise(mean_claim = mean(cumulative_claim_paid, na.rm = TRUE), .groups = 'drop')

ggplot(heatmap_breed_age, aes(x = pet_age_years, y = nb_breed_type)) + 
  geom_tile(aes(fill = mean_claim), color = "white") + 
  scale_fill_gradient(low = "blue", high = "red", name = "Cumulative Claim Amount") + 
  labs(title = "Heatmap of Cumulative Claims by Pet Age (Years) and Breed Traits",
       x = "Pet Age (Years)",
       y = "Breed Traits Group") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#
# Owner Age, Excess
#
heatmap_ownerage_excess <- severity_claims %>% 
  mutate(owner_age_group = cut(owner_age_years, breaks = seq(15, 85, by = 5), include.lowest = TRUE)) %>%
  group_by(owner_age_group, nb_contribution) %>% 
  summarise(mean_claim = mean(cumulative_claim_paid, na.rm = TRUE), .groups = 'drop')

ggplot(heatmap_ownerage_excess, aes(x = owner_age_group, y = nb_contribution)) + 
  geom_tile(aes(fill = mean_claim), color = "white") + 
  scale_fill_gradient(low = "blue", high = "red", name = "Cumulative Claim Amount") + 
  labs(title = "Heatmap of Cumulative Claims by Owner Age and Excess", 
       x = "Owner Age Group (Years)", 
       y = "Excess") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#
# Random Forest Variable Importance Measure
#

library(randomForest)
predictor_vars = c("pet_gender", "pet_de_sexed", "pet_de_sexed_age", "pet_is_switcher", "nb_policy_first_inception_date", "pet_age_months", "nb_contribution", "nb_excess", "nb_address_type_adj", "nb_suburb", "nb_postcode", "nb_state", "person_dob", "nb_contribution_excess", "pet_age_years", "owner_age_years", "nb_number_of_breeds", "nb_average_breed_size", "nb_breed_type", "nb_breed_trait", "nb_breed_name_unique", "nb_breed_name_unique_concat", "is_multi_pet_plan", "lead_date_day", "quote_date", "quote_time_group")

# Random forest to find important variables
rf_model <- randomForest(cumulative_claim_paid ~ pet_gender + pet_de_sexed + pet_de_sexed_age + 
                           pet_is_switcher + nb_policy_first_inception_date + pet_age_months + 
                           nb_contribution + nb_excess + nb_address_type_adj + 
                           nb_postcode + nb_state + person_dob + nb_contribution_excess + 
                           pet_age_years + owner_age_years + nb_number_of_breeds + 
                           nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan,
                         data = severity_claims, importance = TRUE, ntree = 500)

# Plot variable importance
importance_matrix <- importance(rf_model)

# Convert to a data frame and arrange by importance
importance_df <- as.data.frame(importance_matrix)
importance_df_sorted <- importance_df %>%
  arrange(desc(`%IncMSE`))
importance_df_sorted

#################
# EDA Frequency #
#################

#############################################

####################
# Model Selection #
###################
