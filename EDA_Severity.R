
#################
# Severity EDA
#################


#
# Import library
#
library(rpart)
library(tidyverse)
severity_total <- read.csv("severity_total.csv")
sample <- read.csv("Sample_price_output_file.csv")

#################
# Handling NA's #
#################
missing_percent <- colMeans(is.na(severity_total)) * 100
cols_to_impute <- names(missing_percent[missing_percent > 0 & missing_percent < 20])
cols_high_missing <- names(missing_percent[missing_percent >= 20])


# Low % Missing
# "person_dob"      "owner_age_years" "nb_breed_trait"  "breed_group"    

# High % Missing
# "pet_de_sexed_age"        "pet_is_switcher"         "claim_first_date"  ]
# "claim_last_date""cumulative_claim_amount" "tenure.y""average_claim_amount"  

# Replace NA values with "None"
severity_total$pet_de_sexed_age[is.na(severity_total$pet_de_sexed_age)] <- "None"
severity_total$pet_is_switcher[is.na(severity_total$pet_is_switcher)] <- "None"

# Owner Age and Owner dob
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
#severity_total <- impute_missing_values(severity_total, "person_dob")
severity_total$owner_age_years[is.na(severity_total$owner_age_years)] <- 
  as.numeric(difftime(reference_date, severity_total$person_dob[is.na(severity_total$owner_age_years)], units = "weeks")) / 52.25

# Nb_breed_trait NAs values with the mode
mode_value <- names(sort(table(severity_total[["nb_breed_trait"]]), decreasing = TRUE))[1]
severity_total[["nb_breed_trait"]][is.na(severity_total[["nb_breed_trait"]])] <- mode_value



#################
# Severity EDA  #
#################

#
# Set up Ggplot Theme
#
#install.packages("devtools")
#install.packages("usethis")
#devtools::install_github('Mikata-Project/ggthemr')
#library(ggthemr)
#ggthemr("fresh")
#library(scales)


### Claims
severity_claims <- severity_total %>% filter(average_claim_amount > 0)

###
### pet_gender
### 
### Obs: Males claim is marginally higher

ggplot(severity_claims, aes(x = pet_gender, y = average_claim_amount, fill = pet_gender)) +  
  stat_summary(fun = "mean", geom="bar") +
  labs(
    title = "Average Monthly Claim by Pet Gender",
    x = "Pet Gender",
    y = "Claim Amount",
    fill = "Pet Gender") +
  scale_fill_manual(values = c("male" = "steelblue", "female" = "peru"),
                    labels = c("male" = "Male", "female" = "Female")) +
  scale_x_discrete(labels = c("male" = "Male", "female" = "Female")) +
  theme_minimal()

###
### pet_de_sexed
### 
### Obs: No Desex leads to higher claim

ggplot(severity_claims, aes(x = pet_de_sexed, y = average_claim_amount, fill = pet_de_sexed)) + 
  stat_summary(fun = "mean", geom = "bar",) +
  labs(
    title = "Average Claim by De-sexed Status", 
    x = "De-sexed Status", 
    y = "Claim Amount", 
    fill = "De-sexed Status"
  ) + 
  scale_fill_manual(values = c("true" = "steelblue", "false" = "peru"),
                    labels = c("true" = "True", "false" = "False")) +
  scale_x_discrete(labels = c("true" = "True", "false" = "False")) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


###
### pet_de_sexed_age
###
### Obs: Older pet de sexed age leads to higher claims severity

severity_claims <- severity_claims %>%
  mutate(pet_de_sexed_age = recode(pet_de_sexed_age,
                                   "0-3 mo" = "0-3 months",
                                   "1-2 yr" = "1-2 years",
                                   "2+ yr" = "2+ years",
                                   "7-12 mo" = "7-12 months",
                                   "4-6 mo" = "4-6 months",
                                   "None" = "None",
                                   "Not Sure" = "Not Sure")) %>%
  mutate(pet_de_sexed_age = factor(pet_de_sexed_age, 
                                   levels = c("0-3 months", "4-6 months", "7-12 months", 
                                              "1-2 years", "2+ years", "Not Sure", "None")))


average_claim_by_age <- severity_claims %>%
  group_by(pet_de_sexed_age) %>%
  summarise(average_claim = mean(average_claim_amount, na.rm = TRUE))

custom_colors <- c("0-3 months" = "steelblue", 
                   "4-6 months" = "lightsteelblue", 
                   "7-12 months" = "darkslategray", 
                   "1-2 years" = "peru", 
                   "2+ years" = "saddlebrown", 
                   "Not Sure" = "tan", 
                   "None" = "gray")

# Plot the average claim for each age band
ggplot(average_claim_by_age, aes(x = pet_de_sexed_age, y = average_claim, fill = pet_de_sexed_age)) + 
  geom_bar(stat = "identity") +
  labs(title = "Average Claim by Pet De-sexed Age", 
       x = "Pet De-sexed Age", 
       y = "Claim Amount",
       fill = "De-sexed Age") + 
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(severity_claims, aes(x = pet_de_sexed_age, y = average_claim_amount, fill = pet_de_sexed_age)) + 
  geom_boxplot() +
  labs(title = "Average Claim by Pet De-sexed Age", 
       x = "Pet De-sexed Age", 
       y = "Claim Amount",
       fill = "De-sexed Age") + 
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0,500))

###
### pet_is_switcher
###
### Obs: Not a pet switcher has a higher claim and is a pet switcher is lower

ggplot(severity_claims, aes(x = pet_is_switcher, y = average_claim_amount, fill = pet_is_switcher)) + 
  stat_summary(fun = "mean", geom = "bar") +
  labs(title = "Average Claim by Pet Switcher Status", 
       x = "Pet Switcher Status", 
       y = "Claim Amount", 
       fill = "Switcher Status") + 
  scale_fill_manual(values = c("true" = "steelblue", "false" = "peru", "None" = "darkslategray"),
                    labels = c("true" = "True", "false" = "False")) +
                   
  scale_x_discrete(labels = c("true" = "True", "false" = "False")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

### 
### nb_policy_first_inception_date
###

severity_claims_by_month <- severity_claims %>% 
  mutate(nb_policy_first_inception_date = as.Date(nb_policy_first_inception_date)) %>% 
  mutate(month = floor_date(nb_policy_first_inception_date, "month")) %>% 
  group_by(month) %>%
  summarise(
    average_claim = mean(average_claim_amount, na.rm = TRUE),
    median_claim = median(average_claim_amount, na.rm = TRUE)
  )

ggplot(severity_claims_by_month, aes(x = month)) + 
  geom_line(aes(y = average_claim, color = "Average Claim"), size = 1) + 
  geom_line(aes(y = median_claim, color = "Median Claim"), size = 1, linetype = "dashed") +  
  labs(title = "Average and Median Claim Amount by Month", 
       x = "Month of Policy Inception", 
       y = "Claim Amount") + 
  scale_color_manual(name = "Claim Type", values = c("Average Claim" = "steelblue", "Median Claim" = "darkorange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)


### 
### pet_age_months
###
## Obs: 

severity_claims <- severity_claims %>%
  mutate(pet_age_year_group = cut(pet_age_months, 
                                  breaks = seq(0, max(pet_age_months, na.rm = TRUE), by = 12),
                                  labels = paste(seq(0, max(pet_age_months, na.rm = TRUE) - 12, by = 12),
                                                 seq(12, max(pet_age_months, na.rm = TRUE), by = 12), 
                                                 sep = "-"), 
                                  include.lowest = TRUE))


average_claim_by_age_group <- severity_claims %>%
  group_by(pet_age_year_group) %>%
  summarise(average_claim = mean(average_claim_amount, na.rm = TRUE))

ggplot(average_claim_by_age_group, aes(x = pet_age_year_group, y = average_claim, group = 1)) + 
  geom_line(color = "steelblue", size = 1) +  # Line graph
  geom_point(color = "steelblue", size = 2) +  # Add points to the line
  labs(title = "Average Claim Amount by Pet Age (Years)", 
       x = "Pet Age (Years)", 
       y = "Claim Amount") + 
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 
### nb_contribution
###
### Obs: 90 is highest median claim, 100 is highest mean claim

average_claim_by_contribution <- severity_claims %>%
  group_by(nb_contribution) %>%
  summarise(average_claim = mean(average_claim_amount, na.rm = TRUE))

ggplot(average_claim_by_contribution, aes(x = factor(nb_contribution), y = average_claim, fill = factor(nb_contribution))) + 
  geom_bar(stat = "identity") +
  labs(title = "Mean Average Claim by Contribution Level", 
       x = "Contribution Level", 
       y = "Claim Amount", 
       fill = "Contribution Level") + 
  scale_fill_manual(values = c("steelblue", "peru", "darkslategray")) +
  theme_minimal()

median_claim_by_contribution <- severity_claims %>%
  group_by(nb_contribution) %>%
  summarise(average_claim = median(average_claim_amount, na.rm = TRUE))

ggplot(median_claim_by_contribution, aes(x = factor(nb_contribution), y = average_claim, fill = factor(nb_contribution))) + 
  geom_bar(stat = "identity") +
  labs(title = "Median Claim by Contribution Level", 
       x = "Contribution Level", 
       y = "Median Claim Amount", 
       fill = "Contribution Level") + 
  scale_fill_manual(values = c("steelblue", "peru", "darkslategray")) +
  theme_minimal()


### 
### nb_excess
###

mean_excess_claim <- severity_claims %>% 
  group_by(nb_excess_FLAG) %>% 
  summarise(mean_claim_amount = mean(average_claim_amount))

ggplot(mean_excess_claim, aes(x = nb_excess_FLAG, y = mean_claim_amount, fill = nb_excess_FLAG)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim by Excess Status", 
       x = "Excess Status", 
       y = "Claim Amount", 
       fill = "Excess Status") +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "peru"),
                    labels = c("FALSE" = "No Excess", "TRUE" = "Excess")) +
  scale_x_discrete(labels = c("FALSE" = "No Excess", "TRUE" = "Excess")) + 
  theme_minimal()

ggplot(severity_claims, aes(x = nb_excess_FLAG, y = average_claim_amount, fill = nb_excess_FLAG)) + 
  geom_boxplot() +
  labs(title = "Average Claim by Excess Status", 
       x = "Excess Status", 
       y = "Claim Amount", 
       fill = "Excess Status") + 
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "peru"), 
                    labels = c("FALSE" = "No Excess", "TRUE" = "Excess")) + 
  scale_x_discrete(labels = c("FALSE" = "No Excess", "TRUE" = "Excess")) +
  scale_y_continuous(limits = c(0,400)) +
  theme_minimal()

### 
### nb_address_type_adj
###

mean_severity_address <- severity_claims %>% 
  group_by(nb_address_type_adj) %>% 
  summarise(mean_address = mean(average_claim_amount))

ggplot(mean_severity_address, aes(x = nb_address_type_adj, y = mean_address, fill = nb_address_type_adj)) +
  geom_bar(stat = "identity") +
  labs(x = "Address Type",
       y = "Claim Amount",
       fill = "Address Type") +
  scale_fill_manual(values = c("House" = "steelblue", "Apartment" = "peru")) + 
  theme_minimal()

median_severity_address <- severity_claims %>% 
  group_by(nb_address_type_adj) %>% 
  summarise(median_address = median(average_claim_amount))

ggplot(median_severity_address, aes(x = nb_address_type_adj, y = median_address, fill = nb_address_type_adj)) +
  geom_bar(stat = "identity") +
  labs(x = "Address Type",
       y = "Claim Amount",
       fill = "Address Type") +
  scale_fill_manual(values = c("House" = "steelblue", "Apartment" = "peru")) + 
  theme_minimal()
  
### 
### nb_state
###
### Obs: NSW highest mean claim but median claim is the lowest

mean_claim_by_state <- severity_claims %>%
  group_by(nb_state) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE))

ggplot(mean_claim_by_state, aes(x = nb_state, y = mean_claim_amount, fill = nb_state)) + 
  geom_bar(stat = "identity") +
  labs(title = "Average Claim by State", 
       x = "State", 
       y = "Claim Amount", 
       fill = "State") + 
  scale_fill_manual(values = c("steelblue", "peru", "lightsteelblue","coral", "darkslategray", "saddlebrown", "tan", "darkgray")) +  # Custom color palette
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_claims_by_state <- severity_claims %>%
  group_by(nb_state) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE),
            median_claim_amount = median(average_claim_amount, na.rm = TRUE)) %>%
  gather(key = "claim_type", value = "claim_amount", mean_claim_amount, median_claim_amount)

ggplot(combined_claims_by_state, aes(x = nb_state, y = claim_amount, fill = claim_type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean and Median Claim by State", 
       x = "State", 
       y = "Claim Amount", 
       fill = "Claim Type") + 
  scale_fill_manual(
    labels = c("mean_claim_amount" = "Mean Claim Amount",
               "median_claim_amount" = "Median Claim Amount"),
    values = c("mean_claim_amount" = "steelblue", "median_claim_amount" = "peru")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 
### person_dob
###
### Irrelevant since same as Owner Age

### 
### nb_contribution_excess
###

severity_claims <- severity_claims %>%
  mutate(contribution_excess_category = paste0(nb_contribution, ", ", nb_excess))

severity_claims <- severity_claims %>%
  mutate(contribution_excess_category = factor(contribution_excess_category,
                                               levels = c("80, 0", "80, 100", "80, 200",
                                                          "90, 0", "90, 100", "90, 200",
                                                          "100, 0", "100, 100", "100, 200")))

mean_claim_by_contribution_excess <- severity_claims %>%
  group_by(contribution_excess_category) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE))

ggplot(mean_claim_by_contribution_excess, aes(x = contribution_excess_category, y = mean_claim_amount, fill = contribution_excess_category)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Mean Claim Amount by Contribution and Excess", 
       x = "Contribution & Excess", 
       y = "Claim Amount") + 
  scale_fill_manual(values = c("steelblue", "peru", "lightsteelblue", "darkslategray", "coral", "saddlebrown", "tan", "darkgreen", "darkgray")) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 
### pet_age_years
###

severity_claims <- severity_claims %>%
  mutate(pet_age_years = factor(pet_age_years, 
                                levels = c("0-6 months", "7-12 months", 
                                           "1 years", "2 years", "3 years", "4 years", 
                                           "5 years", "6 years", "7 years", "8 years", "9 years")))

mean_claim_by_pet_age_years <- severity_claims %>%
  group_by(pet_age_years) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE))

ggplot(mean_claim_by_pet_age_years, aes(x = pet_age_years, y = mean_claim_amount, fill = pet_age_years)) + 
  geom_bar(stat = "identity") +
  labs(title = "Mean Claim Amount by Pet Age", 
       x = "Pet Age", 
       y = "Claim Amount", fill = "Pet Age") + 
  scale_fill_manual(values = c("steelblue", "peru", "lightsteelblue", 
                               "coral", "darkslategray", "saddlebrown", 
                               "tan", "darkorange", "darkgray", "skyblue", "forestgreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(severity_claims, aes(x = pet_age_years, y = average_claim_amount, fill = pet_age_years)) + 
  geom_boxplot() +
  labs(title = "Claim Amount by Pet Age", 
       x = "Pet Age", 
       y = "Claim Amount", fill = "Pet Age") + 
  scale_fill_manual(values = c("steelblue", "peru", "lightsteelblue", 
                               "coral", "darkslategray", "saddlebrown", 
                               "tan", "darkorange", "darkgray", "skyblue", "forestgreen")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0,800)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

claim_stats_by_age <- severity_claims %>%
  group_by(pet_age_years) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE),
            median_claim_amount = median(average_claim_amount, na.rm = TRUE))

ggplot(claim_stats_by_age, aes(x = pet_age_years, group = 1)) + 
  geom_line(aes(y = mean_claim_amount, color = "Mean"), size = 1.5) +
  geom_line(aes(y = median_claim_amount, color = "Median"), size = 1.5, linetype = "dashed") +
  labs(title = "Mean and Median Claim Amount by Pet Age (Years)", 
       x = "Pet Age (Years)", 
       y = "Claim Amount", 
       color = "Claim Type") + 
  scale_color_manual(values = c("Mean" = "steelblue", "Median" = "coral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 
### owner_age_years
###

claim_by_owner_age_groups <- severity_claims %>%
  mutate(owner_age_group_10 = cut(owner_age_years,
                                  breaks = c(0, 20, 30, 40, 50, 60, 70, 80, Inf),
                                  labels = c("Under 20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "Over 80"),
                                  right = FALSE)) %>%
  filter(!is.na(owner_age_group_10) & !is.na(average_claim_amount))

mean_claim_by_owner_age_groups <- claim_by_owner_age_groups %>%
  group_by(owner_age_group_10) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE))

ggplot(mean_claim_by_owner_age_groups, aes(x = owner_age_group_10, y = mean_claim_amount, fill = owner_age_group_10)) + 
  geom_bar(stat = "identity") +
  labs(title = "Mean Claim Amount by Owner Age Group (10-Year Bins)", 
       x = "Owner Age Group", 
       y = "Mean Claim Amount",
       fill = "Owner Age Group") + 
  scale_fill_manual(values = c("steelblue", "peru", "lightsteelblue", "darkslategray", "saddlebrown", "coral", "tan", "darkgray")) +  # Custom color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(claim_by_owner_age_groups, aes(x = owner_age_group_10, y = average_claim_amount, fill = owner_age_group_10)) + 
  geom_boxplot() + 
  labs(title = "Mean Claim Amount by Owner Age Group", 
       x = "Owner Age Group", 
       y = "Mean Claim Amount",
       fill = "Owner Age Group") + 
  scale_fill_manual(values = c("steelblue", "peru", "lightsteelblue", "darkslategray", "saddlebrown", "coral", "tan", "darkgray")) +  # Custom color palette
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  scale_y_continuous(limits = c(0,750))
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 
### nb_number_of_breeds
###

num_breeds_severity_claims <- severity_claims %>%
  mutate(nb_breed_group = ifelse(nb_number_of_breeds == 1, "1", "2+"))

claim_stats_by_breed_group <- num_breeds_severity_claims %>%
  group_by(nb_breed_group) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE),
            median_claim_amount = median(average_claim_amount, na.rm = TRUE)) %>%
  pivot_longer(cols = c(mean_claim_amount, median_claim_amount), 
               names_to = "claim_type", 
               values_to = "claim_amount")

ggplot(claim_stats_by_breed_group, aes(x = nb_breed_group, y = claim_amount, fill = claim_type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean and Median Claim Amount by Number of Breeds", 
       x = "Number of Breeds", 
       y = "Claim Amount", 
       fill = "Claim Type") + 
  scale_fill_manual(values = c("mean_claim_amount" = "steelblue", "median_claim_amount" = "peru"), 
                    labels = c("mean_claim_amount" = "Mean", "median_claim_amount" = "Median")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(num_breeds_severity_claims, aes(x = nb_breed_group, y = average_claim_amount, fill = nb_breed_group)) + 
  geom_boxplot() +
  labs(title = "Claim Amount by Number of Breeds", 
       x = "Number of Breeds", 
       y = "Claim Amount", 
       fill = "Number of Breeds") + 
  scale_fill_manual(values = c("1" = "steelblue", "2+" = "peru")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0,500)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### 
### nb_average_breed_size
###

severity_claims <- severity_claims %>%
  mutate(breed_size_category = case_when(
    nb_average_breed_size >= 1 & nb_average_breed_size < 2 ~ "Small",
    nb_average_breed_size >= 2 & nb_average_breed_size < 3 ~ "Medium",
    nb_average_breed_size >= 3 & nb_average_breed_size <= 4 ~ "Large",
    TRUE ~ "Unknown"
  )) %>%
  mutate(breed_size_category = factor(breed_size_category, levels = c("Small", "Medium", "Large", "Unknown")))

mean_claim_by_breed_size_category <- severity_claims %>%
  group_by(breed_size_category) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE))


ggplot(mean_claim_by_breed_size_category, aes(x = breed_size_category, y = mean_claim_amount, fill = breed_size_category)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Mean Claim Amount by Breed Size", 
       x = "Breed Size", 
       y = "Mean Claim Amount",
       fill = "Breed Size") + 
  scale_fill_manual(values = c("steelblue", "peru", "darkslategray")) +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

claim_stats_by_breed_size_category <- severity_claims %>%
  group_by(breed_size_category) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE),
            median_claim_amount = median(average_claim_amount, na.rm = TRUE)) %>%
  pivot_longer(cols = c(mean_claim_amount, median_claim_amount), 
               names_to = "claim_type", 
               values_to = "claim_amount")

ggplot(claim_stats_by_breed_size_category, aes(x = breed_size_category, y = claim_amount, fill = claim_type)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Mean and Median Claim Amount by Breed Size Category", 
       x = "Breed Size Category", 
       y = "Claim Amount", 
       fill = "Claim Type") + 
  scale_fill_manual(values = c("mean_claim_amount" = "steelblue", "median_claim_amount" = "peru"), 
                    labels = c("mean_claim_amount" = "Mean", "median_claim_amount" = "Median")) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 
### nb_breed_type
###

severity_claims <- severity_claims %>%
  mutate(nb_breed_type = recode(nb_breed_type, 
                                "cross" = "Cross", 
                                "designerbreed" = "Designer Breed", 
                                "purebred" = "Purebred",
                                .default = "Unnamed Cross"))

mean_claim_by_breed_type <- severity_claims %>%
  group_by(nb_breed_type) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE))

      
ggplot(mean_claim_by_breed_type, aes(x = nb_breed_type, y = mean_claim_amount, fill = nb_breed_type)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Mean Claim Amount by Breed Type", 
       x = "Breed Type", 
       y = "Mean Claim Amount", fill = "Breed Type") + 
  scale_fill_manual(values = c("steelblue", "peru", "lightsteelblue", "darkslategray", "saddlebrown", "coral", "tan", "darkgray")) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

claim_stats_by_breed_type <- severity_claims %>%
  group_by(nb_breed_type) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE),
            median_claim_amount = median(average_claim_amount, na.rm = TRUE)) %>%
  pivot_longer(cols = c(mean_claim_amount, median_claim_amount), 
               names_to = "claim_type", 
               values_to = "claim_amount")

### 
### nb_breed_trait
###

mean_claim_by_breed_group <- severity_claims %>%
  group_by(breed_group) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE)) %>% 
  filter(!is.na(breed_group))

ggplot(mean_claim_by_breed_group, aes(x = breed_group, y = mean_claim_amount, fill = breed_group)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Mean Claim Amount by Breed Group", 
       x = "Breed Group", 
       y = "Claim Amount",
       fill = "Breed Group") + 
  scale_fill_manual(values = c("steelblue", "peru", "lightsteelblue", "darkslategray", "saddlebrown", "coral", "tan", "darkgray")) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(axis.text.x = element_blank()) 

claim_stats_by_breed_group <- severity_claims %>%
  group_by(breed_group) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE),
            median_claim_amount = median(average_claim_amount, na.rm = TRUE)) %>%
  pivot_longer(cols = c(mean_claim_amount, median_claim_amount), 
               names_to = "claim_type", 
               values_to = "claim_amount") %>% 
  filter(!is.na(breed_group))

severity_claims_box <- severity_claims %>% 
  filter(!is.na(breed_group))

ggplot(severity_claims_box, aes(x = breed_group,y = average_claim_amount, fill = breed_group)) + 
  geom_boxplot() +
  labs(title = "Distribution of Claim Amounts by Breed Group", 
       x = "Breed Group", 
       y = "Claim Amount") + 
  scale_fill_manual(values = c("steelblue", "peru", "lightsteelblue", "darkslategray", "saddlebrown", "coral", "tan", "darkgray")) + 
  theme_minimal() +
  scale_y_continuous(limits = c(0,400)) +
  theme(axis.text.x = element_blank()) 

### 
### nb_breed_name_unique
###
### Too many categories

### 
### nb_breed_name_unique_concat
###

### 
### is_multi_pet_plan
###
### Obs: Multi-Pet leads to higher claim amount for mean and median

multi_pet_severity <- severity_claims %>% 
  group_by(is_multi_pet_plan) %>% 
  summarise(mean_multi = mean(average_claim_amount))

ggplot(multi_pet_severity, aes(x = is_multi_pet_plan, y = mean_multi,
                               fill = is_multi_pet_plan)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim by Multi-Pet Plan Status",
       x = "Multi-Pet Plan Status", y = "Claim Amount",
       fill = "Multi-Pet Plan Status") +
  scale_fill_manual(values = c("steelblue", "peru"),
                    labels = c("false" = "Single Pet", "true" = "Multi-Pet")) +
  scale_x_discrete(labels = c("false" = "Single Pet", "true" = "Multi-Pet"))
  theme_minimal()

### 
### lead_date_day
###
### Obs: Significantly higher frequency of 0 lead date observations so
### not enough data on 1-5 and 5+ categories

severity_claims_lead_date <- severity_claims %>%
  mutate(lead_date_day_group = case_when(
    lead_date_day == 0 ~ "0",
    lead_date_day >= 1 & lead_date_day <= 5 ~ "1-5",
    lead_date_day > 5 ~ "5+"
  ))

severity_claims_lead_date <- severity_claims_lead_date %>%
  group_by(lead_date_day_group) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE))

ggplot(severity_claims_lead_date, aes(x = lead_date_day_group, y = mean_claim_amount, fill = lead_date_day_group)) + 
  geom_bar(stat = "identity") +
  labs(title = "Mean Claim Amount by Lead Date Day Group", 
       x = "Lead Date Day Group", 
       y = "Claim Amount") + 
  scale_fill_manual(values = c("0" = "steelblue", "1-5" = "peru", "5+" = "darkgray")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 
### quote_date
###

severity_claims <- severity_claims %>%
  mutate(quote_date = as.Date(quote_date))

severity_claims <- severity_claims %>%
  mutate(quote_month = floor_date(quote_date, "month"))

mean_claim_by_month <- severity_claims %>%
  group_by(quote_month) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE))

ggplot(mean_claim_by_month, aes(x = quote_month, y = mean_claim_amount)) + 
  geom_line(color = "steelblue", size = 1.2) + 
  geom_point(color = "steelblue", size = 3) + 
  labs(title = "Mean Claim Amount by Quote Month", 
       x = "Quote Month", 
       y = "Claim Amount") + 
  scale_y_continuous(labels = scales::comma) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###
### quote_time_group
###

claim_stats_by_time_group <- severity_claims %>%
  group_by(quote_time_group) %>%
  summarise(mean_claim_amount = mean(average_claim_amount, na.rm = TRUE),
            median_claim_amount = median(average_claim_amount, na.rm = TRUE)) %>%
  pivot_longer(cols = c(mean_claim_amount, median_claim_amount), 
               names_to = "claim_type", 
               values_to = "claim_amount")

ggplot(claim_stats_by_time_group, aes(x = quote_time_group, y = claim_amount, fill = claim_type)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Mean and Median Claim Amount by Quote Time Group", 
       x = "Quote Time Group", 
       y = "Claim Amount", 
       fill = "Claim Type") + 
  scale_fill_manual(values = c("mean_claim_amount" = "steelblue", "median_claim_amount" = "peru"), 
                    labels = c("mean_claim_amount" = "Mean", "median_claim_amount" = "Median")) + 
  scale_y_continuous(labels = scales::comma) + 
  scale_x_discrete(labels = c("Afternoon", "Evening", "Late Night", "Morning")) +
  theme_minimal()

###
### Median Taxable Income
###

severity_claims <- severity_claims %>% 
  rename(median_taxable_income = median_taxable_income.x) %>% 
  select(-median_taxable_income.y)

clean_incomes <- gsub(",", "", severity_claims$median_taxable_income)
severity_claims$median_taxable_income <- as.numeric(clean_incomes)

severity_claims <- severity_claims %>% 
  filter(!is.na(median_taxable_income), !is.na(average_claim_amount))

breaks <- seq(35, max(severity_claims$median_taxable_income / 1000, na.rm = TRUE) + 5, by = 5)
labels <- paste(seq(35, max(breaks) - 5, by = 5), seq(40, max(breaks), by = 5), sep = "-")

severity_claims <- severity_claims %>% 
  mutate(taxable_income_bin = cut(median_taxable_income / 1000,
                                  breaks = breaks,
                                  labels = labels,
                                  include.lowest = TRUE))

average_claim_by_income_bin <- severity_claims %>% 
  group_by(taxable_income_bin) %>% 
  summarise(average_claim = mean(average_claim_amount, na.rm = TRUE)) %>% 
  filter(!is.na(taxable_income_bin))

ggplot(average_claim_by_income_bin, aes(x = taxable_income_bin, y = average_claim, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Average Claim Amount by Median Taxable Income",
       x = "Median Taxable Income ('000)",
       y = "Claim Amount") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

severity_claims %>% group_by(taxable_income_bin) %>% summarise(m = median(average_claim_amount))

ggplot(severity_claims, aes(x = taxable_income_bin, y = average_claim_amount)) +
  geom_boxplot() +
  labs(title = "Average Claim Amount by Median Taxable Income",
       x = "Median Taxable Income ('000)",
       y = "Claim Amount") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  scale_y_continuous(limits = c(0,650))

ggplot(severity_claims, aes(x = taxable_income_bin, y = average_claim_amount)) + 
  geom_boxplot(fill = "steelblue", color = "steelblue", alpha = 0.5) +
  geom_line(data = average_claim_by_income_bin, 
            aes(x = taxable_income_bin, y = average_claim, group = 1, color = "Mean Claim"), 
            size = 1) +
  geom_point(data = average_claim_by_income_bin, 
             aes(x = taxable_income_bin, y = average_claim, color = "Mean Claim"), 
             size = 2) +
  scale_color_manual(name = "Legend", values = c("Mean Claim" = "darkred")) +
  labs(title = "Claim Amount by Median Taxable Income (Mean and Boxplot)",
       x = "Median Taxable Income ('000)",
       y = "Claim Amount") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1000)) + 
  theme_minimal()

###
### nb_postcode to SA Mapping
### SA Level 2, Level 3, Level 4
sa2 <- read.csv("POSTCODE_SA2.csv", na.rm=T)
sa3 <- read.csv("POSTCODE_SA3.csv", na.rm=T)
sa4 <- read.csv("POSTCODE_SA4.csv", na.rm=T)



### 
### nb_suburb
### 

###
### nb_postcode
###

##
### Interaction Terms
### 

## Breed x Age

### Dual Income Young People