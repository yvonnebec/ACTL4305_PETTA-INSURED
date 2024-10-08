#Install all necessary packages



#load all packages
library(tidyverse)
library(tibble)
library(lubridate)
library(randomForest)

#set working directory
setwd("C:/Users/luori/OneDrive/Desktop/2024/Semester 3/Assignment/Assignment Data")

#clear environment
rm(list=ls())

# Import data sets
sample_price <- read.csv("Sample_price_output_file.csv")
claims_data <- read.csv("UNSW_claims_data.csv")
earned_data <- read.csv("UNSW_earned_data_adjusted_Sep27.csv")
severity_dataset <- read.csv("UNSW_claims_data_tagged.csv")

abc <- severity_total

#Duplicate Claim_id
Claims_id <- abc %>%
  group_by(claim_id) %>%
  summarise(count = n()) %>%
  filter(count > 1)

#140 claims have been split for multiple records,
#i.e. split for mutiple records such as different illness


summary(abc)
str(abc)

#Need to Covert variables to factors

###-------------------CHRONIC ISSUES--------------------------------###

#Concatenate condition_category
# Same Pet, Same condition different months for signs of chronic illness

test <- abc %>%
  mutate(condition_exposure = paste(condition_category, exposure_id, sep = "_"))

test <- test %>%
  group_by(condition_exposure) %>%
  summarise(count = n()) %>%
  filter(count > 1)

test <- test %>%
  filter(count > 5)

ggplot(test, aes(x = count)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Counts by repeat condition claim",
       x = "Count",
       y = "Frequency") +
  theme_minimal()

#Overwhelming is other which might not be sign of chronic illness
#Will filter and check if premiums have been increased but from
#guest lecture, 

### Check for discount with multi pet plan


#######
# EDA #
#######

# Age in months
ggplot(severity_total, aes(x = pet_age_months)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Pet Age",
       x = "Pet Age (months)",
       y = "Frequency") +
  theme_minimal()

# Check with claims
ggplot(severity_total, aes(x = num_claims, y = pet_age_months)) +
  geom_violin(color = "lightblue") +
  labs(title = "Number of Pets vs. Number of Claims",
       x = "Number of calims",
       y = "Number of Claims") +
  theme_minimal()

### more pets younger - most claims younger need to try to find casual effects

### move away from frequency and look at severity
ggplot(severity_total, aes(x = pet_age_months, y = cumulative_claim_paid)) +
  geom_point(color = "lightblue") +
  labs(title = "Pet age vs severity",
       x = "Pet age",
       y = "Severity") +
  theme_minimal()

###-----------------Claim Frequency----------------###

# OUTLIERS - extremely high claim count - Extremely low Earned_Units
Data <- severity_total


# Could filter out for non-claims
Data <- severity_total %>% filter(num_claims > 0)

# Histogram of claim frequency
ggplot(Data, aes(x = claim_frequency)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Claim Frequency", x = "Claim Frequency", y = "Count")

# Scatter plot of claim frequency vs. pet age in years
ggplot(Data, aes(x = pet_age_years, y = claim_frequency)) +
  geom_point(alpha = 0.5) +
  labs(title = "Claim Frequency vs. Pet Age", x = "Pet Age (Years)", y = "Claim Frequency")


##############################
# Exposure Variable Analysis #
##############################

# Calculate claims frequency rate
Data <- Data %>%
  mutate(claims_frequency_rate = claim_frequency / earned_units)

#Outliers due to incredibly low earn_unites
view(Data %>% filter(claims_frequency_rate > 99))

Data %>% filter(claims_frequency_rate < 101) %>% nrow()


###NA's arise when earn_units is zero, no exposure
sum(is.na(Data$claims_frequency_rate))

Data <- Data %>% filter(claims_frequency_rate < 1000)


# Scatter plot of claims frequency rate vs. earned units
ggplot(Data, aes(x = earned_units, y = claims_frequency_rate)) +
  geom_point(alpha = 0.5) +
  labs(title = "Claims Frequency Rate vs. Earned Units", x = "Earned Units", y = "Claims Frequency Rate")

# Correlation analysis
cor(Data$earned_units, Data$claims_frequency_rate)




#######################
# Pet Characteristics #
#######################

###Need to include proportions

ggplot(Data, aes(x = pet_gender, y = claim_frequency)) + 
  geom_boxplot() + 
  labs(title = "Claim Frequency by Pet Gender", x = "Gender", y = "Claim Frequency")

# Calculate average claim frequency by pet gender
average_claim_frequency <- Data %>%
  group_by(pet_gender) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Log transform claim frequency
data_clean <- data_clean %>%
  mutate(log_claim_frequency = log(claim_frequency + 1))

# Boxplot for log-transformed claim frequency by gender
ggplot(data_clean, aes(x = pet_gender, y = log_claim_frequency, fill = pet_gender)) +
  geom_violin() +
  labs(title = "Log-Transformed Claim Frequency by Pet Gender", x = "Gender", y = "Log(Claim Frequency + 1)") +
  theme_minimal()


# Print the result
print(average_claim_frequency)

ggplot(Data, aes(x = pet_de_sexed, y = claim_frequency)) +
  geom_boxplot() +
  labs(title = "Claim Frequency by De-sexed Status", x = "De-sexed Status", y = "Claim Frequency")


# Calculate average claim frequency by pet gender
average_claim_frequency <- Data %>%
  group_by(pet_de_sexed) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))


# Reorder factor levels of pet_age_group
Data$pet_age_years <- factor(Data$pet_age_years, 
                             levels = c("0-6 months", "7-12 months", "1 years", "2 years", 
                                        "3 years", "4 years", "5 years", "6 years", 
                                        "7 years", "8 years", "9 years", "10 years"))


# Calculate claim rate by age group
claim_rate_by_age <- Data %>%
  group_by(pet_age_years) %>%
  summarise(
    total_claims = sum(num_claims, na.rm = TRUE),
    num_pets = n(),
    claim_rate = total_claims / num_pets
  )

str(severity_dataset)

ggplot(claim_rate_by_age, aes(x = pet_age_years, y = claim_rate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Claim Rate by Pet Age Group", x = "Pet Age Group", y = "Claim Rate (Claims per Pet)") +
  theme_minimal()

# standardised claim rate
# Calculate total exposure by age group (assuming you have an exposure variable 'earned_units')
claim_rate_by_age <- Data %>%
  group_by(pet_age_years) %>%
  summarise(
    total_claims = sum(claim_frequency, na.rm = TRUE),
    total_exposure = sum(earned_units, na.rm = TRUE),
    standardized_claim_rate = total_claims / total_exposure
  )

sum(data_clean$claim_frequency, na.rm = TRUE)
sum(data_clean$earned_units, na.rm = TRUE)

print(claim_rate_by_age)



str(data_clean$pet_age_group)

# Visualize standardized claim rate
ggplot(claim_rate_by_age, aes(x = pet_age_years, y = standardized_claim_rate)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Standardized Claim Rate by Pet Age Group", x = "Pet Age Group", y = "Standardized Claim Rate") +
  theme_minimal()


##########################
# Policy Characteristics #
##########################


###FILTER FOR NON CLAIMS
data_clean <- Data %>%
  mutate(policy_age = as.numeric(difftime(Sys.Date(), nb_policy_first_inception_date, units = "days")) / 365) %>%
  filter(claim_frequency > 0 & claim_frequency <= 20)


summary(data_clean$owner_age_years)

ggplot(data_clean, aes(x = policy_age, y = claim_frequency)) +
  geom_point(alpha = 0.5) +
  labs(title = "Claim Frequency vs. Policy Age", x = "Policy Age (Years)", y = "Claim Frequency")

ggplot(data_clean, aes(x = policy_age, y = claim_frequency)) +
  geom_bin2d(bins = 30) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Claim Frequency Density vs. Policy Age", x = "Policy Age (Years)", y = "Claim Frequency")



### LOOK at Contribution
sum(is.na(Data$pet_de_sexed_age))


#Excess is continuous
ggplot(data_clean, aes(x = nb_excess, y = claim_frequency)) +
  geom_boxplot() +
  labs(title = "Claim Frequency by Excess Amount", x = "Excess Amount", y = "Claim Frequency")


ggplot(data_clean, aes(x = factor(nb_excess), y = claim_frequency)) +
  geom_boxplot() +
  labs(title = "Claim Frequency by Excess Amount", x = "Excess Amount", y = "Claim Frequency")



#########################
# Owner Characteristics #
#########################

### Younger owners tend to have greater frequency of claims

ggplot(severity_total, aes(x = owner_age_years)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of owner ages",
       x = "Owner Age",
       y = "Frequency") +
  theme_minimal()

###change
proportion_table2 <- severity_total %>%
  # Group num_claims > 0 together
  mutate(num_claims_grouped = ifelse(num_claims > 0, "1+", as.character(num_claims))) %>%
  group_by(owner_age_years, num_claims_grouped) %>%
  summarise(claim_count = n()) %>%
  ungroup() %>%
  # Calculate the total count of pets per age in years
  group_by(owner_age_years) %>%
  mutate(total_owners = sum(claim_count)) %>%
  # Calculate the proportion of claims for each pet age in years
  mutate(proportion = claim_count / total_owners) %>%
  ungroup() %>%
  filter(num_claims_grouped > 0) %>%
  # Arrange the data by pet_age_years
  arrange(owner_age_years)

ggplot(proportion_table2, aes(x = owner_age_years, y = proportion)) +
  geom_line(color = "lightblue") +
  geom_point() +
  labs(
    x = "Owner Age (Years)",
    y = "Proportion of Claims",
    title = "Proportion of Claims by Owner Age"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(proportion_table2$owner_age_years)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## few NA values to consider if we use owner age as a predictor variable

ggplot(data_clean, aes(x = owner_age_years, y = claim_frequency)) +
  geom_point(alpha = 0.5) +
  labs(title = "Claim Frequency vs. Owner Age", x = "Owner Age (Years)", y = "Claim Frequency")

data_clean <- data_clean %>%
  filter(!is.na(owner_age_years)) %>%
  mutate(owner_age_group = cut(owner_age_years, breaks = c(0, 25, 35, 45, 55, 65, Inf), labels = c("0-25", "25-35", "35-45", "45-55", "55-65", "65+")))

ggplot(data_clean, aes(x = owner_age_group, y = claim_frequency)) +
  geom_boxplot() +
  labs(title = "Claim Frequency by Owner Age Group", x = "Owner Age Group", y = "Claim Frequency")



# Create a violin plot
ggplot(data_clean, aes(x = owner_age_group, y = claim_frequency)) +
  geom_violin(trim = FALSE, fill = "skyblue", alpha = 0.7) +
  labs(title = "Claim Frequency by Owner Age Group", x = "Owner Age Group", y = "Claim Frequency") +
  theme_minimal()


###log transform
# Create a new variable with log-transformed claim frequency
data_clean <- data_clean %>%
  mutate(log_claim_frequency = log(claim_frequency + 1))

ggplot(data_clean, aes(x = owner_age_group, y = log_claim_frequency)) +
  geom_violin(trim = FALSE, fill = "lightgreen", alpha = 0.7) +
  labs(title = "Log-Transformed Claim Frequency by Owner Age Group", x = "Owner Age Group", y = "Claim Frequency") +
  theme_minimal()

view(data_clean %>% filter(claim_frequency < 0))

###Person DOB - might be correlated to onwer age

##################################
# Breed and Plan Characteristics #
##################################

# Segmentation by breed
Breed <- severity_total %>%
  filter(!is.na(claim_frequency)) %>% 
  group_by(nb_breed_type) %>% summarise(mean_freq = mean(claim_frequency))


ggplot(data_clean, aes(x = nb_breed_type, y = claim_frequency)) +
  geom_boxplot() +
  labs(title = "Claim Frequency by Breed Type", x = "Breed Type", y = "Claim Frequency")

ggplot(data_clean, aes(x = nb_breed_type, y = claim_frequency)) +
  geom_violin(trim = FALSE, fill = "skyblue", alpha = 0.7) +
  labs(title = "Claim Frequency by Breed Type", x = "Breed Type", y = "Claim Frequency") +
  theme_minimal()

# Violin plot with log-transformed claim frequency
ggplot(data_clean, aes(x = nb_breed_type, y = log_claim_frequency)) +
  geom_violin(trim = FALSE, fill = "skyblue", alpha = 0.7) +
  labs(title = "Log-Transformed Claim Frequency by Breed Type", x = "Breed Type", y = "Log(Claim Frequency + 1)") +
  theme_minimal()


###Include breed trait and unique name
ggplot(data_clean, aes(x = nb_breed_trait, y = log_claim_frequency)) +
  geom_boxplot() +
  labs(title = "Claim Frequency by Breed Trait", x = "Breed Trait", y = "Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotates x-axis labels for readability

# Calculate average claim frequency by breed trait
avg_claim_frequency_breed <- Data %>%
  group_by(nb_breed_trait) %>%
  summarise(mean_claim_frequency = mean(claim_frequency, na.rm = TRUE)) %>%
  arrange(desc(mean_claim_frequency))

print(avg_claim_frequency_breed)

ggplot(avg_claim_frequency_breed, aes(x = reorder(nb_breed_trait, -mean_claim_frequency), y = mean_claim_frequency)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Average Claim Frequency by Breed Trait", x = "Breed Trait", y = "Mean Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##fix continuous
ggplot(data_clean, aes(x = nb_average_breed_size, y = claim_frequency)) +
  geom_boxplot() +
  labs(title = "Claim Frequency by Breed Size", x = "Average Breed Size", y = "Claim Frequency")

#MUlTIPLAN

# Boxplot for claim frequency by multi-pet plan status
ggplot(data_clean, aes(x = is_multi_pet_plan, y = log_claim_frequency, fill = is_multi_pet_plan)) +
  geom_boxplot() +
  labs(title = "Claim Frequency by Multi-Pet Plan Status", x = "Multi-Pet Plan", y = "Claim Frequency") +
  theme_minimal()

# Calculate summary statistics for claim frequency by multi-pet plan status
summary_table <- Data %>%
  group_by(is_multi_pet_plan) %>%
  summarise(
    total_policies = n(),
    total_claims = sum(claim_frequency, na.rm = TRUE),
    avg_claim_frequency = mean(claim_frequency, na.rm = TRUE),
    median_claim_frequency = median(claim_frequency, na.rm = TRUE)
  )

print(summary_table)

# Bar plot for average claim frequency by multi-pet plan status
ggplot(summary_table, aes(x = is_multi_pet_plan, y = avg_claim_frequency, fill = is_multi_pet_plan)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Multi-Pet Plan Status", x = "Multi-Pet Plan", y = "Average Claim Frequency") +
  theme_minimal()


#########################
# Claim Characteristics #
#########################

#Num of unique conditions and mutuiple claims


################################
# Geographical Characteristics #
################################

ggplot(data_clean, aes(x = nb_state, y = claim_frequency)) +
  geom_boxplot() +
  labs(title = "Claim Frequency by State", x = "State", y = "Claim Frequency")

data_clean$nb_contribution <- factor(data_clean$nb_contribution)


ggplot(data_clean, aes(x = nb_contribution, y = claim_frequency)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Claim Frequency vs. Premium Contribution", x = "Premium Contribution", y = "Claim Frequency")



#################################
# Financial and Time dependence #
#################################


ggplot(data_clean, aes(x = nb_contribution, y = claim_frequency)) +
  geom_point(alpha = 0.5) +
  labs(title = "Claim Frequency vs. Premium Contribution", x = "Premium Contribution", y = "Claim Frequency")


ggplot(data_clean, aes(x = nb_policy_first_inception_date, y = claim_frequency)) +
  geom_line() +
  labs(title = "Claim Frequency Over Time", x = "Date", y = "Claim Frequency")

##################################
# Interactions between variables #
##################################





###--------------look at proportions




##Check how the claim_amount varies with num_claims to see the pattern between claim frequency and severity.




Filtered_total <- severity_total %>%
  filter(!num_claims == 0)

#times series
ggplot(Filtered_total, aes(x = nb_policy_first_inception_date, y = num_claims)) + geom_line() + theme_minimal()

view(severity_total %>%
  filter(nb_breed_trait == "bull", num_claims != 0)
)

