#################################################
#################################################
######### Branch file for Frequency EDA #########
#################################################
#################################################

ggthemr('fresh')


#set working directory
setwd("C:/Users/luori/OneDrive/Desktop/2024/Semester 3/Assignment/Assignment Data")

Data_full <- severity_total

Data_claims <- severity_total %>%
                filter(claim_frequency > 0 & claim_frequency <= 100)

######################
# Chronic Conditions #
######################


# Assuming severity_total is your data frame
Data_claims <- Data_claims %>%
  mutate(chronic = if_else(num_claims >= 3 & num_unique_conditions == 1, TRUE, FALSE))

# View the updated data frame
view(Data_claims)
# Calculate the proportion of pets with chronic conditions
mean(severity_total$chronic)

#only 0.2% not significant


#######
# EDA #
#######

# Age in months
ggplot(Data_full, aes(x = pet_age_months)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Pet Age",
       x = "Pet Age (months)",
       y = "Frequency") +
  theme_minimal()

ggplot(Data_claims, aes(x = pet_age_months)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Pet Age",
       x = "Pet Age (months)",
       y = "Frequency") +
  theme_minimal()

# Distribtion of claims
ggplot(Data_claims, aes(x = claim_frequency)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Claim Frequency", x = "Claim Frequency", y = "Count") +
  theme_minimal()

###heavily right skewed

###filter for outliers
Data_multiple <- Data_claims %>%
  filter(claim_frequency > 1 & claim_frequency < 50)

ggplot(Data_multiple, aes(x = claim_frequency)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Claim Frequency", x = "Claim Frequency", y = "Count") +
  theme_minimal()

#######################
# Pet Characteristics #
#######################

###----------------PET GENDER----------------###

# Calculate average claim frequency by pet gender
average_claim_frequency <- Data %>%
  group_by(pet_gender) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Create the barplot
ggplot(average_claim_frequency, aes(x = pet_gender, y = avg_claim_frequency, fill = pet_gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Pet Gender", x = "Pet Gender", y = "Average Claim Frequency") +
  theme_minimal() + theme(legend.position = "none") +
  theme(text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5)) 

ggplot(Data_multiple, aes(x = claim_frequency, fill = pet_gender)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.25, color = "lightblue") +
  labs(title = "Histograms of Claim Frequency by gender",
       x = "Claim Frequency", y = "Count", fill = "Multi-Pet Plan") +
  theme_minimal()

###----------------PET DESEX STATUS-------------###

# Calculate average claim frequency by pet desex status
average_claim_frequency_desex <- Data %>%
  group_by(pet_de_sexed) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Create the barplot
ggplot(average_claim_frequency_desex, aes(x = pet_de_sexed, y = avg_claim_frequency, fill = pet_de_sexed)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Pet desex status", x = "Pet desex status", y = "Average Claim Frequency") +
  theme_minimal() + theme(legend.position = "none") +
  theme(text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5)) 

###----------------COMBINED-----------------###

# Summarizing the data by is_multi_pet_plan and pet_gender
summary_data <- Data_claims %>%
  group_by(pet_de_sexed, pet_gender) %>%
  summarise(total_claim_frequency = sum(claim_frequency, na.rm = TRUE))

# Bar plot of claim frequency by multi-pet plan and pet gender
ggplot(summary_data, aes(x = as.factor(pet_de_sexed), y = total_claim_frequency, fill = pet_gender)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Bar Plot of Claim Frequency by Multi-Pet Plan and Pet Gender",
       x = "Multi-Pet Plan (True/False)", y = "Total Claim Frequency", fill = "Pet Gender") +
  theme_minimal()

### Add labels


# Summarizing the data by desexed status and pet_gender, calculating the average claim frequency
summary_data <- Data_full %>%
  group_by(pet_de_sexed, pet_gender) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Side-by-side bar plot of average claim frequency by desexed status and pet gender
ggplot(summary_data, aes(x = as.factor(pet_de_sexed), y = avg_claim_frequency, fill = pet_gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Claim Frequency by Desexed Status and Pet Gender",
       x = "Desexed Status (True/False)", y = "Average Claim Frequency", fill = "Pet Gender") +
  theme_minimal()


###----------------PET AGE------------------###

# Reorder factor levels of pet_age_group
Data$pet_age_years <- factor(Data_full$pet_age_years, 
                             levels = c("0-6 months", "7-12 months", "1 years", "2 years", 
                                        "3 years", "4 years", "5 years", "6 years", 
                                        "7 years", "8 years", "9 years", "10 years"))
# Summary Table
claim_freq_by_age <- Data_full %>%
  group_by(pet_age_years) %>%
  summarise(
    total_claims = sum(num_claims, na.rm = TRUE),
    num_pets = n(),
    avg_claim_frequency = mean(claim_frequency, na.rm = TRUE)
  )

ggplot(claim_rate_by_age, aes(x = pet_age_years, y = claim_rate)) +
  geom_bar(stat = "identity") +
  labs(title = "Claim freq by Pet Age Group", x = "Pet Age Group", y = "Claim freq") +
  theme_minimal()


########################
# Plan Characteristics #
########################

###-------------------MULTI-PLAN--------------------###

### check for multi plan
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

# Summarizing the data by is_multi_pet_plan and pet_gender, calculating the average claim frequency
summary_data <- Data_full %>%
  group_by(is_multi_pet_plan, pet_gender) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Side-by-side bar plot of average claim frequency by multi-pet plan and pet gender
ggplot(summary_data, aes(x = as.factor(is_multi_pet_plan), y = avg_claim_frequency, fill = pet_gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Claim Frequency by Multi-Pet Plan and Pet Gender",
       x = "Multi-Pet Plan (True/False)", y = "Average Claim Frequency", fill = "Pet Gender") +
  theme_minimal()

#########################
# Breed Characteristics #
#########################

# Summarizing the data by nb_breed_type, calculating the average claim frequency
summary_data <- Data_full %>%
  group_by(nb_breed_type) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Bar plot of average claim frequency by nb_breed_type
ggplot(summary_data, aes(x = nb_breed_type, y = avg_claim_frequency, fill = nb_breed_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Breed Type",
       x = "Breed Type", y = "Average Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###---------------BREED GROUP-----------------###

# Calculate average claim frequency by breed trait
avg_claim_frequency_breed <- Data_full %>%
  group_by(breed_group) %>%
  summarise(mean_claim_frequency = mean(claim_frequency, na.rm = TRUE)) %>%
  arrange(desc(mean_claim_frequency))

print(avg_claim_frequency_breed)

ggplot(avg_claim_frequency_breed, aes(x = reorder(breed_group, -mean_claim_frequency), y = mean_claim_frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Breed Trait", x = "Breed Trait", y = "Mean Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


################################
# Geographical Characteristics #
################################

ggplot(Data_multiple, aes(x = nb_state, y = claim_frequency)) +
  geom_boxplot() +
  labs(title = "Claim Frequency by State", x = "State", y = "Claim Frequency")

# Summarizing the data by nb_state, calculating the average claim frequency
summary_data <- Data_multiple %>%
  group_by(nb_state) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Bar plot of average claim frequency by state
ggplot(summary_data, aes(x = nb_state, y = avg_claim_frequency)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Average Claim Frequency by State", x = "State", y = "Average Claim Frequency") +
  theme_minimal()


### Feature Engineering

##########################
# Policy Characteristics #
##########################

###--------------------CONTRIBUTION--------------------------###

Data_full$nb_contribution <- factor(Data_full$nb_contribution)

# Summarizing the data by nb_contribution, calculating the average claim frequency
summary_data <- Data_full %>%
  group_by(nb_contribution) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Bar plot of average claim frequency by premium contribution
ggplot(summary_data, aes(x = nb_contribution, y = avg_claim_frequency)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Average Claim Frequency vs. Premium Contribution", 
       x = "Premium Contribution", y = "Average Claim Frequency") +
  theme_minimal()

# Greater coverage = greater chance of claim being made

###---------------EXCESS--------------###

# Summarizing the data by nb_excess, calculating the average claim frequency
summary_data <- Data_full %>%
  group_by(nb_excess) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Bar plot of average claim frequency by nb_excess
ggplot(summary_data, aes(x = nb_excess, y = avg_claim_frequency)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Average Claim Frequency vs. Excess Amount", 
       x = "Excess Amount", y = "Average Claim Frequency") +
  theme_minimal()


# Lower excess plans have a greater claim frequency


#########################
# Owner Characteristics #
#########################


# Creating bins for generations based on age ranges
Data_full <- Data_full %>%
  mutate(
    generation = cut(owner_age_years,
                     breaks = c(0, 24, 39, 54, 74, Inf),   # Define the age ranges
                     labels = c("Gen Z", "Millennials", "Gen X", "Baby Boomers", "Silent Generation"),  # Assign labels
                     right = FALSE)  # Whether the intervals are closed on the right
  )

# Summarizing the data by generation, calculating the average claim frequency
summary_data <- Data_full %>%
  drop_na(generation) %>%
  group_by(generation) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE)) 

summary_data

# Bar plot of average claim frequency by generation
ggplot(summary_data, aes(x = generation, y = avg_claim_frequency, fill = generation)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Generation", x = "Generation", y = "Average Claim Frequency") +
  theme_minimal()


###------------------- Address Type --------------------------###


# Summarizing the data by Address Type, calculating the average claim frequency
summary_data <- Data_full %>%
  drop_na(nb_address_type_adj) %>%
  group_by(nb_address_type_adj) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE)) 


# Bar plot of average claim frequency by generation
ggplot(summary_data, aes(x = nb_address_type_adj, y = avg_claim_frequency, fill = nb_address_type_adj)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Address Type", x = "Generation", y = "Average Claim Frequency") +
  theme_minimal()
