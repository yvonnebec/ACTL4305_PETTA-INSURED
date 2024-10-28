#################################################
#################################################
######### Branch file for Frequency EDA #########
#################################################
#################################################

install.packages("devtools")
library(devtools)
devtools::install_github('Mikata-Project/ggthemr')
library(ggthemr)


ggthemr('fresh')


#set working directory
setwd("C:/Users/luori/OneDrive/Desktop/2024/Semester 3/Assignment/Assignment Data")

Data_full <- read.csv("actl4305_final_df_v8.csv")

str(Data_full)
# severity_full

Data_claims <- Data_full %>%
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
mean(Data_claims$chronic)

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
average_claim_frequency <- Data_full %>%
  group_by(pet_gender) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Create the barplot
gender_plot <- ggplot(average_claim_frequency, aes(x = pet_gender, y = avg_claim_frequency, fill = pet_gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Pet Gender", x = "Pet Gender", y = "Average Claim Frequency") +
  theme_minimal() + theme(legend.position = "none") +
  theme(text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5)) 


###----------------PET DESEX STATUS-------------###

# Calculate average claim frequency by pet desex status
average_claim_frequency_desex <- Data_full %>%
  group_by(pet_de_sexed) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Create the barplot
Desexed_plot <- ggplot(average_claim_frequency_desex, aes(x = pet_de_sexed, y = avg_claim_frequency, fill = pet_de_sexed)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Pet desex status", x = "Pet desex status", y = "Average Claim Frequency") +
  theme_minimal() + theme(legend.position = "none") +
  theme(text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5)) 


###----------------PET AGE------------------###

Data_full$pet_age_years <- as.factor(Data_full$pet_age_years)
summary(Data_full$pet_age_years)


# Group "9 years" and "10 years" together into "9+ years" using ifelse
Data_full$pet_age_years <- ifelse(Data_full$pet_age_years %in% c("9 years", "10 years"), 
                                  "9+ years", 
                                  Data_full$pet_age_years)

# Reorder the factor levels again to include "9+ years"
Data_full$pet_age_years <- factor(Data_full$pet_age_years, 
                                  levels = c("0-6 months", "7-12 months", "1 years", "2 years", 
                                             "3 years", "4 years", "5 years", "6 years", 
                                             "7 years", "8 years", "9+ years"))
# Summary Table
claim_freq_by_age <- Data_full %>%
  group_by(pet_age_years) %>%
  summarise(
    total_claims = sum(num_claims, na.rm = TRUE),
    num_pets = n(),
    avg_claim_frequency = mean(claim_frequency, na.rm = TRUE)
  )

ggplot(claim_freq_by_age, aes(x = pet_age_years, y = avg_claim_frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Claim Frequency by Pet Age Group", x = "Pet Age Group", y = "Average Claim Frequency") +
  theme_minimal()

summary(Data_full$pet_age_years)

########################
# Plan Characteristics #
########################

###-------------------MULTI-PLAN--------------------###

### check for multi plan
# Calculate summary statistics for claim frequency by multi-pet plan status
summary_table <- Data_full %>%
  group_by(is_multi_pet_plan) %>%
  summarise(
    total_policies = n(),
    total_claims = sum(claim_frequency, na.rm = TRUE),
    avg_claim_frequency = mean(claim_frequency, na.rm = TRUE),
    median_claim_frequency = median(claim_frequency, na.rm = TRUE)
  )

print(summary_table)

# Bar plot for average claim frequency by multi-pet plan status
Multi_plan_plot <- ggplot(summary_table, aes(x = is_multi_pet_plan, y = avg_claim_frequency, fill = is_multi_pet_plan)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Multi-Pet Plan Status", x = "Multi-Pet Plan", y = "Average Claim Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 11)) +  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))


#########################
# Breed Characteristics #
#########################

# Summarizing the data by nb_breed_type, calculating the average claim frequency
summary_data <- Data_full %>%
  group_by(nb_breed_type) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Bar plot of average claim frequency by nb_breed_type
breedtype_plot <- ggplot(summary_data, aes(x = nb_breed_type, y = avg_claim_frequency, fill = nb_breed_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Breed Type",
       x = "Breed Type", y = "Average Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

breedtype_plot

###---------------BREED GROUP-----------------###

# Calculate average claim frequency by breed trait
avg_claim_frequency_breed <- Data_full %>%
  group_by(breed_group) %>%
  summarise(mean_claim_frequency = mean(claim_frequency, na.rm = TRUE)) %>%
  arrange(desc(mean_claim_frequency))

print(avg_claim_frequency_breed)

breedgroup_plot <- ggplot(avg_claim_frequency_breed, aes(x = reorder(breed_group, -mean_claim_frequency), y = mean_claim_frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Breed Trait", x = "Breed Trait", y = "Mean Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))


###-------------- BREED SIZE --------------------###

# nb_average_breed_size
summary(severity_total$nb_average_breed_size)

# Perform analysis for nb_average_breed_size with custom breaks for severity_total
Data_full <- Data_full %>%
  filter(!is.na(nb_average_breed_size), !is.na(claim_frequency)) %>%
  mutate(nb_average_breed_size_bands = cut(nb_average_breed_size, 
                                           breaks = c(1, 1.5, 2, 2.5, 3, 4),
                                           labels = c("1-1.5", "1.5-2", "2-2.5", "2.5-3", "3+"),
                                           include.lowest = TRUE))

#^^^ 2-2.5 abonormally high, is this maybe because of cross breeds change average to be non-whole number?
# could it be caused by some other correlated factor?

# Check the distribution of the bins
table(Data_full$nb_average_breed_size_bands)

# Summarize by average severity total for each bin
avg_bands_breed_size <- Data_full %>%
  group_by(nb_average_breed_size_bands) %>%
  summarise(mean_severity_total = mean(claim_frequency, na.rm = TRUE))

# Plot a bar graph of the average severity total by bins for nb_average_breed_size
breedsize_plot <- ggplot(avg_bands_breed_size, aes(x = nb_average_breed_size_bands, y = mean_severity_total)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Average Frequency Total by Breed Size Bands", 
       x = "Breed Size Bands", 
       y = "Average Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

breedsize_plot

###---------------- Number of Breeds -----------------###

# nb_number_of_breeds
summary(severity_total$nb_number_of_breeds)

# Summarize by average claim frequency for each number of breeds
avg_claim_frequency_breeds <- severity_total %>%
  filter(!is.na(nb_number_of_breeds), !is.na(claim_frequency)) %>%  # Remove NA values
  group_by(nb_number_of_breeds) %>%
  summarise(mean_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Plot a bar graph comparing nb_number_of_breeds for average claim frequency
no_breeds_plot <- ggplot(avg_claim_frequency_breeds, aes(x = as.factor(nb_number_of_breeds), y = mean_claim_frequency)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Average Claim Frequency by Number of Breeds", 
       x = "Number of Breeds", 
       y = "Average Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

################################
# Geographical Characteristics #
################################

# Summarizing the data by nb_state, calculating the average claim frequency
summary_data <- Data_full %>%
  group_by(nb_state) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Bar plot of average claim frequency by state
state_plot <- ggplot(summary_data, aes(x = nb_state, y = avg_claim_frequency)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Average Claim Frequency by State", x = "State", y = "Average Claim Frequency") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

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
contribution_plot <- ggplot(summary_data, aes(x = nb_contribution, y = avg_claim_frequency)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Average Claim Frequency vs. Premium Contribution", 
       x = "Premium Contribution", y = "Average Claim Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

contribution_plot

# Greater coverage = greater chance of claim being made

###---------------EXCESS--------------###

# Summarizing the data by nb_excess, calculating the average claim frequency
summary_data <- Data_full %>%
  group_by(nb_excess) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Bar plot of average claim frequency by nb_excess
excess_plot <- ggplot(summary_data, aes(x = nb_excess, y = avg_claim_frequency)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Average Claim Frequency vs. Excess Amount", 
       x = "Excess Amount", y = "Average Claim Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

excess_plot

# Lower excess plans have a greater claim frequency


#########################
# Owner Characteristics #
#########################

# Creating bins for generations based on age ranges
Data_full <- Data_full %>%
  mutate(
    generation = cut(owner_age_years,
                     breaks = c(0, 24, 39, 54, 74, Inf),
                     labels = c("Gen Z", "Millennials", "Gen X", "Baby Boomers", "Silent Generation"),
                     right = FALSE)  # Whether the intervals are closed on the right
  )

# Summarizing the data by generation, calculating the average claim frequency
summary_data <- Data_full %>%
  drop_na(generation) %>%
  group_by(generation) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE)) 

summary_data

# Bar plot of average claim frequency by generation
owner_age_plot <- ggplot(summary_data, aes(x = generation, y = avg_claim_frequency, fill = generation)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Generation", x = "Generation", y = "Average Claim Frequency") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))


###------------------- Address Type --------------------------###


# Summarizing the data by Address Type, calculating the average claim frequency
summary_data <- Data_full %>%
  drop_na(nb_address_type_adj) %>%
  group_by(nb_address_type_adj) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE)) 


# Bar plot of average claim frequency by address type
address_type_plot <- ggplot(summary_data, aes(x = nb_address_type_adj, y = avg_claim_frequency, fill = nb_address_type_adj)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Address Type", x = "Address Type", y = "Average Claim Frequency") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

address_type_plot

##########################
# Policy Characteristics #
##########################

###------------------- quote_date --------------------------###

# Summarizing the data by Quote time, calculating the average claim frequency
summary_data <- Data_full %>%
  drop_na(quote_time_group) %>%
  group_by(quote_time_group) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE)) 


# Bar plot of average claim frequency by quote time
quote_time_plot <- ggplot(summary_data, aes(x = quote_time_group, y = avg_claim_frequency, fill = quote_time_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency quote time", x = "Quote time", y = "Average Claim Frequency") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

quote_time_plot

###------------------- Policy First Inception --------------------------###

########DOESNT WORK###############

# Extract the date part and convert to Date type
Data_full <- Data_full %>%
  mutate(policy_inception_date = as.Date(substr(nb_policy_first_inception_date, 1, 10), format = "%d/%m/%Y"))

# Summarize the data by date (optional: calculating average claim frequency per date)
summary_data <- Data_full %>%
  group_by(policy_inception_date) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Plot the change in claim frequency over time
ggplot(summary_data, aes(x = policy_inception_date, y = avg_claim_frequency)) +
  geom_line(color = "blue", alpha = 0.5) +  # Keeps the original line for reference
  geom_smooth(method = "loess", color = "red", size = 1.2) +  # Adds a smoother line
  labs(title = "Smoothed Claim Frequency Over Time", 
       x = "Policy Inception Date", 
       y = "Average Claim Frequency") +
  theme_minimal()


# Summarizing the data by month
summary_data_monthly <- Data_full %>%
  mutate(month = format(policy_inception_date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Convert month to a proper date format for better ordering in the plot
summary_data_monthly <- summary_data_monthly %>%
  mutate(month = as.Date(paste0(month, "-01")))

# Create a bar plot of average claim frequency by month
ggplot(summary_data_monthly, aes(x = month, y = avg_claim_frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Month", 
       x = "Month", y = "Average Claim Frequency") +
  theme_minimal()

###------------------- Switcher --------------###

### Drop false as we don't know if switch or not

summary_data <- Data_full %>%
  drop_na(pet_is_switcher) %>%
  group_by(pet_is_switcher) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE)) 


# Bar plot of average claim frequency by switcher
Switcher_plot <- ggplot(summary_data, aes(x = pet_is_switcher, y = avg_claim_frequency, fill = pet_is_switcher)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by swicher", x = "Switched or not", y = "Average Claim Frequency") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

Switcher_plot

summary(Data_full$pet_is_switcher)
summary(Data_full$nb_breed_trait)


###################################
########### Graphs ################
###################################


# Summarizing the data by desexed status and pet_gender, calculating the average claim frequency
summary_data <- Data_full %>%
  group_by(pet_de_sexed, pet_gender) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Side-by-side bar plot of average claim frequency by desexed status and pet gender
Gemder_Desexed_plot <- ggplot(summary_data, aes(x = as.factor(pet_de_sexed), y = avg_claim_frequency, fill = pet_gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Claim Frequency by Desexed Status and Pet Gender",
       x = "Desexed Status (True/False)", y = "Average Claim Frequency", fill = "Pet Gender") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

### Males slightly higher claim frequency
### Non-desexed pets have higher overall claim frequency

# Summarizing the data by is_multi_pet_plan and pet_gender, calculating the average claim frequency
summary_data <- Data_full %>%
  group_by(is_multi_pet_plan, nb_address_type_adj) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Side-by-side bar plot of average claim frequency by multi-pet plan and pet gender
Address_plan_plot <- ggplot(summary_data, aes(x = nb_address_type_adj, y = avg_claim_frequency, fill = as.factor(is_multi_pet_plan))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Claim Frequency by Multi-Pet Plan and Address type",
       x = "Address Type", y = "Average Claim Frequency", fill = "Mutli Pet Plan") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

### Same trend where males are slighlty higher than average
### Non- multi plans have slightly higher claim freq


############################
### Proprotion of Claims ###
############################

### looks AT proPORTION OF ALL CLAIMS

# Calculate the proportion of claims and non-claims for each pet age group
claim_freq_by_age <- Data_full %>%
  group_by(pet_age_years) %>%
  summarise(
    total_claims = sum(num_claims, na.rm = TRUE),
    num_pets = n(),
    avg_claim_frequency = mean(claim_frequency, na.rm = TRUE)
  ) %>%
  mutate(proportion_claims = total_claims / sum(total_claims),  # Proportion of claims
         proportion_non_claims = 1 - proportion_claims)  # Remaining proportion as non-claims

# Reshape the data into long format for stacked bar plot
claim_freq_by_age_long <- claim_freq_by_age %>%
  select(pet_age_years, proportion_claims, proportion_non_claims) %>%
  pivot_longer(cols = c(proportion_claims, proportion_non_claims),
               names_to = "type",
               values_to = "proportion")

# Create the stacked bar plot
ggplot(claim_freq_by_age_long, aes(x = proportion, y = pet_age_years, fill = type)) +
  geom_bar(stat = "identity", color = "black") + 
  scale_fill_manual(values = c("proportion_claims" = "lightblue", "proportion_non_claims" = "grey")) +
  labs(title = "Proportion of Total Claims by Pet Age Group", 
       x = "Proportion of Claims", 
       y = "Pet Age Group") +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) + 
  coord_flip() 

### Looks at proportion of claims within each age group
### Overwhelming Majority of claims are within the 0-6month brackets

# Calculate the proportion of claims and non-claims for each pet age group
claim_freq_by_age <- Data_full %>%
  group_by(pet_age_years) %>%
  summarise(
    total_claims = sum(num_claims, na.rm = TRUE),
    num_pets = n(),
    proportion_claims = total_claims / num_pets 
  ) %>%
  mutate(proportion_non_claims = 1 - proportion_claims) 

# Reshape the data into long format for stacked bar plot
claim_freq_by_age_long <- claim_freq_by_age %>%
  select(pet_age_years, proportion_claims, proportion_non_claims) %>%
  pivot_longer(cols = c(proportion_claims, proportion_non_claims),
               names_to = "type",
               values_to = "proportion")

# Create the stacked bar plot
p <- ggplot(claim_freq_by_age_long, aes(x = pet_age_years, y = proportion, fill = type)) +
  geom_bar(stat = "identity", position = "stack", color = "black",  size = 0.8, width = 0.7) +  # Stacked bars with a black border
  scale_fill_manual(values = c("proportion_claims" = "lightblue", "proportion_non_claims" = "grey"),
                    labels = c("Claims", "Non-Claims")) +  # Custom colors and labels
  labs(title = "Proportion of Claims and Non-Claims by Pet Age Group", 
       x = "Pet Age Group", 
       y = "Proportion of Claimss") +
  scale_y_continuous() +  # Show y-axis in percentages
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

# Add rectangles around the specific bars: "0-6 months", "7 years", and "9+ years"
p + 
  annotate("rect", xmin = 0.47, xmax = 1.52, ymin = -0.02, ymax = 1.02, color = "darkgreen", fill = NA, size = 1.5) +  # "0-6 months"
  annotate("rect", xmin = 8.47, xmax = 9.52, ymin = -0.02, ymax = 1.02, color = "darkgreen", fill = NA, size = 1.5) +  # "7 years"
  annotate("rect", xmin = 10.47, xmax = 11.52, ymin = -0.02, ymax = 1.02, color = "darkgreen", fill = NA, size = 1.5)  # "9+ years"


### --------------------------- GENERATIONS ---------------------- ###

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
Generation_plot <-ggplot(summary_data, aes(x = generation, y = avg_claim_frequency, fill = generation)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Generation", x = "Generation", y = "Average Claim Frequency") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

Generation_plot

