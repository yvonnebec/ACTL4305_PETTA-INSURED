
# Main dataset
severity_total <- read.csv("severity_Conan.csv")

severity_total <- severity_total %>%
  mutate(nb_postcode = as.numeric(nb_postcode))

sum(is.na(severity_total$nb_postcode))

head(severity_total)
# Get datasets
family <- read.csv("Family.csv")
SA4 <- read.csv("postcode_SA4.csv")

view(family)
view(SA4)

summary(family)

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

summary(merged_data)


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
  mutate(POSTCODE = as.character(POSTCODE))

summary(result$avg_weighted_homeless_rate)

# Merging result with severity_total based on nb_postcode
severity_external <- severity_total %>%
  left_join(result, by = c("nb_postcode" = "POSTCODE"))

# View the first few rows of the merged dataset
head(severity_external)

view(result %>% filter(weighted_Averagehouseholdsize < 2))


# Exporting to a specific directory, e.g., Desktop (modify the path for your system)
write.csv(severity_external, "C:/Users/luori/OneDrive/Desktop/2024/Semester 3/Assignment/Assignment Data/Merged Data/severity_external.csv", row.names = FALSE)


######################
# Family & Community #
######################

###------------------Average Homelessness per 10,000 people ------------------###
# Create a histogram of the avg_weighted_homeless_rate_clean
ggplot(Data_family, aes(x = avg_weighted_homeless_rate_clean)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Average Weighted Homeless Rate", 
       x = "Average Weighted Homeless Rate", 
       y = "Frequency") +
  theme_minimal()

summary(severity_external$avg_weighted_homeless_rate)

# Create bins for avg_weighted_homeless_rate in intervals of 50, with the final bin being >200
severity_external <- severity_external %>%
  filter(!is.na(avg_weighted_homeless_rate)) %>%
  mutate(
    homeless_count_per10k = cut(
      as.numeric(avg_weighted_homeless_rate),
      breaks = c(0, 25, 50, 75, 100, Inf),  # Custom bins based on the provided statistics
      labels = c("0-25", "25-50", "51-75", "76-100", ">150"),
      right = FALSE  # Left-closed intervals
    )
  )

summary(Data_family$homeless_count_per10k)

# View the first few rows of the updated dataset
head(Data_family)

# Summarize the data by homeless_rate_bins and calculate average claims_frequency
summary_data <- Data_family %>%
  group_by(homeless_count_per10k) %>%
  summarise(avg_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Create a bar plot
ggplot(summary_data, aes(x = homeless_count_per10k, y = avg_claim_frequency, fill = homeless_count_per10k)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claims Frequency by Homeless Rate Bins (200 intervals)", 
       x = "Homeless Rate Bins (200 intervals)", 
       y = "Average Claims Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###might be in relation to other variables 
# 101 - 150 highest


###------------------------- Household Size ------------------------------###

summary(severity_external$weighted_Averagehouseholdsize)



# Manually defining breaks based on the range of 2 to 3.2
breaks <- seq(2, 3.2, by = 0.2)
severity_external$household_bins <- cut(severity_external$weighted_Averagehouseholdsize, 
                                        breaks = breaks, include.lowest = TRUE)


# Check the binning
table(severity_external$household_bins)

ggplot(severity_external, aes(x = household_bins)) +
  geom_bar() +
  labs(title = "Histogram of Weighted Average Household Size Bins", 
       x = "Household Size Bins", 
       y = "Frequency") +
  theme_minimal()

# Equal-width binning analysis
bin_analysis <- aggregate(claim_frequency ~ household_bins, data = severity_external, FUN = mean)


## Follow a normal distribution with higher than nromal kurtosis

# Print the results
print(bin_analysis)


# Plot for equal-width bins
ggplot(bin_analysis, aes(x = household_bins, y = claim_frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Claim Frequency by Weighted Household Size Bins (Equal Width)", 
       x = "Household Size Bins", y = "Average Claim Frequency") +
  theme_minimal()



# Try quantile?

# weighted_rentmore30percent
# weighted_rentless30percent

### --------------------- Mortgage ---------------------------###


# Financial Liability

# weighted_Mortgageless30percent
summary(severity_external$weighted_Mortgageless30percent)

severity_external <- severity_external %>%
  filter(!is.na(weighted_Mortgageless30percent), !is.na(claim_frequency)) %>%  # Remove NA values
  mutate(mortgageless30_bands = cut(weighted_Mortgageless30percent, 
                                    breaks = c(60, 65, 70, 72.5, 75, 80, Inf),  # Custom breaks: add break at 80
                                    labels = c("60-65%", "65-70%", "70-72.5%", "72.5-75%", "75-80%", "80%+"),
                                    include.lowest = TRUE))

# Check the distribution of the bins
table(severity_external$mortgageless30_bands)

# Summarize by average claim frequency for each bin
avg_claim_frequency_bands <- severity_external %>%
  group_by(mortgageless30_bands) %>%
  summarise(mean_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Plot a bar graph of the average claim frequency by bins
ggplot(avg_claim_frequency_bands, aes(x = mortgageless30_bands, y = mean_claim_frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Frequency by Mortgageless30percent Bands", 
       x = "Mortgageless30percent Bands", 
       y = "Average Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# weighted_Mortgagemore30percent
summary(severity_external$weighted_Mortgagemore30percent)

# Perform analysis for weighted_Mortgagemore30percent with custom breaks
severity_external <- severity_external %>%
  filter(!is.na(weighted_Mortgagemore30percent), !is.na(claim_frequency)) %>%  # Remove NA values
  mutate(mortgagemore30_bands = cut(weighted_Mortgagemore30percent, 
                                    breaks = c(8, 12, 14, 16, 18, 20, 25),  # Custom breaks based on statistics
                                    labels = c("8-12%", "12-14%", "14-16%", "16-18%", "18-20%", "20-25%"),
                                    include.lowest = TRUE))

# Check the distribution of the bins
table(severity_external$mortgagemore30_bands)

# Summarize by average claim frequency for each bin
avg_claim_frequency_bands_mortgage <- severity_external %>%
  group_by(mortgagemore30_bands) %>%
  summarise(mean_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Plot a bar graph of the average claim frequency by bins for weighted_Mortgagemore30percent
ggplot(avg_claim_frequency_bands_mortgage, aes(x = mortgagemore30_bands, y = mean_claim_frequency)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Average Claim Frequency by Mortgagemore30percent Bands", 
       x = "Mortgagemore30percent Bands", 
       y = "Average Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# weighted_rentmore30percent
summary(severity_external$weighted_rentmore30percent)

# Perform analysis for weighted_rentmore30percent with custom breaks in multiples of 5
severity_external <- severity_external %>%
  filter(!is.na(weighted_rentmore30percent), !is.na(claim_frequency)) %>%  # Remove NA values
  mutate(rentmore30_bands = cut(weighted_rentmore30percent, 
                                breaks = c(10, 30, 35, 40, 45, 50),  # Custom breaks with first and last multiples of 5
                                labels = c("10-30%", "30-35%", "35-40%", "40-45%", "45+%"),
                                include.lowest = TRUE))

# Check the distribution of the bins
table(severity_external$rentmore30_bands)

# Summarize by average claim frequency for each bin
avg_claim_frequency_bands_rent <- severity_external %>%
  group_by(rentmore30_bands) %>%
  summarise(mean_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Plot a bar graph of the average claim frequency by bins for weighted_rentmore30percent
ggplot(avg_claim_frequency_bands_rent, aes(x = rentmore30_bands, y = mean_claim_frequency)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Average Claim Frequency by Rentmore30percent Bands", 
       x = "Rentmore30percent Bands", 
       y = "Average Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Extreme financial burden higher likelihood of claims


# weighted_rentless30percent
summary(severity_external$weighted_rentless30percent)

# Perform analysis for the new variable with custom breaks
severity_external <- severity_external %>%
  filter(!is.na(weighted_rentless30percent), !is.na(claim_frequency)) %>%  # Remove NA values
  mutate(weighted_rentless30percent_bands = cut(weighted_rentless30percent, 
                                                breaks = c(40, 45, 50, 55, 60, 62.5, 65, 70),  # Custom breaks with additional breaks at 45, 50, 62.5
                                                labels = c("40-45%", "45-50%", "50-55%", "55-60%", "60-62.5%", "62.5-65%", "65-70%"),
                                                include.lowest = TRUE))

# Check the distribution of the bins
table(severity_external$weighted_rentless30percent_bands)

# Summarize by average claim frequency for each bin
avg_claim_frequency_weighted_rentless30percent_bands <- severity_external %>%
  group_by(weighted_rentless30percent_bands) %>%
  summarise(mean_claim_frequency = mean(claim_frequency, na.rm = TRUE))

# Plot a bar graph of the average claim frequency by bins for the new variable
ggplot(avg_claim_frequency_weighted_rentless30percent_bands, aes(x = weighted_rentless30percent_bands, y = mean_claim_frequency)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Average Claim Frequency by New Variable Bands", 
       x = "New Variable Bands", 
       y = "Average Claim Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


head(severity_external)

######################
# Hip Health Scores #
######################


### External data is a lot more granular
# might have to group manually

#feed data into chatgpt (public) and then get then to match based on nb_breed_trait


hip_health <- read.csv("dog_breed_health_score_hip.csv")


head(hip_health)
unique(hip_health$Breed)



