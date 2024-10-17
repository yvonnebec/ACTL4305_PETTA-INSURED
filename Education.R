
# Get datasets
education <- read.csv("Education.csv")
SA4 <- read.csv("postcode_SA4.csv")


severity_external <- read.csv("severity_external.csv")

# Makign function bit annoying as lots of different variables


# In a work context, the term "Total Fully Engaged (%)" typically refers to the percentage of people 
# who are either fully employed or actively engaged in employment or education. It is commonly used in 
# labor market statistics to measure the proportion of the workforce or a particular demographic that is 
# fully participating in work or study.

summary(education)

head(severity_external)

#NOTE - Data is for persons aged 15 and over


# more variables
#completed year 12 (high school)
# Unemployment rate
# Participation rate

###after only INCOME and PERSONSBORNOVERSEAS left

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
  mutate(POSTCODE = as.character(POSTCODE))

# Merging result with severity_total based on nb_postcode
severity_external <- severity_external %>%
  left_join(result, by = c("nb_postcode" = "POSTCODE"))

# View the first few rows of the merged dataset
head(severity_external)



# Exporting to a specific directory, e.g., Desktop (modify the path for your system)
write.csv(severity_external, "C:/Users/luori/OneDrive/Desktop/2024/Semester 3/Assignment/Assignment Data/Merged Data/severity_external.csv", row.names = FALSE)



############################
# Education and Employment #
############################

head(severity_external)

### --------------------- % Fully Engaged ---------------------------###

summary(severity_external$avg_weighted_Total_fully_engaged_percent)

### avg_weighted_Total_fully_engaged_percent

# Create 3 equal-sized bins (quantiles) while removing NA values
severity_external$engagement_bins <- cut(severity_external$avg_weighted_Total_fully_engaged_percent, 
                                         breaks = quantile(severity_external$avg_weighted_Total_fully_engaged_percent, 
                                                           probs = seq(0, 1, length.out = 5), 
                                                           na.rm = TRUE), 
                                         include.lowest = TRUE)

# Check the binning
table(severity_external$engagement_bins)

# Group by the bins and calculate the mean claim frequency for each bin
engagement_bin_analysis <- aggregate(claim_frequency ~ engagement_bins, data = severity_external, FUN = mean)

# Print the result
print(engagement_bin_analysis)

library(ggplot2)

# Plotting the average claim frequency by engagement bins
ggplot(engagement_bin_analysis, aes(x = engagement_bins, y = claim_frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Claim Frequency by Engagement Percent Bins", 
       x = "Engagement Percent Bins based on 4 quantiles", 
       y = "Average Claim Frequency") +
  theme_minimal()


### --------------------- % Completed year 12 ---------------------------###

### avg_weighted_Completed_year12

# Create 3 equal-sized bins (quantiles) for avg_weighted_Completed_year12
severity_external$year12_bins <- cut(severity_external$avg_weighted_Completed_year12, 
                                     breaks = quantile(severity_external$avg_weighted_Completed_year12, 
                                                       probs = seq(0, 1, length.out = 6), 
                                                       na.rm = TRUE), 
                                     include.lowest = TRUE)

# Plotting a histogram for avg_weighted_Completed_year12 without creating bins
ggplot(severity_external, aes(x = avg_weighted_Completed_year12)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +  # Adjust bins for desired granularity
  labs(title = "Distribution of Completed Year 12 Percent", 
       x = "Completed Year 12 Percent", 
       y = "Frequency") +
  theme_minimal()

# Check the binning
table(severity_external$year12_bins)

# Group by the year12_bins and calculate the mean claim frequency
year12_bin_analysis <- aggregate(claim_frequency ~ year12_bins, data = severity_external, FUN = mean, na.action = na.omit)

# Print the result
print(year12_bin_analysis)


# Plotting the average claim frequency by year12 bins
ggplot(year12_bin_analysis, aes(x = year12_bins, y = claim_frequency)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Claim Frequency by Completed Year 12 Percent (Quantile Bins)", 
       x = "Completed Year 12 Percent Bins", 
       y = "Average Claim Frequency") +
  theme_minimal()

# Higher percentage completed year 12, high claim freq

### --------------------- Unemployment rate (%) ---------------------------###

### avg_weighted_Unemployment_rate

# Plotting a histogram for avg_weighted_Unemployment_rate without creating bins
ggplot(severity_external, aes(x = avg_weighted_Unemployment_rate)) +
  geom_histogram(color = "black", bins = 30) +
  labs(title = "Distribution of Unemployment Rate", 
       x = "Unemployment Rate", 
       y = "Frequency") +
  theme_minimal()

# Create 3 equal-sized bins (quantiles) for avg_weighted_Unemployment_rate
severity_external$unemployment_bins <- cut(severity_external$avg_weighted_Unemployment_rate, 
                                           breaks = quantile(severity_external$avg_weighted_Unemployment_rate, 
                                                             probs = seq(0, 1, length.out = 6), 
                                                             na.rm = TRUE), 
                                           include.lowest = TRUE)

# Check the binning
table(severity_external$unemployment_bins)

# Group by the unemployment bins and calculate the mean claim frequency
unemployment_bin_analysis <- aggregate(claim_frequency ~ unemployment_bins, data = severity_external, FUN = mean, na.action = na.omit)

# Print the result
print(unemployment_bin_analysis)


# Plotting the average claim frequency by unemployment rate bins
ggplot(unemployment_bin_analysis, aes(x = unemployment_bins, y = claim_frequency)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Claim Frequency by Unemployment Rate (Quantile Bins)", 
       x = "Unemployment Rate Bins", 
       y = "Average Claim Frequency") +
  theme_minimal()


### no trends for unemployment rate


 ### --------------------- Participation rate (%) ---------------------------###

### Higher participation rate - slightly higher claim frequency

ggplot(severity_external, aes(x = avg_weighted_Participation_rate)) +
  geom_histogram(color = "black", bins = 30) +
  labs(title = "Distribution of Unemployment Rate", 
       x = "Participation Rate", 
       y = "Frequency") +
  theme_minimal()

### avg_weighted_Participation_rate

# Create 3 equal-sized bins (quantiles) for avg_weighted_Participation_rate
severity_external$participation_bins <- cut(severity_external$avg_weighted_Participation_rate, 
                                            breaks = quantile(severity_external$avg_weighted_Participation_rate, 
                                                              probs = seq(0, 1, length.out = 6), 
                                                              na.rm = TRUE), 
                                            include.lowest = TRUE)

# Check the binning
table(severity_external$participation_bins)

# Group by the participation rate bins and calculate the mean claim frequency
participation_bin_analysis <- aggregate(claim_frequency ~ participation_bins, data = severity_external, FUN = mean, na.action = na.omit)

# Print the result
print(participation_bin_analysis)


# Plotting the average claim frequency by participation rate bins
ggplot(participation_bin_analysis, aes(x = participation_bins, y = claim_frequency)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Claim Frequency by Participation Rate (Quantile Bins)", 
       x = "Participation Rate Bins", 
       y = "Average Claim Frequency") +
  theme_minimal()


