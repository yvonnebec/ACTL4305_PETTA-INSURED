#################################################
#################################################
######### Branch file for Correlations ##########
#################################################
#################################################

# Install ggcor
install.packages("ggcor")
install.packages("DescTools")

#set working directory
setwd("C:/Users/luori/OneDrive/Desktop/2024/Semester 3/Assignment/Assignment Data")

# load all necessary packages
library("ggcor")
library(ggplot2)
library(DescTools)

Final_data <- read.csv("actl4305_final_df_v8.csv")

head(Final_data)

# Variables of interest

# pet_gender
# pet_is_switcher
# pet_de_sexed
# is_multi_pet_plan

#pet_age_years

# weighted_Mortgagemore30percent
# median_income_band


#######################################
### Chi-Square Test of Independence ###
#######################################

summary(Final_data$pet_age_years)

age_summary <- Final_data %>%
  group_by(pet_age_years) %>%
  summarise(count = n())

age_summary

# Modify the pet_age_years column to group 9 and 10 years together
Final_data <- Final_data %>%
  mutate(pet_age_years = ifelse(pet_age_years %in% c(9, 10), "9-10", pet_age_years))

### Need to backtrack and look at the continuous data and then
### generate correlation matrix using "ggcor"

# Chi-square test between pet_age_years and pet_gender
chisq.test(table(Final_data$pet_age_years, Final_data$pet_gender))
fisher.test(table(Final_data$pet_age_years, Final_data$pet_gender))


# Repeat for the other variables
chisq.test(table(Final_data$pet_age_years, Final_data$pet_is_switcher))
chisq.test(table(Final_data$pet_age_years, Final_data$pet_de_sexed))
chisq.test(table(Final_data$pet_age_years, Final_data$is_multi_pet_plan))

chisq.test(table(Final_data$pet_de_sexed, Final_data$median_income_band))

# All suggest variables are not independent

### Income band, rent and location
chisq.test(table(Final_data$weighted_rentmore30percent, Final_data$median_income_band))


################
### Cramer V ###
################


library(DescTools)
# Calculate Cramér's V for the chi-square test between pet_age_years and pet_is_switcher
CramerV(table(Final_data$pet_age_years, Final_data$pet_is_switcher))

CramerV(table(Final_data$pet_age_years, Final_data$pet_de_sexed))

CramerV(table(Final_data$pet_age_years, Final_data$is_multi_pet_plan))


# Install the vcd package if you don't have it
# install.packages("vcd")

# Load the necessary library
library(vcd)

# Assuming Final_data contains the required variables
# List of variables to compare
variables <- c("pet_gender", "pet_is_switcher", "pet_de_sexed", "is_multi_pet_plan", "pet_age_years")

# Combine all predictors from both lists
variables <- c(
  "pet_gender", # black
  "pet_is_switcher", #black
  "pet_de_sexed", #black
  "is_multi_pet_plan", #black
  "nb_address_type_adj",
  "pet_age_BAND",
  "owner_age_band",
  "nb_breed_type", #black
  #"breed_type_band", 
  "nb_average_breed_size_FACTOR",
  "binned_breeds", # black
  "median_income_band",
  "nb_excess_FACTOR",
  "breed_MAP",
  "nb_contribution_FACTOR",
  "agricultural_land_BAND",
  "electorate_rating_banded"
)

str(Final_data$breed_type_band)

# Subset the dataset to include only the specified variables
subset_data <- Final_data[ , variables]

# Get summary statistics for the subset
summary(subset_data)

sum(is.na(subset_data))


# Convert each specified variable in Final_data to a factor
for (var in variables) {
  if (var %in% colnames(Final_data)) {  # Check if the variable exists in the dataset
    Final_data[[var]] <- as.factor(Final_data[[var]])
  } else {
    warning(paste("Variable", var, "not found in Final_data"))
  }
}


summary(Final_data)

# Initialize an empty matrix to store Cramer's V values
cramers_v_matrix <- matrix(NA, ncol = length(variables), nrow = length(variables),
                           dimnames = list(variables, variables))

# Calculate Cramer's V for each pair of variables using CramerV()
for (i in 1:length(variables)) {
  for (j in 1:length(variables)) {
    cramers_v_matrix[i, j] <- CramerV(Final_data[[variables[i]]], Final_data[[variables[j]]])
  }
}

# Convert the matrix to a data frame for easier visualization
cramers_v_df <- as.data.frame(cramers_v_matrix)
# Print the result
print(cramers_v_df)
# Check the structure
str(cramers_v_df)


############
### PLOT ###
############


# Ensure cramers_v_matrix is a matrix with row and column names set
rownames(cramers_v_matrix) <- variables
colnames(cramers_v_matrix) <- variables

print(cramers_v_matrix)

# Convert to a data frame and then manually reshape to get Variable1 and Variable2 columns
cramers_v_long <- as.data.frame(as.table(cramers_v_matrix))
colnames(cramers_v_long) <- c("Variable1", "Variable2", "CramersV")

# Confirm structure
head(cramers_v_long)

### Plot using ggcor or gg_tiles, geom_tiles

# Plot the Cramer's V matrix using ggplot2 and geom_tile
ggplot(data = cramers_v_long, aes(x = Variable1, y = Variable2, fill = CramersV)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(0, 1), space = "Lab",
                       name="Cramer's V") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  # Add labels only for values greater than 0.5
  geom_text(aes(label = ifelse(abs(CramersV) > 0.4 & abs(CramersV) < 0.99, round(CramersV, 2), "")), color = "black")




#################
### Pearson's ###
#################

# Continuous Variables

# pet_age_months
# owner_age_years
# median_taxable_income 
# agricultural_land


# Subset the data to include only the specified variables
numeric_data <- Final_data[, c("pet_age_months", "owner_age_years", "median_taxable_income", "agricultural_land")]

# Ensure all variables are numeric
numeric_data <- data.frame(lapply(numeric_data, as.numeric))

# Calculate the Pearson's correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs", method = "pearson")

# Print the result
print(correlation_matrix)


# Convert the correlation matrix to a long format
correlation_long <- melt(correlation_matrix)

# Plot the heatmap using ggplot2
# Plot the heatmap using ggplot2, adding labels for correlations greater than 0.5
ggplot(data = correlation_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Create the tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), 
                       name = "Pearson\nCorrelation") +  # Color scale
  labs(title = "Pearson Correlation Matrix Heatmap",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add labels only for values greater than 0.5
  geom_text(aes(label = ifelse(abs(value) > 0.5 & abs(value) < 1, round(value, 2), "")), color = "black")
