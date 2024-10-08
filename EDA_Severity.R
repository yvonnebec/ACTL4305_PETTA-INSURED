
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

# Set NAs in pet_is_switcher to 0
levels(severity_total$pet_is_switcher) <- c(levels(severity_total$pet_is_switcher), "false")
severity_total$pet_is_switcher[is.na(severity_total$pet_is_switcher)] <- "false"


#######
# EDA #
#######

colnames(severity_total)

severity_claims <- severity_total %>%
  filter(num_claims > 0)

# Breed Type
ggplot(severity_claims, aes(x = nb_breed_type, y = cumulative_claim_paid, col = nb_breed_type)) +
  geom_boxplot() +
  labs(title = "Distribution of Cumulative Claim Paid by Breed Type",
       x = "Breed Type",
       y = "Cumulative Claim Paid") +
  theme_minimal()

severity_bins <- severity_total %>%
  mutate(age_bins = cut(pet_age_months, breaks = seq(0, max(pet_age_months, na.rm = TRUE), by = 5), 
                        include.lowest = TRUE, right = FALSE, 
                        labels = paste(seq(0, max(pet_age_months, na.rm = TRUE), by = 5)[-length(seq(0, max(pet_age_months, na.rm = TRUE), by = 5))], 
                                       seq(5, max(pet_age_months, na.rm = TRUE), by = 5)-1, sep = "-")))

# Create a histogram of cumulative_claim_paid for each age bin
ggplot(severity_bins, aes(x = age_bins, y = cumulative_claim_paid)) +
  geom_boxplot() +
  labs(title = "Cumulative Claims Paid by Pet Age (Months)",
       x = "Pet Age (Months)",
       y = "Cumulative Claims Paid") +
  theme_minimal() +
  coord_flip()

severity_bins <- severity_total %>%
  filter(cumulative_claim_paid < 10000) %>% 
  group_by(pet_age_years) %>%
  summarise(total_claims = sum(cumulative_claim_paid, na.rm = TRUE)) %>%
  mutate(proportion = total_claims / sum(total_claims))

severity_bins
severity_bins$pet_age_years <- factor(severity_bins$pet_age_years, 
                                      levels = c("0-6 months", "7-12 months", "1 years", "2 years", "3 years", "4 years", 
                                                 "5 years", "6 years", "7 years", "8 years", "9 years", "10 years"))

# Arrange the data frame by the custom age_bins factor
severity_bins_ordered <- severity_bins %>%
  arrange(pet_age_years)


# Pet age and Owner age
heatmap_data <- severity_claims %>% 
  mutate(owner_age_group = cut(owner_age_years, breaks = seq(15, 85, by = 5), include.lowest = TRUE)) %>%
  group_by(owner_age_group, pet_age_years) %>% 
  summarise(mean_claim = mean(cumulative_claim_paid, na.rm = TRUE), .groups = 'drop')

# Plot
ggplot(heatmap_data, aes(x = owner_age_group, y = pet_age_years)) + 
  geom_tile(aes(fill = mean_claim), color = "white") + 
  scale_fill_gradient(low = "blue", high = "red", name = "Cumulative Claim Amount") + 
  labs(title = "Heatmap of Cumulative Claims by Owner Age and Pet Age", 
       x = "Owner Age Group (Years)", 
       y = "Pet Age (Years)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##### 
heatmap_data <- severity_claims %>% 
  mutate(owner_age_group = cut(owner_age_years, breaks = seq(15, 85, by = 5), include.lowest = TRUE)) %>%
  group_by(owner_age_group, nb_breed_type) %>% 
  summarise(mean_claim = mean(cumulative_claim_paid, na.rm = TRUE), .groups = 'drop')

ggplot(heatmap_data, aes(x = owner_age_group, y = nb_breed_type)) + 
  geom_tile(aes(fill = mean_claim), color = "white") + 
  scale_fill_gradient(low = "blue", high = "red", name = "Cumulative Claim Amount") + 
  labs(title = "Heatmap of Cumulative Claims by Owner Age and Breed Traits", 
       x = "Owner Age Group (Years)", 
       y = "Breed Traits Group") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##
heatmap_data <- severity_claims %>% 
  mutate(owner_age_group = cut(owner_age_years, breaks = seq(15, 85, by = 5), include.lowest = TRUE)) %>%
  group_by(owner_age_group, nb_contribution) %>% 
  summarise(mean_claim = mean(cumulative_claim_paid, na.rm = TRUE), .groups = 'drop')

# Plot
ggplot(heatmap_data, aes(x = owner_age_group, y = nb_contribution)) + 
  geom_tile(aes(fill = mean_claim), color = "white") + 
  scale_fill_gradient(low = "blue", high = "red", name = "Cumulative Claim Amount") + 
  labs(title = "Heatmap of Cumulative Claims by Owner Age and Excess", 
       x = "Owner Age Group (Years)", 
       y = "Excess") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Simple linear regression model
model <- lm(cumulative_claim_paid ~ owner_age_years + nb_excess + nb_contribution + nb_breed_trait + pet_age_years, data = severity_claims)
summary(model)

anova_model <- aov(cumulative_claim_paid ~ factor(nb_number_of_breeds) + factor(nb_breed_trait) + factor(nb_excess), data = severity_claims)
summary(anova_model)

# Boxplot of cumulative claim by nb_breed_trait
ggplot(severity_claims, aes(x = factor(nb_breed_type), y = cumulative_claim_paid)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cumulative Claim by Breed Trait", x = "Breed Trait", y = "Cumulative Claim Amount")


library(randomForest)
predictor_vars = c("pet_gender", "pet_de_sexed", "pet_de_sexed_age", "pet_is_switcher", "nb_policy_first_inception_date", "pet_age_months", "nb_contribution", "nb_excess", "nb_address_type_adj", "nb_suburb", "nb_postcode", "nb_state", "person_dob", "nb_contribution_excess", "pet_age_years", "owner_age_years", "nb_number_of_breeds", "nb_average_breed_size", "nb_breed_type", "nb_breed_trait", "nb_breed_name_unique", "nb_breed_name_unique_concat", "is_multi_pet_plan", "lead_date_day", "quote_date", "quote_time_group")

# Random forest to find important variables
rf_model <- randomForest(cumulative_claim_paid ~ pet_gender + pet_de_sexed + pet_de_sexed_age + 
                           pet_is_switcher + nb_policy_first_inception_date + pet_age_months + 
                           nb_contribution + nb_excess + nb_address_type_adj + 
                           nb_postcode + nb_state + person_dob + nb_contribution_excess + 
                           pet_age_years + owner_age_years + nb_number_of_breeds + 
                           nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan,
                         data = severity_claims, importance = TRUE, ntree = 500)  # Increased number of trees

# Plot variable importance
importance_matrix <- importance(rf_model)

# Convert to a data frame and arrange by importance
importance_df <- as.data.frame(importance_matrix)
importance_df_sorted <- importance_df %>%
  arrange(desc(`%IncMSE`))
importance_df_sorted

severity_claims$nb_b

####################
# Model Selection #
###################

severity_modelling <- severity_total %>%
  select(all_of(predictor_vars), cumulative_claim_paid) %>% 
  mutate(across(where(is.character), as.factor))

# Fit the full model
full_model <- lm(cumulative_claim_paid ~ ., data = severity_modelling)

install.packages("leaps")

# Load the leaps package
library(leaps)

# Perform best subset selection
subset_selection <- regsubsets(cumulative_claim_paid ~ ., data = severity_modelling, nvmax = 20)
summary(subset_selection)



# LASSO
library(glmnet)

X <- model.matrix(cumulative_claim_paid ~ ., data = severity_total)[, -1]  # Exclude intercept
y <- severity_total$cumulative_claim_paid

# Standardize the predictors
X_scaled <- scale(X)

# Fit Lasso model (alpha = 1 for Lasso)
lasso_model <- cv.glmnet(X_scaled, y, alpha = 1)

# Plot cross-validated error
plot(lasso_model)

# Identify the best lambda
best_lambda <- lasso_model$lambda.min
lasso_coefs <- coef(lasso_model, s = best_lambda)

# Display coefficients
print(lasso_coefs)

## Ridge
# Fit Ridge model (alpha = 0 for Ridge)
ridge_model <- cv.glmnet(X_scaled, y, alpha = 0)

# Plot cross-validated error
plot(ridge_model)

# Identify the best lambda
best_lambda_ridge <- ridge_model$lambda.min
ridge_coefs <- coef(ridge_model, s = best_lambda_ridge)

# Display coefficients
print(ridge_coefs)
