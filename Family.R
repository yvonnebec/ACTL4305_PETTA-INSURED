

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


summary(result$avg_weighted_homeless_rate)

# Merging result with severity_total based on nb_postcode
Severity_external <- severity_total %>%
  left_join(result, by = c("nb_postcode" = "POSTCODE"))

# View the first few rows of the merged dataset
head(Severity_external)

view(result %>% filter(weighted_Averagehouseholdsize < 2))
