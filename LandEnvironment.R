

# Import data frames
land_environment <- read.csv("EXTERNAL_LANDENVIRONMENT.csv")
SA4 <- read.csv("postcode_SA4.csv")

view(family)
view(SA4)

summary(family)

str(family$Homeless_per10k)

family <- family %>%
  filter(Year == 2021) %>%
  rename(
    Averagehouseholdsize = Average.household.size..no..of.persons.,
    Socioeconomic_index = SEIFA.Index.of.relative.socio.economic.advantage.and.disadvantage..IRSAD....rank.within.Australia..decile.
  ) %>%
  mutate(Code = as.numeric(Code),
         Homeless_per10k = as.numeric(gsub(",", "", Homeless_per10k)),
         Averagehouseholdsize = as.numeric((Averagehouseholdsize)),
         Socioeconomic_index = as.numeric(Socioeconomic_index)
  )


family_filtered <- family %>%
  select(Code, Homeless_per10k, Averagehouseholdsize, Socioeconomic_index)

na_rows <- family_filtered %>%
  filter(is.na(Homeless_per10k))


summary(family$Homeless_per10k)
summary(family$Averagehouseholdsize)

sum(is.na(family$Homeless_per10k))
sum(is.na(family$Code))

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
    weighted_socioeconomic_index = Socioeconomic_index * PERCENTAGE
  )

summary(merged_data)


# Group by SA4_CODE_2011 and calculate the average weighted homelessness rate
result <- merged_data %>%
  group_by(POSTCODE) %>%
  summarise(
    avg_weighted_homeless_rate = sum(weighted_homeless_rate, na.rm = TRUE),
    weighted_Averagehouseholdsize = sum(weighted_Averagehouseholdsize, na.rm = TRUE),
    weighted_socioeconomic_index = sum(weighted_socioeconomic_index, na.rm = TRUE)
  )


summary(result$avg_weighted_homeless_rate)

# Merging result with severity_total based on nb_postcode
Severity_external <- severity_total %>%
  left_join(result, by = c("nb_postcode" = "POSTCODE"))

# View the first few rows of the merged dataset
head(Severity_external)

view(result %>% filter(weighted_Averagehouseholdsize < 2))
