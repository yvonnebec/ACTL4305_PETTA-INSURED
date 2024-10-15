# Import Lib
library(tidyverse)

# Import data frames
land_environment <- read.csv("EXTERNAL_LANDENVIRONMENT.csv", skip = 6, header = FALSE)
SA4 <- read.csv("postcode_SA4.csv")

#View(land_environment)
#View(SA4)

colnames(land_environment) <- land_environment[1, ]
land_environment <- land_environment[-1, ]

land_environment <- land_environment %>%mutate_all(~ na_if(., "-"))

land_environment$Year <- as.numeric(land_environment$Year)
land_environment$Code <- as.numeric(land_environment$Code)

land_environment_np <- land_environment %>%
  filter(Year == 2022) %>% select(Code,Label,Year, "National parks (%)") %>%
  rename("National_parks" = "National parks (%)")

# National Park % data and Postcode (2022 data)
merged_data_np <- land_environment_np %>%
  left_join(SA4, by = c("Code" = "SA4_CODE_2011"))

# Area of agricultural land (ha) 2021
land_environment_al <- land_environment %>%
  filter(Year == 2021) %>% select(Code,Label,Year, "Area of agricultural land (ha)") %>%
  rename("Agriculture_land" = "Area of agricultural land (ha)")

merged_data_al <- land_environment_al %>%
  left_join(SA4, by = c("Code" = "SA4_CODE_2011"))

land_data <- merge(merged_data_al, merged_data_np, by = "Code")

land_data <- land_data %>%
  select(-matches("\\.y$"), -Year.x)

colnames(land_data) <- gsub("\\.x$", "", colnames(land_data))

land_data <- land_data %>% distinct()

merged_data <- land_data %>%
  mutate(
    PERCENTAGE = as.numeric(PERCENTAGE) / 100,
    Agriculture_land = as.numeric(gsub(",", "", Agriculture_land)),
    National_parks = as.numeric(National_parks),
    weighted_national_parks = National_parks * PERCENTAGE,
    weighted_agricultural_land = Agriculture_land * PERCENTAGE,
  )

result <- merged_data %>%
  group_by(POSTCODE) %>%
  summarise(
    weighted_national_parks = sum(weighted_national_parks, na.rm = TRUE),
    weighted_agricultural_land = sum(weighted_agricultural_land, na.rm = TRUE),
  )

result$weighted_national_parks <- result$weighted_national_parks / 100

# Banding land data
banded_result <- result %>%
  mutate(
    national_parks_band = cut(weighted_national_parks * 100,
                              breaks = c(-Inf, 0, 5, 10, 25, 50, 75, 100),  # '-Inf' to handle exactly 0
                              labels = c("0", "0-5%", "5-10%", "10-25%", "25-50%", "50-75%", "75-100%"),
                              include.lowest = TRUE,
                              right = TRUE),
    agricultural_land_band = cut(weighted_agricultural_land,
                                 breaks = c(-Inf, 0, 10000, 100000, 1000000, 10000000, Inf),  # '-Inf' for exactly 0
                                 labels = c("0", "0-10,000", "10,000-100,000", "100,000-1,000,000", "1,000,000-10,000,000", "10,000,000+"),
                                 include.lowest = TRUE,
                                 right = TRUE)
  ) %>% select(POSTCODE, national_parks_band, agricultural_land_band)

severity_total <- read.csv("severity_total.csv")
View(severity_total)

severity_total <- severity_total %>%
  left_join(banded_result, by = c("nb_postcode" = "POSTCODE")) %>% 
  mutate(
    agricultural_land_band = replace_na(agricultural_land_band, "Unknown"),
    national_parks_band = replace_na(national_parks_band, "Unknown")
  )



write.csv(severity_total, file = "severity_total.csv", row.names = FALSE)
