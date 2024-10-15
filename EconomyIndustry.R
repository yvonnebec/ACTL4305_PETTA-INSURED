# Import Lib
library(tidyverse)

# Import data frames
economy_industry <- read.csv("EXTERNAL_ECONOMYANDINDUSTRY.csv", skip = 6, header = FALSE)
SA4 <- read.csv("postcode_SA4.csv")

#View(economy_industry)
#View(SA4)

colnames(economy_industry) <- economy_industry[1, ]
economy_industry <- economy_industry[-1, ]

# Define important columns
important_cols <- c("Total number of businesses",
                    "Number of businesses with turnover of zero to less than $50k",
                    "Number of businesses with turnover of $50k to less than $200k",
                    "Number of businesses with turnover of $200k to less than $2m",
                    "Number of businesses with turnover of $2m to less than $5m",
                    "Number of businesses with turnover of $5m to less than $10m",
                    "Number of businesses with turnover of $10m or more")

View(economy_industry)

economy_industry <- economy_industry %>% mutate_all(~ na_if(., "-"))

economy_industry$Year <- as.numeric(economy_industry$Year)
economy_industry$Code <- as.numeric(economy_industry$Code)

economy_industry_cols <- economy_industry %>%
  filter(Year == 2023) %>%
  select(Code, Label, Year, all_of(important_cols)) %>%
  rename(
    total_number_of_businesses = `Total number of businesses`,
    number_of_businesses_turnover_0_to_50k = `Number of businesses with turnover of zero to less than $50k`,
    number_of_businesses_turnover_50k_to_200k = `Number of businesses with turnover of $50k to less than $200k`,
    number_of_businesses_turnover_200k_to_2m = `Number of businesses with turnover of $200k to less than $2m`,
    number_of_businesses_turnover_2m_to_5m = `Number of businesses with turnover of $2m to less than $5m`,
    number_of_businesses_turnover_5m_to_10m = `Number of businesses with turnover of $5m to less than $10m`,
    number_of_businesses_turnover_10m_or_more = `Number of businesses with turnover of $10m or more`
  )

# Merge on Postcode
merged_economy <- economy_industry_cols %>%
  left_join(SA4, by = c("Code" = "SA4_CODE_2011"))

nrow(merged_economy)

View(merged_economy)

weighted_merged_data <- merged_economy %>%
  mutate(
    PERCENTAGE = as.numeric(PERCENTAGE) / 100,
    total_number_of_businesses = as.numeric(gsub(",", "", total_number_of_businesses)),
    number_of_businesses_turnover_0_to_50k = as.numeric(gsub(",", "", number_of_businesses_turnover_0_to_50k)),
    number_of_businesses_turnover_50k_to_200k = as.numeric(gsub(",", "", number_of_businesses_turnover_50k_to_200k)),
    number_of_businesses_turnover_200k_to_2m = as.numeric(gsub(",", "", number_of_businesses_turnover_200k_to_2m)),
    number_of_businesses_turnover_2m_to_5m = as.numeric(gsub(",", "", number_of_businesses_turnover_2m_to_5m)),
    number_of_businesses_turnover_5m_to_10m = as.numeric(gsub(",", "", number_of_businesses_turnover_5m_to_10m)),
    number_of_businesses_turnover_10m_or_more = as.numeric(gsub(",", "", number_of_businesses_turnover_10m_or_more)),
    # Calculate weighted variables
    weighted_total_number_of_businesses = total_number_of_businesses * PERCENTAGE,
    weighted_businesses_turnover_0_to_50k = number_of_businesses_turnover_0_to_50k * PERCENTAGE,
    weighted_businesses_turnover_50k_to_200k = number_of_businesses_turnover_50k_to_200k * PERCENTAGE,
    weighted_businesses_turnover_200k_to_2m = number_of_businesses_turnover_200k_to_2m * PERCENTAGE,
    weighted_businesses_turnover_2m_to_5m = number_of_businesses_turnover_2m_to_5m * PERCENTAGE,
    weighted_businesses_turnover_5m_to_10m = number_of_businesses_turnover_5m_to_10m * PERCENTAGE,
    weighted_businesses_turnover_10m_or_more = number_of_businesses_turnover_10m_or_more * PERCENTAGE
  )

result <- weighted_merged_data %>%
  group_by(POSTCODE) %>%
  summarise(
    weighted_total_number_of_businesses = sum(weighted_total_number_of_businesses, na.rm = TRUE),
    weighted_businesses_turnover_0_to_50k = sum(weighted_businesses_turnover_0_to_50k, na.rm = TRUE),
    weighted_businesses_turnover_50k_to_200k = sum(weighted_businesses_turnover_50k_to_200k, na.rm = TRUE),
    weighted_businesses_turnover_200k_to_2m = sum(weighted_businesses_turnover_200k_to_2m, na.rm = TRUE),
    weighted_businesses_turnover_2m_to_5m = sum(weighted_businesses_turnover_2m_to_5m, na.rm = TRUE),
    weighted_businesses_turnover_5m_to_10m = sum(weighted_businesses_turnover_5m_to_10m, na.rm = TRUE),
    weighted_businesses_turnover_10m_or_more = sum(weighted_businesses_turnover_10m_or_more, na.rm = TRUE)
  )
View(result)

# Banding weighted data
banded_result <- result %>%
  mutate(
    total_number_of_businesses_band = cut(weighted_total_number_of_businesses, 
                                            breaks = c(-Inf, 10000, 20000, 30000, 40000, 50000, 75000, 100000, Inf), 
                                            labels = c("0-10000", "10000-20000", "20000-30000", "30000-40000","40000-50000", "50000-75000", "75000-100000", "100000+")),
    businesses_turnover_0_to_50k_band = cut(weighted_businesses_turnover_0_to_50k, 
                                            breaks = c(-Inf, 100, 250, 500, 750, 1000, 2500, Inf), 
                                            labels = c("0-100", "100-250", "250-500", "500-750", "750-1000", "1000-2500", "2500+")),
    businesses_turnover_50k_to_200k_band = cut(weighted_businesses_turnover_50k_to_200k, 
                                               breaks = c(-Inf, 100, 250, 500, 750, 1000, 2500, Inf), 
                                               labels = c("0-100", "100-250", "250-500", "500-750", "750-1000", "1000-2500", "2500+")),
    businesses_turnover_200k_to_2m_band = cut(weighted_businesses_turnover_200k_to_2m, 
                                              breaks = c(-Inf, 100, 250, 500, 750, 1000, 2500, Inf), 
                                              labels = c("0-100", "100-250", "250-500", "500-750", "750-1000", "1000-2500", "2500+")),
    businesses_turnover_2m_to_5m_band = cut(weighted_businesses_turnover_2m_to_5m, 
                                            breaks = c(-Inf, 100, 250, 500, 750, 1000, 2500, Inf), 
                                            labels = c("0-100", "100-250", "250-500", "500-750", "750-1000", "1000-2500", "2500+")),
    businesses_turnover_5m_to_10m_band = cut(weighted_businesses_turnover_5m_to_10m, 
                                             breaks = c(-Inf, 100, 250, 500, 750, 1000, 2500, Inf), 
                                             labels = c("0-100", "100-250", "250-500", "500-750", "750-1000", "1000-2500", "2500+")),
    businesses_turnover_10m_or_more_band = cut(weighted_businesses_turnover_10m_or_more, 
                                               breaks = c(-Inf, 100, 250, 500, 750, 1000, 2500, Inf), 
                                               labels = c("0-100", "100-250", "250-500", "500-750", "750-1000", "1000-2500", "2500+"))
  )

#View(banded_result)
banded_result$POSTCODE <- as.character(banded_result$POSTCODE)

severity_total <- read.csv(file="severity_total.csv", header = TRUE)
colnames(severity_total)
severity_total <- severity_total %>% 
  select(
    -weighted_total_number_of_businesses,
    -businesses_turnover_0_to_50k,
    -businesses_turnover_50k_to_200k,
    -businesses_turnover_200k_to_2m,
    -businesses_turnover_2m_to_5m,
    -businesses_turnover_5m_to_10m,
    -businesses_turnover_10m_or_more,
    -total_number_of_businesses_band,
    -businesses_turnover_0_to_50k_band,
    -businesses_turnover_50k_to_200k_band,
    -businesses_turnover_200k_to_2m_band,
    -businesses_turnover_2m_to_5m_band,
    -businesses_turnover_5m_to_10m_band,
    -businesses_turnover_10m_or_more_band)

View(banded_result)

severity_total <- severity_total %>% 
  left_join(banded_result, by = c("nb_postcode" = "POSTCODE"))

severity_total <- severity_total %>%
  rename(
    businesses_turnover_0_to_50k = weighted_businesses_turnover_0_to_50k,
    businesses_turnover_50k_to_200k = weighted_businesses_turnover_50k_to_200k,
    businesses_turnover_200k_to_2m = weighted_businesses_turnover_200k_to_2m,
    businesses_turnover_2m_to_5m = weighted_businesses_turnover_2m_to_5m,
    businesses_turnover_5m_to_10m = weighted_businesses_turnover_5m_to_10m,
    businesses_turnover_10m_or_more = weighted_businesses_turnover_10m_or_more
  )

View(severity_total)

severity_total <- severity_total %>% 
  mutate(
    businesses_turnover_0_to_50k = as.character(businesses_turnover_0_to_50k),
    businesses_turnover_50k_to_200k = as.character(businesses_turnover_50k_to_200k),
    businesses_turnover_200k_to_2m = as.character(businesses_turnover_200k_to_2m),
    businesses_turnover_2m_to_5m = as.character(businesses_turnover_2m_to_5m),
    businesses_turnover_5m_to_10m = as.character(businesses_turnover_5m_to_10m),
    businesses_turnover_10m_or_more = as.character(businesses_turnover_10m_or_more),
    businesses_turnover_0_to_50k_band = as.character(businesses_turnover_0_to_50k_band),
    businesses_turnover_50k_to_200k_band = as.character(businesses_turnover_50k_to_200k_band),
    businesses_turnover_200k_to_2m_band = as.character(businesses_turnover_200k_to_2m_band),
    businesses_turnover_2m_to_5m_band = as.character(businesses_turnover_2m_to_5m_band),
    businesses_turnover_5m_to_10m_band = as.character(businesses_turnover_5m_to_10m_band),
    businesses_turnover_10m_or_more_band = as.character(businesses_turnover_10m_or_more_band)
  ) %>%
  mutate(
    agricultural_land = replace_na(agricultural_land, "Unknown"),
    national_parks = replace_na(national_parks, "Unknown"),
    agricultural_land_band = replace_na(agricultural_land_band, "Unknown"),
    national_parks_band = replace_na(national_parks_band, "Unknown"),
    businesses_turnover_0_to_50k = replace_na(businesses_turnover_0_to_50k, "Unknown"),
    businesses_turnover_50k_to_200k = replace_na(businesses_turnover_50k_to_200k, "Unknown"),
    businesses_turnover_200k_to_2m = replace_na(businesses_turnover_200k_to_2m, "Unknown"),
    businesses_turnover_2m_to_5m = replace_na(businesses_turnover_2m_to_5m, "Unknown"),
    businesses_turnover_5m_to_10m = replace_na(businesses_turnover_5m_to_10m, "Unknown"),
    businesses_turnover_10m_or_more = replace_na(businesses_turnover_10m_or_more, "Unknown"),
    businesses_turnover_0_to_50k_band = replace_na(businesses_turnover_0_to_50k_band, "Unknown"),
    businesses_turnover_50k_to_200k_band = replace_na(businesses_turnover_50k_to_200k_band, "Unknown"),
    businesses_turnover_200k_to_2m_band = replace_na(businesses_turnover_200k_to_2m_band, "Unknown"),
    businesses_turnover_2m_to_5m_band = replace_na(businesses_turnover_2m_to_5m_band, "Unknown"),
    businesses_turnover_5m_to_10m_band = replace_na(businesses_turnover_5m_to_10m_band, "Unknown"),
    businesses_turnover_10m_or_more_band = replace_na(businesses_turnover_10m_or_more_band, "Unknown")
  )

severity_economy <- severity_total %>% 
  select(
    nb_postcode,
    weighted_total_number_of_businesses,
    businesses_turnover_0_to_50k,
    businesses_turnover_50k_to_200k,
    businesses_turnover_200k_to_2m,
    businesses_turnover_2m_to_5m,
    businesses_turnover_5m_to_10m,
    businesses_turnover_10m_or_more,
    total_number_of_businesses_band,
    businesses_turnover_0_to_50k_band,
    businesses_turnover_50k_to_200k_band,
    businesses_turnover_200k_to_2m_band,
    businesses_turnover_2m_to_5m_band,
    businesses_turnover_5m_to_10m_band,
    businesses_turnover_10m_or_more_band)

write.csv(severity_economy, file = "severity_economy.csv", row.names = FALSE)
