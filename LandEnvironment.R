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
  left_join(SA4, by = c("Code" = "SA4_CODE_2011")) %>%
  select(POSTCODE, National_parks)


# Area of agricultural land (ha) 2021
land_environment_al <- land_environment %>%
  filter(Year == 2021) %>% select(Code,Label,Year, "Area of agricultural land (ha)") %>%
  rename("Agriculture_land" = "Area of agricultural land (ha)")

merged_data_al <- land_environment_al %>%
  left_join(SA4, by = c("Code" = "SA4_CODE_2011")) %>%
  select(POSTCODE, Agriculture_land)

land_data <- merge(merged_data_al, merged_data_np, by = "POSTCODE")
land_data <- land_data %>% mutate_all(~replace(., . == "-", "Unknown"))
land_data %>% filter(National_parks == "Unknown")

# Convert Agriculture_land to numeric and National_parks to numeric
land_data <- land_data %>%
  mutate(
    Agriculture_land = as.numeric(gsub(",", "", Agriculture_land)),
    National_parks = as.numeric(National_parks)
  )

# Band land variables
land_data_banded <- land_data %>%
  mutate(
    Agriculture_land_band = case_when(
      is.na(Agriculture_land) ~ "Unknown",
      TRUE ~ cut(as.numeric(gsub(",", "", Agriculture_land)),
                 breaks = c(0, 10000, 50000, 100000, 1000000, 10000000, Inf),
                 labels = c("0-10,000 ha", "10,000-50,000 ha", "50,000-100,000 ha",
                            "100,000-1,000,000 ha", "1,000,000-10,000,000 ha", "10,000,000+ ha"),
                 include.lowest = TRUE)
    ),
    National_parks_band = case_when(
      is.na(National_parks) ~ "Unknown",
      TRUE ~ cut(National_parks,
                 breaks = c(0, 10, 25, 50, 75, 100),
                 labels = c("0-10%", "10-25%", "25-50%", "50-75%", "75-100%"),
                 include.lowest = TRUE)
    )
  )

land_data_banded %>% select(POSTCODE, Agriculture_land_band, National_parks_band)
write.csv(land_data_banded, file = "environment_land_data.csv", row.names = FALSE)

