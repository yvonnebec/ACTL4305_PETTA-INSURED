# Import data frames
land_environment <- read.csv("EXTERNAL_LANDENVIRONMENT.csv", skip = 6, header = FALSE)
SA4 <- read.csv("postcode_SA4.csv")

View(land_environment)
View(SA4)

colnames(land_environment) <- land_environment[1, ]
land_environment <- land_environment[-1, ]

land_environment$Year <- as.numeric(land_environment$Year)
land_environment$Code <- as.numeric(land_environment$Code)

land_environment_np <- land_environment %>%
  filter(Year == 2022) %>% select(Code,Label,Year, "National parks (%)") %>% 
  rename("National_parks" = "National parks (%)") 

merged_data <- land_environment_np %>%
  left_join(SA4, by = c("Code" = "SA4_CODE_2011")) %>%
  select(POSTCODE, National_parks)

View(land_environment)

land_environment_al <- land_environment %>% 
  filter(Year == 2021) %>% select(Code,Label,Year, "Area of agricultural land (ha)") %>% 
  rename("Agriculture_land" = "Area of agricultural land (ha)") 

merged_data_al <- land_environment_al %>%
  left_join(SA4, by = c("Code" = "SA4_CODE_2011")) %>%
  select(POSTCODE, Agriculture_land)

land_data <- merge(merged_data_al, merged_data, by = "POSTCODE")
land_data <- land_data %>% mutate_all(~replace(., . == "-", "Unknown"))

write.csv(land_data, "land_data.csv", row.names = FALSE)