library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

country_data <- read_csv(file.choose())
country_plastic <- country_data %>%
  select(region_id, country_name, composition_plastic_percent) %>%
  filter(!is.na(composition_plastic_percent))

city_data <- read_csv(file.choose())

city_plastic <- city_data %>%
  select(region_id,country_name, city_name, composition_plastic_percent) %>%
  filter(!is.na(composition_plastic_percent))
##save

write_csv(country_plastic,
          "country_plastic_clean_2024.csv")

write_csv(city_plastic,
          "city_plastic_clean_2024.csv")

#more then 15
country_critical <- country_plastic %>%
  filter(composition_plastic_percent >= 15) %>%
  arrange(desc(composition_plastic_percent))

ggplot(country_critical,
       aes(x = reorder(country_name, composition_plastic_percent),
           y = composition_plastic_percent)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Countries with Plastic Waste > 15% (2024)",
    subtitle = "High-contributing countries driving landfill pressure",
    x = "Country",
    y = "Plastic Waste (%)"
  ) +
  theme_minimal(base_size = 12)


#more then 16

city_critical <- city_plastic %>%
  filter(composition_plastic_percent >= 16) %>%
  arrange(desc(composition_plastic_percent))

ggplot(city_critical,
       aes(x = reorder(paste(city_name, country_name, sep = ", "),
                       composition_plastic_percent),
           y = composition_plastic_percent)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Cities with Plastic Waste > 16% (2024)",
    subtitle = "Urban hotspots contributing to plastic landfill risk",
    x = "City, Country",
    y = "Plastic Waste (%)"
  ) +
  theme_minimal(base_size = 11)


#2025 predictions' 
growth_rate <- 0.03   # 3% annual increase (assumption)

country_future <- country_critical %>%
  mutate(
    plastic_2024 = composition_plastic_percent,
    plastic_2025 = plastic_2024 * (1 + growth_rate),
    plastic_2026 = plastic_2025 * (1 + growth_rate),
    risk_2026 = case_when(
      plastic_2026 < 20 ~ "Low",
      plastic_2026 < 30 ~ "Medium",
      TRUE ~ "High"
    )
  )

country_long <- country_future %>%
  select(country_name, plastic_2024, plastic_2025) %>%
  pivot_longer(
    cols = c(plastic_2024, plastic_2025),
    names_to = "year",
    values_to = "plastic_percent"
  )

ggplot(country_long,
       aes(x = reorder(country_name, plastic_percent),
           y = plastic_percent,
           fill = year)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Plastic Waste Comparison: 2024 vs 2025 (High-Risk Countries)",
    x = "Country",
    y = "Plastic Waste (%)",
    fill = "Year"
  ) +
  theme_minimal(base_size = 12)

#2026 risk based on 2024-25 data

ggplot(country_future,
       aes(x = reorder(country_name, plastic_2026),
           y = plastic_2026,
           fill = risk_2026)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Predicted Landfill Risk due to Plastic Waste – 2026 (Countries)",
    x = "Country",
    y = "Plastic Waste (%)",
    fill = "Risk Level"
  ) +
  theme_minimal(base_size = 12)

#CITIES 2025 predection

city_future <- city_critical %>%
  mutate(
    plastic_2024 = composition_plastic_percent,
    plastic_2025 = plastic_2024 * (1 + growth_rate),
    plastic_2026 = plastic_2025 * (1 + growth_rate),
    risk_2026 = case_when(
      plastic_2026 < 20 ~ "Low",
      plastic_2026 < 30 ~ "Medium",
      TRUE ~ "High"
    )
  )

city_long <- city_future %>%
  mutate(city_country = paste(city_name, country_name, sep = ", ")) %>%
  select(city_country, plastic_2024, plastic_2025) %>%
  pivot_longer(
    cols = c(plastic_2024, plastic_2025),
    names_to = "year",
    values_to = "plastic_percent"
  )

ggplot(city_long,
       aes(x = reorder(city_country, plastic_percent),
           y = plastic_percent,
           fill = year)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Plastic Waste Comparison: 2024 vs 2025 (High-Risk Cities)",
    x = "City, Country",
    y = "Plastic Waste (%)",
    fill = "Year"
  ) +
  theme_minimal(base_size = 11)

#Cities 2026 risk
ggplot(city_future,
       aes(x = reorder(paste(city_name, country_name, sep = ", "),
                       plastic_2026),
           y = plastic_2026,
           fill = risk_2026)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Predicted Landfill Risk due to Plastic Waste – 2026 (Cities)",
    x = "City, Country",
    y = "Plastic Waste (%)",
    fill = "Risk Level"
  ) +
  theme_minimal(base_size = 11)


#save the data
write_csv(country_future, "country_plastic_prediction_2026.csv")
write_csv(city_future, "city_plastic_prediction_2026.csv")
