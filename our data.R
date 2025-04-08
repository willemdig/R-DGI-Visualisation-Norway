library(tidyverse)

Sys.setlocale("LC_CTYPE", "Norwegian_Norway.UTF-8")

# -- STEP 1: Import data ------------------------------------------------------

# 2023
availability_2023 <- read_csv("~/ourdata_data_availability.csv")
accessibility_2023 <- read_csv("~/ourdata_data_accessibility.csv")
support_2023 <- read_csv("~/ourdata_government_support.csv")

# 2019
ourdata_2019 <- read_csv("~/ourdata_index_2019.csv")

# -- STEP 2: Merge and compute 2023 composite index --------------------------

ourdata_2023 <- availability_2023 %>%
  full_join(accessibility_2023, by = "country") %>%
  full_join(support_2023, by = "country") %>%
  mutate(
    ourdata_index = rowMeans(select(., data_availability, data_accessibility, government_support_to_data_reuse), na.rm = TRUE)
  )

# -- STEP 3: Harmonize country codes with 2019 names -------------------------

country_mapping <- tribble(
  ~country_name,         ~country_code,
  "Australia",           "AUS",
  "Austria",             "AUT",
  "Belgium",             "BEL",
  "Brazil",              "BRA",
  "Canada",              "CAN",
  "Switzerland",         "CHE",
  "Chile",               "CHL",
  "Colombia",            "COL",
  "Costa Rica",          "CRI",
  "Czech Republic",      "CZE",
  "Germany",             "DEU",
  "Denmark",             "DNK",
  "Spain",               "ESP",
  "Estonia",             "EST",
  "Finland",             "FIN",
  "France",              "FRA",
  "United Kingdom",      "GBR",
  "Greece",              "GRC",
  "Croatia",             "HRV",
  "Iceland",             "ISL",
  "Ireland",             "IRE",
  "Israel",              "ISR",
  "Italy",               "ITA",
  "Japan",               "JPN",
  "Korea",               "KOR",
  "Lithuania",           "LTU",
  "Luxembourg",          "LUX",
  "Latvia",              "LVA",
  "Mexico",              "MEX",
  "Netherlands",         "NLD",
  "Norway",              "NOR",
  "New Zealand",         "NZL",
  "Peru",                "PER",
  "Poland",              "POL",
  "Portugal",            "PRT",
  "Romania",             "ROU",
  "Slovak Republic",     "SVK",
  "Slovenia",            "SVN",
  "Sweden",              "SWE",
  "Turkey",              "TUR",
  "Lithuania (alt)",     "LTH",
  "Iceland (alt)",       "ICE"
)



ourdata_composite_2023 <- ourdata_2023 %>%
  left_join(country_mapping, by = c("country" = "country_code")) %>%
  mutate(country = coalesce(country_name, country)) %>%
  select(-country_name)

# -- STEP 4: Join with 2019 and RESCALE 2019 pillar values ------------------

# NOTE: The 2019 pillar scores are originally scaled 0–0.33. We multiply them by 3 to match 2023 scale (0–1).
ourdata_2019_2023 <- ourdata_2019 %>%
  full_join(ourdata_composite_2023, by = "country") %>%
  mutate(
    data_availability_2019_rescaled = data_availability_2019 * 3,
    data_accessibility_2019_rescaled = data_accessibility_2019 * 3,
    gov_support_2019_rescaled = gov_support_2019 * 3
  )

# -- STEP 5: Clean and pivot for plotting ------------------------------------

ourdata_clean <- ourdata_2019_2023 %>%
  select(
    country,
    ourdata_index_2019,
    data_availability_2019_rescaled,
    data_accessibility_2019_rescaled,
    gov_support_2019_rescaled,
    ourdata_index_2023 = ourdata_index,
    data_availability_2023 = data_availability,
    data_accessibility_2023 = data_accessibility,
    gov_support_2023 = government_support_to_data_reuse
  )

ourdata_long <- ourdata_clean %>%
  pivot_longer(
    cols = -country,
    names_to = c("dimension", "year"),
    names_pattern = "(.*)_(2019|2023)",
    values_to = "score"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = score
  ) %>%
  relocate(dimension, .after = country)

# -- STEP 6: Focus on selected countries --------------------------------------

focus_countries <- c("Norway", "Denmark", "Korea", "France", "Ireland")

ourdata_focus <- ourdata_long %>%
  filter(country %in% focus_countries)

# Prepare for grouped bar chart
ourdata_focus_long <- ourdata_focus %>%
  pivot_longer(cols = c("2019", "2023"), names_to = "year", values_to = "score") %>%
  mutate(
    country = factor(country, levels = focus_countries),
    year = factor(year, levels = c("2019", "2023"))
  )



country_names_nb <- tribble(
  ~english,             ~norsk,
  "Norway",             "Norge",
  "Denmark",            "Danmark",
  "Korea",              "Sør-Korea",
  "France",             "Frankrike",
  "Ireland",            "Irland"
)


ourdata_focus_long <- ourdata_focus_long %>%
  left_join(country_names_nb, by = c("country" = "english")) %>%
  mutate(
    country_nb = factor(norsk, levels = c("Norge", "Danmark", "Sør-Korea", "Frankrike", "Irland"))
  )


# Add Norwegian country names
country_names_nb <- tribble(
  ~english,             ~norsk,
  "Norway",             "Norge",
  "Denmark",            "Danmark",
  "Korea",              "Sør-Korea",
  "France",             "Frankrike",
  "Ireland",            "Irland"
)

ggplot(ourdata_focus_long, aes(x = country_nb, y = score, fill = year)) +
  geom_col(position = "dodge", width = 0.6) +
  facet_wrap(~dimension, scales = "free_y") +
  scale_fill_manual(values = c("2019" = "gray70", "2023" = "steelblue")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "OURData-indeks og dimensjoner: 2019 vs 2023",
    subtitle = "Topp tre land i 2019 + Norge og Danmark",
    x = "Land",
    y = "Score",
    fill = "År"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 12),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    plot.margin = margin(t = 30, r = 20, b = 20, l = 20)
  )

ggsave(
  filename = "ourdata_norge_danmark_plot.png",
  width = 10,
  height = 6,
  dpi = 300
)


write_csv(ourdata_2023, "ourdata_2023_full.csv")
write_csv(ourdata_2019, "ourdata_2019_full.csv")

