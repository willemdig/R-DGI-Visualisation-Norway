library(tidyverse)

Sys.setlocale("LC_CTYPE", "Norwegian_Norway.UTF-8")

# --- STEP 1: Import data ------------------------------------------------------

availability_2023 <- read_csv("~/ourdata_data_availability.csv")
accessibility_2023 <- read_csv("~/ourdata_data_accessibility.csv")
support_2023 <- read_csv("~/ourdata_government_support.csv")
ourdata_2019 <- read_csv("~/ourdata_index_2019.csv")

# --- STEP 2: Merge and compute 2023 composite index --------------------------

ourdata_2023 <- availability_2023 %>%
  full_join(accessibility_2023, by = "country") %>%
  full_join(support_2023, by = "country") %>%
  mutate(
    ourdata_index = rowMeans(select(., data_availability, data_accessibility, government_support_to_data_reuse), na.rm = TRUE)
  )

# --- STEP 3: Country mapping (to fix names) -----------------------------------

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
  "Turkey",              "TUR"
)

ourdata_composite_2023 <- ourdata_2023 %>%
  left_join(country_mapping, by = c("country" = "country_code")) %>%
  mutate(country = coalesce(country_name, country)) %>%
  select(-country_name)

# --- STEP 4: Join with 2019 and rescale 2019 scores ---------------------------

ourdata_2019_2023 <- ourdata_2019 %>%
  full_join(ourdata_composite_2023, by = "country") %>%
  mutate(
    data_availability_2019_rescaled = data_availability_2019 * 3,
    data_accessibility_2019_rescaled = data_accessibility_2019 * 3,
    gov_support_2019_rescaled = gov_support_2019 * 3
  )

# --- STEP 5: Clean and pivot to long format ----------------------------------

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

# --- STEP 6: Focus on selected countries -------------------------------------

focus_countries <- c("Norway", "Denmark", "Korea", "France", "Ireland")

ourdata_focus <- ourdata_long %>%
  filter(country %in% focus_countries)

ourdata_focus_long <- ourdata_focus %>%
  pivot_longer(cols = c("2019", "2023"), names_to = "year", values_to = "score") %>%
  mutate(
    country = factor(country, levels = focus_countries),
    year = factor(year, levels = c("2019", "2023"))
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

ourdata_focus_long <- ourdata_focus_long %>%
  left_join(country_names_nb, by = c("country" = "english")) %>%
  mutate(
    country_nb = factor(norsk, levels = c("Norge", "Danmark", "Sør-Korea", "Frankrike", "Irland"))
  )

# --- STEP 7: Bring back composite index --------------------------------------

ourdata_focus_long <- ourdata_focus_long %>%
  left_join(
    ourdata_clean %>%
      select(country, ourdata_index_2019, ourdata_index_2023),
    by = "country"
  )

# --- STEP 8: Wide format and scale scores to match index ---------------------

ourdata_wide <- ourdata_focus_long %>%
  pivot_wider(names_from = dimension, values_from = score) %>%
  group_by(country, year, country_nb) %>%
  mutate(
    total_score = data_availability + data_accessibility + gov_support,
    composite_index = case_when(
      year == "2019" ~ ourdata_index_2019,
      year == "2023" ~ ourdata_index_2023
    )
  ) %>%
  ungroup()

ourdata_stacked <- ourdata_wide %>%
  pivot_longer(cols = c(data_availability, data_accessibility, gov_support),
               names_to = "dimension", values_to = "raw_score") %>%
  mutate(
    share = raw_score / total_score,
    scaled_score = share * composite_index
  )

# --- STEP 9: Plot as stacked bar ---------------------------------------------

ggplot(ourdata_stacked, aes(x = country_nb, y = scaled_score, fill = dimension)) +
  geom_col(position = "stack", width = 0.6) +
  facet_wrap(~year) +
  scale_fill_manual(values = c(
    "data_availability" = "#d73027",
    "data_accessibility" = "#4575b4",
    "gov_support" = "#66c2a5"
  ),
  labels = c(
    "data_availability" = "Tilgjengelighet",
    "data_accessibility" = "Tilgang",
    "gov_support" = "Myndighetsstøtte"
  )) +
  labs(
    title = "OURData-indeks oppdelt etter dimensjoner og år",
    subtitle = "Hver søyle viser det totale OURData-indekspoenget, fordelt på tre dimensjoner",
    x = "Land",
    y = "Skalert poengsum",
    fill = "Dimensjon",
    caption = "Kilde: OECD OURData Index"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    axis.text.x = element_text(size = 12),
    strip.text = element_text(face = "bold")
  ) +
  geom_text(aes(label = sprintf("%.3f\n(%.2f)", raw_score, scaled_score)),
            position = position_stack(vjust = 0.5), size = 3.5, lineheight = 0.95) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 0.6)


# --- Optional: Save plot -----------------------------------------------------

ggsave(
  filename = "ourdata_dgi_style_plot.png",
  width = 10,
  height = 6,
  dpi = 300
)
