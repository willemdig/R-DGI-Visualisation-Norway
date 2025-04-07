library(tidyverse)

# -- STEP 1: Read in 2019 and 2023 OURData scores (assumes you've already done this) --

# Example:
# ourdata_clean <- your merged and cleaned dataset with country, year, score

ourdata_clean <- ourdata_2019_2023 %>%
  mutate(
    ourdata_index_2023 = rowMeans(select(., data_availability, data_accessibility, government_support_to_data_reuse), na.rm = TRUE)
  ) %>%
  select(
    country,
    ourdata_index_2019,
    data_availability_2019_rescaled,
    data_accessibility_2019_rescaled,
    gov_support_2019_rescaled,
    ourdata_index_2023,
    data_availability_2023 = data_availability,
    data_accessibility_2023 = data_accessibility,
    gov_support_2023 = government_support_to_data_reuse
  )



# -- STEP 2: Transform to long format for plotting --

plot_data <- ourdata_clean %>%
  select(country, ourdata_index_2019, ourdata_index_2023) %>%
  pivot_longer(
    cols = starts_with("ourdata_index"),
    names_to = "year",
    names_pattern = "ourdata_index_(\\d+)",
    values_to = "score"
  ) %>%
  mutate(year = as.numeric(year))


# -- STEP 3: Dynamically determine countries to include -------------------------

norway_scores <- plot_data %>%
  filter(country == "Norway") %>%
  select(year, norway_score = score)

focus_countries <- plot_data %>%
  left_join(norway_scores, by = "year") %>%
  filter(score > norway_score) %>%
  pull(country) %>%
  unique() %>%
  union("Norway")  # Always include Norway

plot_data <- plot_data %>%
  filter(country %in% focus_countries)

# -- STEP 4: Order countries by 2019 rank (descending) --------------------------

score_order_2019 <- plot_data %>%
  filter(year == 2019) %>%
  arrange(desc(score)) %>%
  pull(country)

plot_data <- plot_data %>%
  mutate(
    country = factor(country, levels = score_order_2019),
    linewidth = if_else(country == "Norway", 2.5,
                        if_else(country == "Denmark", 1.8, 1))
  )

# -- STEP 5: Calculate Norway ranks ---------------------------------------------

norway_2019_rank <- plot_data %>%
  filter(year == 2019) %>%
  arrange(desc(score)) %>%
  mutate(rank = row_number()) %>%
  filter(country == "Norway") %>%
  pull(rank)

norway_2023_rank <- plot_data %>%
  filter(year == 2023) %>%
  arrange(desc(score)) %>%
  mutate(rank = row_number()) %>%
  filter(country == "Norway") %>%
  pull(rank)

# -- STEP 6: Define country colors ----------------------------------------------

country_colors <- c(
  "Norway"   = "#d73027",
  "Denmark"  = "#4575b4",
  "Korea"    = "#FFD700",
  "France"   = "#999999",
  "Ireland"  = "#228B22"
)

# Fill in the rest with grey
missing_countries <- setdiff(levels(plot_data$country), names(country_colors))
country_colors <- c(country_colors, setNames(rep("grey80", length(missing_countries)), missing_countries))

# -- STEP 7: Plot ---------------------------------------------------------------

ggplot(plot_data, aes(x = score, y = year, group = country)) +
  geom_line(aes(color = country, linewidth = linewidth)) +
  geom_point(aes(color = country), size = 3) +
  
  # Norway rank labels
  geom_text(
    data = plot_data %>% filter(country == "Norway", year == 2019),
    aes(label = paste0(norway_2019_rank, ". plass")),
    color = "#d73027", size = 3,
    hjust = 0.5, vjust = 1.6
  ) +
  geom_text(
    data = plot_data %>% filter(country == "Norway", year == 2023),
    aes(label = paste0(norway_2023_rank, ". plass")),
    color = "#d73027", size = 3,
    hjust = -3, vjust = 1.5
  ) +
  
  scale_color_manual(
    values = country_colors,
    guide = guide_legend(ncol = 1)
  ) +
  scale_linewidth_identity() +
  coord_flip() +
  scale_y_continuous(
    breaks = c(2019, 2023),
    limits = c(2018.8, 2023.4)
  ) +
  labs(
    title = "OECD OURData-indeks (2019–2023)",
    subtitle = "Norges posisjon blant ledende land",
    x = "Score",
    y = NULL,
    color = "Land",
    caption = "Kilde: OECD OURData Index 2019 og 2023"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.direction = "vertical",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, color = "grey40", hjust = 0),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5)
  )

