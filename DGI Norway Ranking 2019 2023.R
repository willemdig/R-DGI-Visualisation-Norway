library(tidyverse)
#library(ggflags)
library(ggimage)

Sys.setlocale("LC_CTYPE", "Norwegian_Norway.UTF-8")

# Load 2023 and 2019 data
data_2023 <- read_csv("OECD_DGI_2023_Full.csv") %>%
  select(Country, Composite_2023)

data_2019 <- read_csv("OECD_DGI_Full_2019.csv") %>%
  select(Country, Composite_2019)

# Join and reshape
data_combined <- data_2023 %>%
  left_join(data_2019, by = "Country") %>%
  pivot_longer(
    cols = starts_with("Composite_"),
    names_to = "Year",
    names_prefix = "Composite_",
    values_to = "Score"
  )

# Country selection
nor_score_2019 <- data_2019 %>% filter(Country == "NOR") %>% pull(Composite_2019)
better_2019 <- data_2019 %>% filter(Composite_2019 > nor_score_2019) %>% pull(Country)
top5_2023 <- data_2023 %>% arrange(desc(Composite_2023)) %>% slice_head(n = 5) %>% pull(Country)
countries_to_plot <- union(better_2019, top5_2023) %>% union("NOR")

plot_data <- data_combined %>% filter(Country %in% countries_to_plot)

#Flags

geom_text(
  data = plot_data %>% filter(Year == "2023"),
  aes(label = case_when(
    Country == "NOR" ~ "🇳🇴",
    Country == "DNK" ~ "🇩🇰",
    Country == "SWE" ~ "🇸🇪",
    Country == "FIN" ~ "🇫🇮",
    Country == "GBR" ~ "🇬🇧",
    Country == "KOR" ~ "🇰🇷",
    Country == "COL" ~ "🇨🇴",
    Country == "CAN" ~ "🇨🇦",
    Country == "EST" ~ "🇪🇪",
    Country == "FRA" ~ "🇫🇷",
    Country == "PRT" ~ "🇵🇹",
    Country == "NZL" ~ "🇳🇿",
    TRUE ~ Country
  )),
  vjust = -1,
  size = 6
)


# Add color groupings for highlighting
plot_data <- plot_data %>%
  mutate(color_group = case_when(
    Country == "NOR" ~ "NOR",
    Country == "DNK" ~ "DNK",
    Country == "SWE" ~ "SWE",
    Country == "FIN" ~ "FIN",
    TRUE ~ "default"
  ))

library(ggimage)
library(httr)

# Add ISO2 codes and flag URLs
plot_data <- plot_data %>%
  mutate(
    iso2 = case_when(
      Country == "GBR" ~ "gb",
      Country == "KOR" ~ "kr",
      Country == "COL" ~ "co",
      Country == "EST" ~ "ee",
      Country == "PRT" ~ "pt",
      Country == "NZL" ~ "nz",
      Country == "CAN" ~ "ca",
      Country == "FRA" ~ "fr",
      Country == "FIN" ~ "fi",
      Country == "DNK" ~ "dk",
      Country == "SWE" ~ "se",
      Country == "NOR" ~ "no",
      TRUE ~ tolower(Country)
    ),
    flag_url = paste0("https://flagcdn.com/w80/", iso2, ".png"),
    Year = as.numeric(Year)
  )

# Filter only rows where flag URL works (no 404s)
plot_data <- plot_data %>%
  mutate(flag_valid = !sapply(flag_url, httr::http_error)) %>%
  filter(flag_valid)

# Plot
ggplot(plot_data, aes(x = Score, y = Year, group = Country)) +
  geom_line(aes(
    color = color_group,
    linewidth = if_else(Country == "NOR", 2.5,
                        if_else(Country %in% c("DNK", "SWE", "FIN"), 1.5, 1))
  )) +
  geom_point(aes(color = color_group), size = 3) +
  geom_image(
    data = plot_data %>% filter(Year == 2023),
    aes(
      y = Year,
      x = case_when(
        Country == "GBR" ~ Score + 0.01,
        TRUE ~ Score
      ),
      image = flag_url
    ),
    size = 0.045,
    asp = 1.2
  ) +
  # Norway rank labels — now placed below the points
  geom_text(
    data = plot_data %>% filter(Country == "NOR", Year == 2019),
    aes(label = "13. plass"),
    x = plot_data %>% filter(Country == "NOR", Year == 2019) %>% pull(Score),
    y = 2019,
    color = I("#d73027"),
    hjust = 0.5,
    vjust = 1.5,
    size = 3
  ) +
  geom_text(
    data = plot_data %>% filter(Country == "NOR", Year == 2023),
    aes(label = "4. plass"),
    x = plot_data %>% filter(Country == "NOR", Year == 2023) %>% pull(Score),
    y = 2023,
    color = I("#d73027"),
    hjust = 2.6,
    vjust = 1.5,
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "NOR" = "#d73027",
      "DNK" = "#4575b4",
      "SWE" = "#91bfdb",
      "FIN" = "#66c2a5",
      "default" = "grey70"
    )
  ) +
  scale_linewidth_identity() +
  labs(
    title = "OECD DGI Composite Score (2019–2023)",
    subtitle = "Norges posisjon iblandt ledende land i samlet DGI-indeksen",
    y = NULL,
    caption = "Kilde: OECD Digital Government Index 2019 og 2023"
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = c(2019, 2023),
    limits = c(2018.8, 2023.4)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, color = "grey40", hjust = 0)
  )


ggsave("dgi_norway_plot.png", width = 10, height = 6, dpi = 300)
