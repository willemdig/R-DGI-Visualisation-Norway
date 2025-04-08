library(tidyverse)
library(RColorBrewer)

# Color palette for subdimensions
sub_palette <- brewer.pal(8, "Set2")[1:3]

# ---- PLOT: Tilgjengelighet ----

plot_tilgjengelighet <- ggplot(availability, aes(x = country_nb, y = scaled_score, fill = label)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = sub_palette) +
  labs(
    title = "OURData 2023 – Tilgjengelighet",
    subtitle = "Subdimensjoner skalert til totalscore for tilgjengelighet",
    x = "Land", y = "Skalert poengsum", fill = "Underindikator"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = "ourdata_2023_tilgjengelighet.png",
  plot = plot_tilgjengelighet,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# ---- PLOT: Tilgang ----

plot_tilgang <- ggplot(accessibility, aes(x = country_nb, y = scaled_score, fill = label)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = sub_palette) +
  labs(
    title = "OURData 2023 – Tilgang",
    subtitle = "Subdimensjoner skalert til totalscore for tilgang",
    x = "Land", y = "Skalert poengsum", fill = "Underindikator"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = "ourdata_2023_tilgang.png",
  plot = plot_tilgang,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# ---- PLOT: Myndighetsstøtte ----

plot_stotte <- ggplot(support, aes(x = country_nb, y = scaled_score, fill = label)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = sub_palette) +
  labs(
    title = "OURData 2023 – Myndighetsstøtte til gjenbruk",
    subtitle = "Subdimensjoner skalert til totalscore for myndighetsstøtte",
    x = "Land", y = "Skalert poengsum", fill = "Underindikator"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = "ourdata_2023_stotte.png",
  plot = plot_stotte,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


write_csv(availability, "ourdata_2023_tilgjengelighet.csv")
write_csv(accessibility, "ourdata_2023_tilgang.csv")
write_csv(support, "ourdata_2023_myndighetsstotte.csv")
