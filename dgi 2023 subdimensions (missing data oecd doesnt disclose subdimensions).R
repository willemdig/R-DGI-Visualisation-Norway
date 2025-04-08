#DGI 2023 subdimensions


library(tidyverse)
library(RColorBrewer)

# ---- STEP 1: Filter for 2023 only -------------------------------------
dgi_2023 <- combined_data %>% 
  filter(Year == "2023") %>%
  mutate(Dimension_nb = recode(Dimension,
                               "Digital_by_Design" = "Digitalt som designprinsipp",
                               "Gov_as_Platform" = "Myndighet som plattform",
                               "Open_by_Default" = "Åpent som standard",
                               "Data_driven" = "Datadrevet",
                               "User_driven" = "Brukerorientert",
                               "Proactiveness" = "Proaktivitet"
  ))

# ---- STEP 2: Set common styles ----------------------------------------

country_names_nb <- c("NOR" = "Norge", "DNK" = "Danmark", "KOR" = "Sør-Korea", 
                      "GBR" = "Storbritannia", "COL" = "Colombia")

dgi_2023 <- dgi_2023 %>%
  mutate(country_nb = country_names_nb[Country],
         country_nb = factor(country_nb, levels = c("Norge", "Danmark", "Sør-Korea", "Storbritannia", "Colombia")))

dimension_colors <- c(
  "Digitalt som designprinsipp" = "#d73027",    # red
  "Myndighet som plattform"     = "#4575b4",    # blue
  "Åpent som standard"          = "#91bfdb",    # light blue
  "Datadrevet"                  = "#66c2a5",    # greenish
  "Brukerorientert"             = "#fee08b",    # yellow
  "Proaktivitet"                = "#fdae61"     # orange
)

# Color fill palettes for bar fill
fill_palette <- RColorBrewer::brewer.pal(8, "Set2")

# ---- STEP 3: Function to plot each DGI subdimension ------------------

plot_dgi_dimension <- function(dimension_label, outline_color, file_name) {
  p <- ggplot(
    dgi_2023 %>% filter(Dimension_nb == dimension_label),
    aes(x = country_nb, y = Scaled_Score, fill = country_nb)
  ) +
    geom_col(position = "stack", color = outline_color, linewidth = 0.6) +
    scale_fill_manual(values = fill_palette) +
    labs(
      title = paste("DGI 2023 –", dimension_label),
      subtitle = "Bidrag til total DGI-score for denne dimensjonen",
      x = "Land",
      y = "Skalert poengsum",
      fill = "Land"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 13),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(file_name, plot = p, width = 9, height = 5.5, dpi = 300, bg = "white")
  return(p)
}

# ---- STEP 4: Create and save plots for each DGI dimension ------------

plot_dgi_dimension("Digitalt som designprinsipp", "#d73027", "dgi_2023_digital_by_design.png")
plot_dgi_dimension("Myndighet som plattform", "#4575b4", "dgi_2023_gov_as_platform.png")
plot_dgi_dimension("Åpent som standard", "#91bfdb", "dgi_2023_open_by_default.png")
plot_dgi_dimension("Datadrevet", "#66c2a5", "dgi_2023_data_driven.png")
plot_dgi_dimension("Brukerorientert", "#fee08b", "dgi_2023_user_driven.png")
plot_dgi_dimension("Proaktivitet", "#fdae61", "dgi_2023_proactiveness.png")
