library(ggplot2)
library(dplyr)

# Country names to display below the columns
country_names <- c("NOR" = "Norge", "DNK" = "Danmark", "KOR" = "S??r-Korea", 
                   "GBR" = "Storbritannia", "COL" = "Colombia")

# Set the country order as a factor
combined_data$Country <- factor(combined_data$Country, levels = c("NOR", "DNK", "KOR", "GBR", "COL"))

# Plot the stacked bar for each country, year by year, with country names below the x-axis
ggplot(combined_data, aes(x = Country, y = Scaled_Score, fill = Dimension)) +
  geom_bar(stat = "identity", position = "stack") +  # Stack the dimensions
  facet_wrap(~ Year, scales = "free_x") +  # Separate plots for each year
  scale_fill_manual(values = c(
    "Digital_by_Design" = "#d73027", 
    "Gov_as_Platform" = "#4575b4", 
    "Open_by_Default" = "#91bfdb",
    "Data_driven" = "#66c2a5", 
    "User_driven" = "#fee08b",
    "Proactiveness" = "#fdae61"
  )) +
  labs(
    title = "DGI-indeksens oppdeling etter land og ??r",
    subtitle = "Hver s??yle representerer indeksen for hvert land, og er delt opp etter de forskjellige dimensjonene i DGI-indeksen.\nHver dimensjon vises i ulike farger, og det vises ogs?? hvordan de enkelte dimensjonene bidrar til det totale indekspoengsummen.",
    x = "Land",
    y = "Skalert poengsum",
    caption = "Kilde: OECD Digital Government Index"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Make country names horizontal
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "top",  # Position the legend at the top
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  # Show dimension scores and scaled scores inside the bars
  geom_text(aes(label = sprintf("%.3f (%-.3f)", Score, Scaled_Score)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Smaller text for labels
  # Add country names below the x-axis
  geom_text(aes(x = Country, y = -0.02, label = country_names[Country]), 
            size = 4, color = "black", vjust = 1)  # Country names below x-axis
