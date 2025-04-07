# Load necessary libraries
library(tidyverse)

#Character standard
Sys.setlocale("LC_CTYPE", "Norwegian_Norway.UTF-8")

# Read the 2019 and 2023 data for Norway, Denmark, UK, SK, and Colombia
countries <- c("NOR", "DNK", "GBR", "KOR", "COL")
data_2019 <- read_csv("OECD_DGI_Full_2019.csv") %>%
  filter(Country %in% countries)  # Filter for selected countries

data_2023 <- read_csv("OECD_DGI_2023_Full.csv") %>%
  filter(Country %in% countries)  # Filter for selected countries

# Manually reshape the 2019 data into long format with Country included
reshape_data <- function(data, year) {
  data_long <- data.frame(
    Dimension = c("Digital_by_Design", "Gov_as_Platform", "Open_by_Default", 
                  "Data_driven", "User_driven", "Proactiveness"),
    Score = c(data$Digital_by_Design_2019, 
              data$Gov_as_Platform_2019,
              data$Open_by_Default_2019, 
              data$Data_driven_2019, 
              data$User_driven_2019, 
              data$Proactiveness_2019),
    Country = data$Country  # Make sure to keep Country in the reshaped data
  )
  mutate(data_long, Year = year, DGI_Score = data$Composite_2019)
}

# Reshape for each country
norway_2019_long <- reshape_data(data_2019 %>% filter(Country == "NOR"), "2019")
denmark_2019_long <- reshape_data(data_2019 %>% filter(Country == "DNK"), "2019")
uk_2019_long <- reshape_data(data_2019 %>% filter(Country == "GBR"), "2019")
sk_2019_long <- reshape_data(data_2019 %>% filter(Country == "KOR"), "2019")
colombia_2019_long <- reshape_data(data_2019 %>% filter(Country == "COL"), "2019")

# Manually reshape the 2023 data into long format for all countries with Country included
reshape_data_2023 <- function(data, year) {
  data_long <- data.frame(
    Dimension = c("Digital_by_Design", "Gov_as_Platform", "Open_by_Default", 
                  "Data_driven", "User_driven", "Proactiveness"),
    Score = c(data$Digital_by_Design_2023, 
              data$Gov_as_Platform_2023,
              data$Open_by_Default_2023, 
              data$Data_driven_2023, 
              data$User_driven_2023, 
              data$Proactiveness_2023),
    Country = data$Country  # Ensure Country column is retained
  )
  mutate(data_long, Year = year, DGI_Score = data$Composite_2023)
}

# Reshape for each country
norway_2023_long <- reshape_data_2023(data_2023 %>% filter(Country == "NOR"), "2023")
denmark_2023_long <- reshape_data_2023(data_2023 %>% filter(Country == "DNK"), "2023")
uk_2023_long <- reshape_data_2023(data_2023 %>% filter(Country == "GBR"), "2023")
sk_2023_long <- reshape_data_2023(data_2023 %>% filter(Country == "KOR"), "2023")
colombia_2023_long <- reshape_data_2023(data_2023 %>% filter(Country == "COL"), "2023")

# Combine all the reshaped data into one dataset
combined_data <- bind_rows(
  norway_2019_long, denmark_2019_long, uk_2019_long, sk_2019_long, colombia_2019_long,
  norway_2023_long, denmark_2023_long, uk_2023_long, sk_2023_long, colombia_2023_long
)

# Calculate the total sum of the dimension scores for each year and normalize
combined_data <- combined_data %>%
  group_by(Year, Country) %>%
  mutate(
    Total_Score = sum(Score),  # Total of dimension scores
    Distribution = Score / Total_Score,  # Calculate the distribution
    Scaled_Score = Distribution * DGI_Score,  # Scale the score to match the DGI score
    Label = sprintf("%.3f (%.1f%%): %.3f", Score, Distribution * 100, Scaled_Score)  # Label with original, proportion, and scaled score
  ) %>%
  ungroup()
# Country names to display below the columns
country_names <- c("NOR" = "Norge", "DNK" = "Danmark", "KOR" = "Sør-Korea", 
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
    title = "DGI-indeksens oppdeling etter land og år",
    subtitle = "Hver søyle representerer indeksen for hvert land, og er delt opp etter de forskjellige dimensjonene i DGI-indeksen.\nDimensjonene er representert med ulike farger, og det vises også hvordan de enkelte dimensjonene bidrar til det totale indekspoengsummen.",
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

