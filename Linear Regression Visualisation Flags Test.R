library(tidyverse)
library(ggimage)

# Filter data for Norway, South Korea, and Denmark
norway_data <- data_combined %>% filter(Country == "NOR")
sk_data <- data_combined %>% filter(Country == "KOR")
denmark_data <- data_combined %>% filter(Country == "DNK")

# Convert Year to numeric (if it's currently a character or factor)
norway_data$Year <- as.numeric(as.character(norway_data$Year))
sk_data$Year <- as.numeric(as.character(sk_data$Year))
denmark_data$Year <- as.numeric(as.character(denmark_data$Year))

# Fit the linear regression model for Norway
lm_norway <- lm(Score ~ Year, data = norway_data)

# Fit the linear regression model for South Korea
lm_sk <- lm(Score ~ Year, data = sk_data)

# Fit the linear regression model for Denmark
lm_denmark <- lm(Score ~ Year, data = denmark_data)

# Make predictions for the future years (2024-2030) for all countries
future_years <- data.frame(Year = 2023:2030)
future_predictions_norway <- predict(lm_norway, newdata = future_years)
future_predictions_sk <- predict(lm_sk, newdata = future_years)
future_predictions_denmark <- predict(lm_denmark, newdata = future_years)

# Get the 2023 value for South Korea
sk_2023_score <- sk_data %>% filter(Year == 2023) %>% pull(Score)

# Add ISO2 country codes for the flags
country_flags <- data.frame(
  Country = c("NOR", "KOR", "DNK"),
  flag_url = c("https://flagcdn.com/w80/no.png", 
               "https://flagcdn.com/w80/kr.png", 
               "https://flagcdn.com/w80/dk.png")
)

# Combine flag information into plot_data
norway_data <- norway_data %>%
  left_join(country_flags, by = "Country")
sk_data <- sk_data %>%
  left_join(country_flags, by = "Country")
denmark_data <- denmark_data %>%
  left_join(country_flags, by = "Country")

# Create the plot
ggplot() +
  # Plot historical data for Norway, South Korea, and Denmark
  geom_point(data = norway_data, aes(x = Year, y = Score), color = 'red', size = 3) +
  geom_point(data = sk_data, aes(x = Year, y = Score), color = 'orange', size = 3) +
  geom_point(data = denmark_data, aes(x = Year, y = Score), color = 'green', size = 3) +
  
  # Linear regression line for Norway
  geom_smooth(data = norway_data, aes(x = Year, y = Score), method = "lm", se = FALSE, color = "red") +
  
  # Linear regression line for South Korea
  geom_smooth(data = sk_data, aes(x = Year, y = Score), method = "lm", se = FALSE, color = "orange") +
  
  # Linear regression line for Denmark
  geom_smooth(data = denmark_data, aes(x = Year, y = Score), method = "lm", se = FALSE, color = "green") +
  
  # Predicted future scores for Norway
  geom_line(data = future_years, aes(x = Year, y = future_predictions_norway), color = "red", linetype = "dashed") +
  
  # Predicted future scores for South Korea
  geom_line(data = future_years, aes(x = Year, y = future_predictions_sk), color = "orange", linetype = "dashed") +
  
  # Predicted future scores for Denmark
  geom_line(data = future_years, aes(x = Year, y = future_predictions_denmark), color = "green", linetype = "dashed") +
  
  # Add the 2023 data as points (if it's not already present in the plot)
  geom_point(data = data_combined %>% filter(!Country %in% c("NOR", "KOR", "DNK")), aes(x = Year, y = Score), color = "black", size = 1) +
  
  # Add a horizontal line for South Korea's 2023 score
  geom_hline(yintercept = sk_2023_score, linetype = "dotted", color = "orange", size = 1) +
  
  # Add a big black line at y = 1 to mark the maximum DGI score
  geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
  
  # Labels and titles
  labs(
    title = "Lineær prognose av DGI-Indeks",
    subtitle = "Basert på historisk data (2019, 2023), gult: Korea, rødt: Norge, grønt: Danmark",
    x = "År",
    y = "DGI Score"
  ) +
  scale_x_continuous(
    breaks = seq(2019, 2030, by = 1),  # Set x-axis breaks to show every year from 2019 to 2030
    limits = c(2019, 2030)  # Ensure the x-axis goes from 2019 to 2030
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5), # Center the title
    plot.subtitle = element_text(hjust = 0.5)  # Center the title
  ) +
  # Add flags as images
  geom_image(data = norway_data, aes(x = Year, y = Score, image = flag_url), size = 0.03, asp = 1.2) +
  geom_image(data = sk_data, aes(x = Year, y = Score, image = flag_url), size = 0.03, asp = 1.2) +
  geom_image(data = denmark_data, aes(x = Year, y = Score, image = flag_url), size = 0.03, asp = 1.2)
