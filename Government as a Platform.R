library(tidyverse)

# Read the data for 2019 and 2023
data_2019 <- read_csv("OECD_DGI_Full_2019.csv")
data_2023 <- read_csv("OECD_DGI_2023_Full.csv")

# Filter the relevant columns for the "Gov_as_Platform" dimension
gov_platform_2019 <- data_2019 %>% select(Country, Gov_as_Platform_2019)
gov_platform_2023 <- data_2023 %>% select(Country, Gov_as_Platform_2023)

# Combine the two years into one dataframe for easier comparison
gov_platform_comparison <- full_join(
  gov_platform_2019 %>% rename(Score_2019 = Gov_as_Platform_2019),
  gov_platform_2023 %>% rename(Score_2023 = Gov_as_Platform_2023),
  by = "Country"
)

# Rank the countries in each year for the "Gov_as_Platform" dimension
gov_platform_comparison <- gov_platform_comparison %>%
  mutate(
    Rank_2019 = rank(-Score_2019, ties.method = "min"),
    Rank_2023 = rank(-Score_2023, ties.method = "min")
  )

# Reorganize the columns as you want: Score 2019, Rank 2019, Score 2023, Rank 2023
gov_platform_comparison <- gov_platform_comparison %>%
  select(Country, Score_2019, Rank_2019, Score_2023, Rank_2023)

# Filter to see the ranking for Norway and Denmark specifically
norway_denmark_ranks <- gov_platform_comparison %>%
  filter(Country %in% c("NOR", "DNK"))

# Print the results for Norway and Denmark in 2019 and 2023
print(norway_denmark_ranks)



# Save the table to an Excel file
write_xlsx(gov_platform_comparison, path = "gov_platform_comparison.xlsx")

# Optionally, if you only want the table for Norway and Denmark
write_xlsx(norway_denmark_ranks, path = "norway_denmark_ranks.xlsx")

