library(tidyverse)
library(kableExtra)
library(officer)
library(flextable)
library(writexl)
library(openxlsx)

# Read the data for 2019 and 2023
data_2019 <- read_csv("OECD_DGI_Full_2019.csv")
data_2023 <- read_csv("OECD_DGI_2023_Full.csv")

# Rename columns to remove the year suffix and make them consistent
data_2019 <- data_2019 %>%
  rename(
    Digital_by_design = Digital_by_Design_2019,
    Data_driven = Data_driven_2019,
    Gov_as_Platform = Gov_as_Platform_2019,
    Open_by_Default = Open_by_Default_2019,
    User_driven = User_driven_2019,
    Proactiveness = Proactiveness_2019
  )

data_2023 <- data_2023 %>%
  rename(
    Digital_by_design = Digital_by_Design_2023,
    Data_driven = Data_driven_2023,
    Gov_as_Platform = Gov_as_Platform_2023,
    Open_by_Default = Open_by_Default_2023,
    User_driven = User_driven_2023,
    Proactiveness = Proactiveness_2023
  )

# Find Norway's position in each dimension in 2019
norway_2019_rank <- data_2019 %>%
  select(Country, Digital_by_design, Data_driven, Gov_as_Platform,
         Open_by_Default, User_driven, Proactiveness) %>%
  pivot_longer(cols = starts_with("Digital") | starts_with("Data") | starts_with("Gov") | 
                 starts_with("Open") | starts_with("User") | starts_with("Proactiveness"),
               names_to = "Dimension", values_to = "Score") %>%
  group_by(Dimension) %>%
  mutate(Rank_2019 = rank(-Score, ties.method = "min")) %>%
  filter(Country == "NOR") %>%
  ungroup()

# Find Norway's position in each dimension in 2023
norway_2023_rank <- data_2023 %>%
  select(Country, Digital_by_design, Data_driven, Gov_as_Platform,
         Open_by_Default, User_driven, Proactiveness) %>%
  pivot_longer(cols = starts_with("Digital") | starts_with("Data") | starts_with("Gov") | 
                 starts_with("Open") | starts_with("User") | starts_with("Proactiveness"),
               names_to = "Dimension", values_to = "Score") %>%
  group_by(Dimension) %>%
  mutate(Rank_2023 = rank(-Score, ties.method = "min")) %>%
  filter(Country == "NOR") %>%
  ungroup()

# Find Denmark's position in each dimension in 2019
denmark_2019_rank <- data_2019 %>%
  select(Country, Digital_by_design, Data_driven, Gov_as_Platform,
         Open_by_Default, User_driven, Proactiveness) %>%
  pivot_longer(cols = starts_with("Digital") | starts_with("Data") | starts_with("Gov") | 
                 starts_with("Open") | starts_with("User") | starts_with("Proactiveness"),
               names_to = "Dimension", values_to = "Score") %>%
  group_by(Dimension) %>%
  mutate(Rank_2019 = rank(-Score, ties.method = "min")) %>%
  filter(Country == "DNK") %>%
  ungroup()

# Find Denmark's position in each dimension in 2023
denmark_2023_rank <- data_2023 %>%
  select(Country, Digital_by_design, Data_driven, Gov_as_Platform,
         Open_by_Default, User_driven, Proactiveness) %>%
  pivot_longer(cols = starts_with("Digital") | starts_with("Data") | starts_with("Gov") | 
                 starts_with("Open") | starts_with("User") | starts_with("Proactiveness"),
               names_to = "Dimension", values_to = "Score") %>%
  group_by(Dimension) %>%
  mutate(Rank_2023 = rank(-Score, ties.method = "min")) %>%
  filter(Country == "DNK") %>%
  ungroup()

# Combine Norway and Denmark data for both years into one table
norway_denmark_rank_table <- full_join(
  norway_2019_rank %>% select(Dimension, Rank_2019),
  norway_2023_rank %>% select(Dimension, Rank_2023),
  by = "Dimension"
) %>%
  full_join(
    denmark_2019_rank %>% select(Dimension, Rank_2019) %>% rename(Rank_2019_Denmark = Rank_2019),
    by = "Dimension"
  ) %>%
  full_join(
    denmark_2023_rank %>% select(Dimension, Rank_2023) %>% rename(Rank_2023_Denmark = Rank_2023),
    by = "Dimension"
  )

# Convert Rank to a natural number (remove the decimal part)
norway_denmark_rank_table$Rank_2019 <- as.integer(norway_denmark_rank_table$Rank_2019)
norway_denmark_rank_table$Rank_2023 <- as.integer(norway_denmark_rank_table$Rank_2023)
norway_denmark_rank_table$Rank_2019_Denmark <- as.integer(norway_denmark_rank_table$Rank_2019_Denmark)
norway_denmark_rank_table$Rank_2023_Denmark <- as.integer(norway_denmark_rank_table$Rank_2023_Denmark)

library(openxlsx)

# Assuming 'norway_denmark_rank_table' is already created

# Rename columns to match your request
colnames(norway_denmark_rank_table) <- c(
  "Dimension",
  "Rank_2019_NO",
  "Rank_2023_NO",
  "Rank_2019_DK",
  "Rank_2023_DK"
)

# Create a new workbook
wb <- createWorkbook()

# Add a worksheet
addWorksheet(wb, "DGI Rank Comparison")

# Write the data to the worksheet
writeData(wb, "DGI Rank Comparison", norway_denmark_rank_table, startCol = 1, startRow = 1)

# Style for header (centered, bold text, light grey background)
headerStyle <- createStyle(
  fontSize = 12, 
  textDecoration = "bold",  # Bold text for the header
  halign = "center", 
  fgFill = "#D9D9D9",  # Light grey background for the header
  border = "TopBottomLeftRight", 
  borderColour = "black", 
  borderStyle = "thin"
)

# Apply header style
addStyle(wb, "DGI Rank Comparison", headerStyle, rows = 1, cols = 1:ncol(norway_denmark_rank_table), gridExpand = TRUE)

# Style for the rest of the table (centered text, borders, and grey fill for alternate rows)
tableStyle <- createStyle(
  fontSize = 10,
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight",
  borderColour = "black",
  borderStyle = "thin",
  fgFill = "#F2F2F2"  # Light grey background for the cells
)

# Apply table style to all data (centered and borders)
addStyle(wb, "DGI Rank Comparison", tableStyle, rows = 2:nrow(norway_denmark_rank_table) + 1, cols = 1:ncol(norway_denmark_rank_table), gridExpand = TRUE)

# Add alternating row colors (lighter grey for every alternate row)
alternateRowStyle <- createStyle(
  fgFill = "#F9F9F9",  # Even lighter grey for alternating rows
  halign = "center",  # Center text for alternating rows
  valign = "center",  # Center vertically for alternating rows
  border = "TopBottomLeftRight",
  borderColour = "black",
  borderStyle = "thin"
)

# Apply alternating row color and ensure borders are included for alternating rows
addStyle(wb, "DGI Rank Comparison", alternateRowStyle, rows = seq(2, nrow(norway_denmark_rank_table) + 1, by = 2), cols = 1:ncol(norway_denmark_rank_table), gridExpand = TRUE)

# Set column width for better presentation
setColWidths(wb, "DGI Rank Comparison", cols = 1:ncol(norway_denmark_rank_table), widths = 20)

# Save the workbook to file
saveWorkbook(wb, "DGI_Rank_Comparison_Refined.xlsx", overwrite = TRUE)

# Confirmation message
cat("Table has been saved as 'DGI_Rank_Comparison_Refined.xlsx'.")
