rm(list = ls())

# === Install required packages ===
install.packages(c("readxl", "dplyr", "purrr", "stringr", "lubridate"))

# === Load libraries ===
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

# === Step 1: Define and unzip ===
zip_path <- "Trade_Data_zip.zip"  # Update if in subfolder
extract_dir <- tempfile()
unzip(zipfile = zip_path, exdir = extract_dir)

# === Step 2: List all Excel files ===
excel_files <- list.files(extract_dir, pattern = "\\.xlsx?$", full.names = TRUE, recursive = TRUE)

# === Step 3: Function to identify trade type from filename ===
get_trade_type <- function(filename) {
  name <- tolower(basename(filename))
  if (str_detect(name, "otschap_eutots_arr")) return("EU import")
  if (str_detect(name, "otschap_eutots_dis")) return("EU export")
  if (str_detect(name, "ots_imp")) return("Non-EU import")
  if (str_detect(name, "ots_exp")) return("Non-EU export")
  return("Unknown")
}

# === Step 4: Function to read and clean a single Excel file ===
read_and_clean_excel <- function(file_path) {
  df <- tryCatch({
    read_excel(file_path, skip = 2)
  }, error = function(e) {
    message(paste("Failed to read:", file_path))
    return(NULL)
  })
  
  if (is.null(df) || ncol(df) < 3) return(NULL)
  
  col3_title <- names(df)[3]
  date_parsed <- suppressWarnings(parse_date_time(col3_title, orders = c("B Y", "b Y"), locale = "en"))
  
  if (is.na(date_parsed)) {
    message(paste("Could not parse date from column name in:", file_path))
    return(NULL)
  }
  
  formatted_date <- format(date_parsed, "%m-%Y")
  month <- as.integer(format(date_parsed, "%m"))
  year <- as.integer(format(date_parsed, "%Y"))
  
  cleaned_df <- df[, c(2, 3)]
  names(cleaned_df) <- c("Code", "Value")
  
  cleaned_df <- cleaned_df %>%
    mutate(
      Date = formatted_date,
      Month = month,
      Year = year,
      Trade_Type = get_trade_type(file_path)
    )
  
  return(cleaned_df)
}

# === Step 5: Apply to all files and combine ===
combined_data <- map_dfr(excel_files, read_and_clean_excel)

# === Step 6: Preview combined data ===
glimpse(combined_data)

# === Step 7: Summary check ===
trade_summary <- combined_data %>%
  count(Trade_Type, name = "Count")
print(trade_summary)

# === Step 7.5: Remove rows with NA in 'Code' or 'Value' ===
combined_data <- combined_data %>%
  filter(!is.na(Code), !is.na(Value))

# === Step 8: Load tariff data ===
tariff_path <- "Tarriff data.xlsx"
tariff_data <- read_excel(tariff_path)

# === Step 9: Clean tariff data ===
tariff_clean <- tariff_data %>%
  select(
    year = 3,
    simple_average = 10,
    trade_weighted = 11,
    HS = 13
  ) %>%
  mutate(year = as.character(year))

# === Step 10: Extract year from combined_data$Date and convert to character ===
combined_data <- combined_data %>%
  mutate(
    year = str_sub(Date, start = 4),  # Extract YYYY
    year = as.character(year)
  )

# === Step 11: Join tariff data ===
combined_data <- combined_data %>%
  left_join(
    tariff_clean,
    by = c("Code" = "HS", "year" = "year"),
    relationship = "many-to-many"  # Optional: to silence warning
  )

# === Step 12: Reorder new columns ===
combined_data <- combined_data %>%
  relocate(simple_average, trade_weighted, .after = Value)

# === Step 13: Final check ===
glimpse(combined_data)

# Count how many rows have NA in either simple_average or trade_weighted
na_counts <- combined_data %>%
  summarise(
    simple_average_NA = sum(is.na(simple_average)),
    trade_weighted_NA = sum(is.na(trade_weighted)),
    both_NA = sum(is.na(simple_average) & is.na(trade_weighted))
  )

print(na_counts)

# Remove rows with NA in either 'simple_average' or 'trade_weighted'
combined_data_clean <- combined_data %>%
  filter(!is.na(simple_average), !is.na(trade_weighted))

# Summary statistics for tariff columns
tariff_summary <- combined_data_clean %>%
  summarise(
    simple_avg_min = min(simple_average, na.rm = TRUE),
    simple_avg_median = median(simple_average, na.rm = TRUE),
    simple_avg_mean = mean(simple_average, na.rm = TRUE),
    simple_avg_max = max(simple_average, na.rm = TRUE),
    trade_weighted_min = min(trade_weighted, na.rm = TRUE),
    trade_weighted_median = median(trade_weighted, na.rm = TRUE),
    trade_weighted_mean = mean(trade_weighted, na.rm = TRUE),
    trade_weighted_max = max(trade_weighted, na.rm = TRUE)
  )

print(tariff_summary)

# Quantile-based summary
quantiles <- combined_data_clean %>%
  summarise(
    simple_avg_Q1 = quantile(simple_average, 0.25, na.rm = TRUE),
    simple_avg_Q3 = quantile(simple_average, 0.75, na.rm = TRUE),
    trade_weighted_Q1 = quantile(trade_weighted, 0.25, na.rm = TRUE),
    trade_weighted_Q3 = quantile(trade_weighted, 0.75, na.rm = TRUE)
  )

print(quantiles)

# === Add tariff exposure categories ===
combined_data_categorised <- combined_data_clean %>%
  mutate(
    sa_exposure = case_when(
      simple_average <= 1.9 ~ "Low",
      simple_average <= 7.8 ~ "Medium",
      TRUE ~ "High"
    ),
    tw_exposure = case_when(
      trade_weighted <= 1.35 ~ "Low",
      trade_weighted <= 7.08 ~ "Medium",
      TRUE ~ "High"
    )
  )

# === Step 1: Compute year-on-year change in trade value ===
combined_data_categorised <- combined_data_categorised %>%
  arrange(Code, Trade_Type, Year) %>%
  group_by(Code, Trade_Type) %>%
  mutate(
    delta_value = Value - lag(Value)
  ) %>%
  ungroup()

# === Step 2: Create treatment and time dummies ===
combined_data_categorised <- combined_data_categorised %>%
  mutate(
    post = if_else(as.integer(year) >= 2016, 1, 0),
    eu = if_else(str_detect(Trade_Type, "EU"), 1, 0),
    treat = if_else(tw_exposure == "High", 1, 0)
  )

# === Baseline regression: effect of high exposure on trade change ===
baseline_model <- lm(delta_value ~ treat, data = combined_data_categorised)
summary(baseline_model)

# === Triple differences regression ===
ddd_model <- lm(delta_value ~ post * eu * treat, data = combined_data_categorised)
summary(ddd_model)



