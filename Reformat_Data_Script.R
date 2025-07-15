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
    HS = 16
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


