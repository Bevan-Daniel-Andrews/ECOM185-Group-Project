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
zip_path <- "Trade_Data_zip.zip"  # update if in subfolder
extract_dir <- tempfile()
unzip(zipfile = zip_path, exdir = extract_dir)

# === Step 2: List all Excel files ===
excel_files <- list.files(extract_dir, pattern = "\\.xlsx?$", full.names = TRUE, recursive = TRUE)

# === Step 3: Function to identify trade type from filename ===
get_trade_type <- function(filename) {
  name <- tolower(basename(filename))
  if (str_detect(name, "otschap_eutots_arr")) return("EU import")
  if (str_detect(name, "otschap_eutots_diss")) return("EU export")
  if (str_detect(name, "ots_imp")) return("Non-EU import")
  if (str_detect(name, "ots_exp")) return("Non-EU export")
  return("Unknown")
}

# === Step 4: Function to read and clean a single Excel file ===
read_and_clean_excel <- function(file_path) {
  # Try reading the sheet (skip top 2 rows)
  df <- tryCatch({
    read_excel(file_path, skip = 2)
  }, error = function(e) {
    message(paste("Failed to read:", file_path))
    return(NULL)
  })
  
  # Skip if read failed or has fewer than 3 columns
  if (is.null(df) || ncol(df) < 3) return(NULL)
  
  # Extract the header of the 3rd column to determine date
  col3_title <- names(df)[3]
  
  # Try to parse this title as a month-year
  date_parsed <- suppressWarnings(parse_date_time(col3_title, orders = c("B Y", "b Y"), locale = "en"))
  
  if (is.na(date_parsed)) {
    message(paste("Could not parse date from column name in:", file_path))
    return(NULL)
  }
  
  formatted_date <- format(date_parsed, "%m-%Y")  # final format MM-YYYY
  
  # Keep only column 2 and 3
  cleaned_df <- df[, c(2, 3)]
  names(cleaned_df) <- c("Code", "Value")
  
  # Add additional columns
  cleaned_df <- cleaned_df %>%
    mutate(
      Date = formatted_date,
      Trade_Type = get_trade_type(file_path)
    )
  
  return(cleaned_df)
}

# === Step 5: Apply to all files and combine ===
combined_data <- map_dfr(excel_files, read_and_clean_excel)

# === Step 6: Preview combined data ===
glimpse(combined_data)
