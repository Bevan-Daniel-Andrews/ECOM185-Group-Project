rm(list = ls())

# === Load libraries ===
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(tidyr)

## Import and export data for EU and non-EU trade 

# === Step 1: Define and unzip ===
zip_path <- "Trade_Data_zip.zip"  # Update if in subfolder
extract_dir <- tempfile()
unzip(zipfile = zip_path, exdir = extract_dir)

# === Step 2: List all Excel files ===
excel_files <- list.files(extract_dir, pattern = "\\.xls[x]?$", full.names = TRUE, recursive = TRUE)

# === Step 3a: Identify trade type from filename (for pre-April 2015 files) ===
get_trade_type <- function(filename) {
  name <- tolower(basename(filename))
  if (str_detect(name, "otschap_eutots_arr")) return("EU import")
  if (str_detect(name, "otschap_eutots_dis")) return("EU export")
  if (str_detect(name, "ots_imp")) return("Non-EU import")
  if (str_detect(name, "ots_exp")) return("Non-EU export")
  return("Unknown")
}

# === Step 3b: Identify if file is in Apr 2015 – Dec 2016 window ===
is_apr15_to_dec16 <- function(filename) {
  name <- tolower(basename(filename))
  match <- str_match(name, "(\\d{2})(\\d{2})")
  if (is.na(match[1,1])) return(FALSE)
  
  part1 <- as.integer(match[1,2])
  part2 <- as.integer(match[1,3])
  possible_dates <- list(
    list(month = part1, year = 2000 + part2),
    list(month = part2, year = 2000 + part1)
  )
  
  for (d in possible_dates) {
    if ((d$year == 2015 && d$month >= 4) || (d$year == 2016)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# === Step 4a: Read Apr 2015 – Dec 2016 combined-sheet files ===
extract_date_info <- function(filename) {
  name <- tolower(basename(filename))
  match <- str_match(name, "(\\d{2})(\\d{2})")
  if (is.na(match[1,1])) return(NULL)
  
  part1 <- as.integer(match[1,2])
  part2 <- as.integer(match[1,3])
  possible_dates <- list(
    list(month = part1, year = 2000 + part2),
    list(month = part2, year = 2000 + part1)
  )
  
  for (d in possible_dates) {
    if ((d$year == 2015 && d$month >= 4) || (d$year == 2016)) {
      return(list(
        Year = d$year,
        Month = d$month,
        Date = sprintf("%02d-%d", d$month, d$year)
      ))
    }
  }
  return(NULL)
}

read_combined_trade_file <- function(file_path) {
  base <- tolower(basename(file_path))
  sheets <- excel_sheets(file_path)
  if (length(sheets) < 2) return(NULL)
  
  date_info <- extract_date_info(base)
  if (is.null(date_info)) return(NULL)
  
  is_export <- str_detect(base, "exp")
  trade_types <- if (is_export) {
    c("EU export", "Non-EU export")
  } else {
    c("EU import", "Non-EU import")
  }
  
  dfs <- map2(sheets[1:2], trade_types, function(sheet, trade_type) {
    df <- tryCatch({
      read_excel(file_path, sheet = sheet, skip = 2)
    }, error = function(e) {
      message(paste("Failed to read sheet:", sheet, "in", basename(file_path)))
      return(NULL)
    })
    
    if (is.null(df) || ncol(df) < 3) return(NULL)
    df <- df[1:min(nrow(df), 99), c(2,3)]
    names(df) <- c("Code", "Value")
    
    df %>%
      mutate(
        Trade_Type = trade_type,
        File = base,
        Date = date_info$Date,
        Month = date_info$Month,
        Year = date_info$Year
      )
  })
  
  bind_rows(dfs)
}

# === Step 4b: Read Pre-April 2015 files ===
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
  
  cleaned_df %>%
    mutate(
      Date = formatted_date,
      Month = month,
      Year = year,
      Trade_Type = get_trade_type(file_path),
      File = tolower(basename(file_path))
    )
}

# === Step 5: Separate files and process ===
files_apr15_dec16 <- excel_files %>%
  keep(is_apr15_to_dec16) %>%
  discard(~ str_detect(tolower(.x), "eutots_arr|eutots_dis"))

files_pre_apr15 <- setdiff(excel_files, files_apr15_dec16)

combined_pre_apr15 <- map_dfr(files_pre_apr15, read_and_clean_excel)
combined_apr15_dec16 <- map_dfr(files_apr15_dec16, read_combined_trade_file)

# === Step 6: Merge datasets ===
combined_all <- bind_rows(combined_pre_apr15, combined_apr15_dec16)

# === Step 7: Preview
glimpse(combined_all)

# === Step 7.5: Remove rows with NA in 'Code' or 'Value' ===
combined_all <- combined_all %>%
  filter(!is.na(Code), !is.na(Value))

## MFN tariff data

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

# === Step 10: Deduplicate tariff data ===
tariff_clean_dedup <- tariff_clean %>%
  distinct(year, HS, simple_average, trade_weighted, .keep_all = TRUE)

# === Step 11: Extract year from combined_all$Date and convert to character ===
combined_all <- combined_all %>%
  mutate(
    year = str_sub(Date, start = 4),  # Extract YYYY
    year = as.character(year)
  )

# === Step 12: Join tariff data ===
combined_all <- combined_all %>%
  left_join(tariff_clean_dedup, by = c("Code" = "HS", "year" = "year"))

# === Step 13: Reorder new columns for readability ===
combined_all <- combined_all %>%
  relocate(simple_average, trade_weighted, .after = Value)

# === Step 14: Check for missing tariff values ===
missing_tariffs <- combined_all %>%
  summarise(
    total_rows = n(),
    missing_simple = sum(is.na(simple_average)),
    missing_trade_weighted = sum(is.na(trade_weighted)),
    missing_either = sum(is.na(simple_average) | is.na(trade_weighted)),
    complete_rows = sum(!is.na(simple_average) & !is.na(trade_weighted))
  )

print(missing_tariffs)

# === Step 15: Remove rows with missing tariff data ===
combined_all_clean <- combined_all %>%
  filter(!is.na(simple_average), !is.na(trade_weighted))


