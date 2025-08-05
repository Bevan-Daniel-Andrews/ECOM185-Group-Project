library(dplyr)
library(ggplot2)
library(scales)
library(broom)
library(tidyr)

## Visualisations & summary statistics ##

# === Step 1: Summary statistics for tariff variables ===
tariff_summary_2015 <- eu_exports_2015 %>%
  summarise(
    simple_avg_mean = mean(simple_average, na.rm = TRUE),
    simple_avg_median = median(simple_average, na.rm = TRUE),
    simple_avg_max = max(simple_average, na.rm = TRUE),
    trade_weighted_mean = mean(trade_weighted, na.rm = TRUE),
    trade_weighted_median = median(trade_weighted, na.rm = TRUE),
    trade_weighted_max = max(trade_weighted, na.rm = TRUE)
  )


# === Step 2: Quantile-based summary statistics ===
quantiles <- combined_all_clean %>%
  summarise(
    simple_avg_Q1 = quantile(simple_average, 0.25, na.rm = TRUE),
    simple_avg_Q3 = quantile(simple_average, 0.75, na.rm = TRUE),
    trade_weighted_Q1 = quantile(trade_weighted, 0.25, na.rm = TRUE),
    trade_weighted_Q3 = quantile(trade_weighted, 0.75, na.rm = TRUE)
  )

print(quantiles)

# === Step 3: Visualise tariff distributions ===

# Histogram: Simple average
ggplot(combined_all_clean, aes(x = simple_average)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Simple Average Tariffs", x = "Simple Average Tariff (%)", y = "Frequency")

# Histogram: Trade-weighted
ggplot(combined_all_clean, aes(x = trade_weighted)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of Trade-Weighted Tariffs", x = "Trade-Weighted Tariff (%)", y = "Frequency")

# Boxplot: Simple average
ggplot(combined_all_clean, aes(y = simple_average)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot: Simple Average Tariffs", y = "Tariff (%)")

# Boxplot: Trade-weighted
ggplot(combined_all_clean, aes(y = trade_weighted)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot: Trade-Weighted Tariffs", y = "Tariff (%)")

# === Step 4: Count unique products per trade type, year, and month ===
product_counts <- combined_all_clean %>%
  group_by(Year, Month, Trade_Type) %>%
  summarise(
    Unique_Products = n_distinct(Code),
    .groups = "drop"
  )

# View result
print(product_counts)
print(product_counts, n = Inf)


# Step 5: Ensure simple_avg_cat is available
combined_all_clean <- combined_all_clean %>%
  mutate(
    simple_avg_cat = case_when(
      simple_average == 0 ~ "Zero",
      simple_average <= quantile(simple_average[simple_average > 0], 0.10, na.rm = TRUE) ~ "Low",
      simple_average <= quantile(simple_average[simple_average > 0], 0.25, na.rm = TRUE) ~ "Low-Medium",
      simple_average <= quantile(simple_average[simple_average > 0], 0.50, na.rm = TRUE) ~ "Medium",
      simple_average <= quantile(simple_average[simple_average > 0], 0.75, na.rm = TRUE) ~ "High",
      TRUE ~ "Extreme"
    ),
    simple_avg_cat = factor(
      simple_avg_cat,
      levels = c("Zero", "Low", "Low-Medium", "Medium", "High", "Extreme"),
      ordered = TRUE
    )
  )

# Step 5.1: Filter to EU exports in 2015
eu_exports_2015 <- combined_all_clean %>%
  filter(Trade_Type == "EU export", Year == 2015)

# Step 5.2: Aggregate export values by tariff exposure
export_plot_data <- eu_exports_2015 %>%
  group_by(simple_avg_cat) %>%
  summarise(
    total_export_billion = sum(Value, na.rm = TRUE) / 1e6,  # £000 → £bn
    .groups = "drop"
  )

# Step 5.3: Define manual colours
tariff_colors <- c(
  "Zero" = "#1b9e77",
  "Low" = "#d95f02",
  "Low-Medium" = "#7570b3",
  "Medium" = "#e7298a",
  "High" = "#66a61e",
  "Extreme" = "#e6ab02"
)

# Step 5.4: Plot with capped y-axis and fixed breaks
ggplot(export_plot_data, aes(x = simple_avg_cat, y = total_export_billion, fill = simple_avg_cat)) +
  geom_col(show.legend = TRUE) +
  labs(
    title = "Export Value by Tariff Exposure (EU Exports, 2015)",
    x = "Tariff Exposure Category",
    y = "Export Value (£bn)",
    fill = "Tariff Exposure"
  ) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10),
    labels = label_comma()
  ) +
  scale_fill_manual(values = tariff_colors) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "right",
    axis.text.x = element_blank()  # Hides redundant x-axis labels
  )

export_plot_data <- export_plot_data %>%
  mutate(
    share_percent = total_export_billion / sum(total_export_billion) * 100
  )
print(export_plot_data)


ggplot(eu_exports_2015, aes(x = simple_avg_cat, y = Value)) +
  geom_boxplot(fill = "lightblue") +
  scale_y_log10(labels = scales::label_comma()) +
  labs(title = "Distribution of Export Values by Tariff Category (EU Exports 2015)",
       x = "Tariff Category", y = "Export Value (£, log scale)")


## Baseline Regressions ##
# === Load required libraries ===


# === Step 6: Filter for EU exports only ===
eu_exports <- combined_all_clean %>%
  filter(Trade_Type == "EU export")

# === Step 6.1: Aggregate total export value by product and year ===
exports_yearly <- eu_exports %>%
  group_by(Code, Year) %>%
  summarise(total_export = sum(Value, na.rm = TRUE), .groups = "drop")

# === Step 6.2: Create wide-format data for 2015–2016 export comparison ===
exports_wide <- exports_yearly %>%
  filter(Year %in% c(2015, 2016)) %>%
  pivot_wider(names_from = Year, values_from = total_export, names_prefix = "year_") %>%
  filter(!is.na(year_2015), !is.na(year_2016)) %>%
  mutate(export_growth = log(year_2016 + 1) - log(year_2015 + 1))  # Log difference

# === Step 6.3: Join tariff data ===
tariff_lookup <- combined_all_clean %>%
  select(Code, simple_average, trade_weighted) %>%
  distinct()

exports_model_data <- exports_wide %>%
  left_join(tariff_lookup, by = "Code") %>%
  mutate(log_export_2015 = log(year_2015 + 1))

# === Step 6.4: Create tariff exposure categories (with 'Zero' as baseline) ===
exports_model_data <- exports_model_data %>%
  mutate(
    simple_avg_cat = case_when(
      simple_average == 0 ~ "Zero",
      simple_average <= quantile(simple_average[simple_average > 0], 0.10, na.rm = TRUE) ~ "Low",
      simple_average <= quantile(simple_average[simple_average > 0], 0.25, na.rm = TRUE) ~ "Low-Medium",
      simple_average <= quantile(simple_average[simple_average > 0], 0.50, na.rm = TRUE) ~ "Medium",
      simple_average <= quantile(simple_average[simple_average > 0], 0.75, na.rm = TRUE) ~ "High",
      TRUE ~ "Extreme"
    ),
    trade_weighted_cat = case_when(
      trade_weighted == 0 ~ "Zero",
      trade_weighted <= quantile(trade_weighted[trade_weighted > 0], 0.10, na.rm = TRUE) ~ "Low",
      trade_weighted <= quantile(trade_weighted[trade_weighted > 0], 0.25, na.rm = TRUE) ~ "Low-Medium",
      trade_weighted <= quantile(trade_weighted[trade_weighted > 0], 0.50, na.rm = TRUE) ~ "Medium",
      trade_weighted <= quantile(trade_weighted[trade_weighted > 0], 0.75, na.rm = TRUE) ~ "High",
      TRUE ~ "Extreme"
    ),
    simple_avg_cat = factor(simple_avg_cat, levels = c("Zero", "Low", "Low-Medium", "Medium", "High", "Extreme")),
    trade_weighted_cat = factor(trade_weighted_cat, levels = c("Zero", "Low", "Low-Medium", "Medium", "High", "Extreme"))
  )

# === Step 6.5: Run categorical regression models ===

# Simple average
model_simple_cat <- lm(export_growth ~ simple_avg_cat + log_export_2015, data = exports_model_data)
tidy_simple_cat <- tidy(model_simple_cat)
glance_simple_cat <- glance(model_simple_cat)

# Trade-weighted average
model_weighted_cat <- lm(export_growth ~ trade_weighted_cat + log_export_2015, data = exports_model_data)
tidy_weighted_cat <- tidy(model_weighted_cat)
glance_weighted_cat <- glance(model_weighted_cat)

# === Step 6.6: Output results ===
print(tidy_simple_cat)
print(glance_simple_cat)

print(tidy_weighted_cat)
print(glance_weighted_cat)

## Fixed effects model ## 

# Step 7 === Convert to long format ===
exports_long <- exports_yearly %>%
  left_join(tariff_lookup, by = "Code") %>%
  filter(Year %in% c(2015, 2016)) %>%
  mutate(
    log_export = log(total_export + 1),
    simple_avg_cat = case_when(
      simple_average == 0 ~ "Zero",
      simple_average <= quantile(simple_average[simple_average > 0], 0.10, na.rm = TRUE) ~ "Low",
      simple_average <= quantile(simple_average[simple_average > 0], 0.25, na.rm = TRUE) ~ "Low-Medium",
      simple_average <= quantile(simple_average[simple_average > 0], 0.50, na.rm = TRUE) ~ "Medium",
      simple_average <= quantile(simple_average[simple_average > 0], 0.75, na.rm = TRUE) ~ "High",
      TRUE ~ "Extreme"
    ),
    simple_avg_cat = factor(simple_avg_cat, levels = c("Zero", "Low", "Low-Medium", "Medium", "High", "Extreme"))
  )

# Step 7.1 === Model with time fixed effects ===
model_fe <- lm(log_export ~ simple_avg_cat + factor(Year), data = exports_long)
summary(model_fe)

## Triple difference regressions (without time FE)

# Step 8: Filter export data only
exports_all <- combined_all_clean %>%
  filter(Trade_Type %in% c("EU export", "Non-EU export"))

# Step 8.1: Aggregate export value by product, year, and trade type
exports_grouped <- exports_all %>%
  group_by(Code, Year, Trade_Type) %>%
  summarise(export_value = sum(Value, na.rm = TRUE), .groups = "drop")

# Step 8.2: Pivot to wide format to calculate growth
exports_wide_ddd <- exports_grouped %>%
  pivot_wider(names_from = Year, values_from = export_value, names_prefix = "year_") %>%
  filter(!is.na(year_2015), !is.na(year_2016)) %>%
  mutate(export_growth = log(year_2016 + 1) - log(year_2015 + 1))

# Step 8.3: Recreate tariff exposure categories (from scratch to avoid inherited ordering)
tariff_bins <- combined_all_clean %>%
  mutate(
    simple_avg_cat = case_when(
      simple_average == 0 ~ "Zero",
      simple_average <= quantile(simple_average[simple_average > 0], 0.10, na.rm = TRUE) ~ "Low",
      simple_average <= quantile(simple_average[simple_average > 0], 0.25, na.rm = TRUE) ~ "Low-Medium",
      simple_average <= quantile(simple_average[simple_average > 0], 0.50, na.rm = TRUE) ~ "Medium",
      simple_average <= quantile(simple_average[simple_average > 0], 0.75, na.rm = TRUE) ~ "High",
      TRUE ~ "Extreme"
    ),
    trade_weighted_cat = case_when(
      trade_weighted == 0 ~ "Zero",
      trade_weighted <= quantile(trade_weighted[trade_weighted > 0], 0.10, na.rm = TRUE) ~ "Low",
      trade_weighted <= quantile(trade_weighted[trade_weighted > 0], 0.25, na.rm = TRUE) ~ "Low-Medium",
      trade_weighted <= quantile(trade_weighted[trade_weighted > 0], 0.50, na.rm = TRUE) ~ "Medium",
      trade_weighted <= quantile(trade_weighted[trade_weighted > 0], 0.75, na.rm = TRUE) ~ "High",
      TRUE ~ "Extreme"
    )
  ) %>%
  mutate(
    simple_avg_cat = factor(simple_avg_cat, levels = c("Zero", "Low", "Low-Medium", "Medium", "High", "Extreme"), ordered = FALSE),
    trade_weighted_cat = factor(trade_weighted_cat, levels = c("Zero", "Low", "Low-Medium", "Medium", "High", "Extreme"), ordered = FALSE)
  ) %>%
  select(Code, Trade_Type, simple_avg_cat, trade_weighted_cat) %>%
  distinct()

# Step 8.4: Join tariff exposure categories
exports_model_ddd_cat <- exports_wide_ddd %>%
  left_join(tariff_bins, by = c("Code", "Trade_Type")) %>%
  filter(!is.na(simple_avg_cat), !is.na(trade_weighted_cat))

# Step 8.5: Add EU dummy
exports_model_ddd_cat <- exports_model_ddd_cat %>%
  mutate(
    eu = if_else(Trade_Type == "EU export", 1, 0)
  )

# Step 8.6: Triple difference model using simple average category (dummy-coded)
model_ddd_simple_cat <- lm(export_growth ~ simple_avg_cat * eu, data = exports_model_ddd_cat)
summary(model_ddd_simple_cat)

# Step 8.7: Triple difference model using trade-weighted category (dummy-coded)
model_ddd_weighted_cat <- lm(export_growth ~ trade_weighted_cat * eu, data = exports_model_ddd_cat)
summary(model_ddd_weighted_cat)


library(dplyr)
library(broom)


## Triple difference model (with time FE) ##

library(dplyr)
library(tidyr)
library(broom)

# === Step 9: Filter exports only ===
exports_all <- combined_all_clean %>%
  filter(Trade_Type %in% c("EU export", "Non-EU export"))

# === Step 9.1: Aggregate export values ===
exports_grouped <- exports_all %>%
  group_by(Code, Year, Trade_Type) %>%
  summarise(export_value = sum(Value, na.rm = TRUE), .groups = "drop")

# === Step 9.2: Join tariff data ===
tariff_lookup <- combined_all_clean %>%
  select(Code, simple_average) %>%
  distinct()

exports_grouped <- exports_grouped %>%
  left_join(tariff_lookup, by = "Code")

# === Step 9.3: Categorise tariff exposure ===
exports_grouped <- exports_grouped %>%
  mutate(
    simple_avg_cat = case_when(
      simple_average == 0 ~ "Zero",
      simple_average <= quantile(simple_average[simple_average > 0], 0.10, na.rm = TRUE) ~ "Low",
      simple_average <= quantile(simple_average[simple_average > 0], 0.25, na.rm = TRUE) ~ "Low-Medium",
      simple_average <= quantile(simple_average[simple_average > 0], 0.50, na.rm = TRUE) ~ "Medium",
      simple_average <= quantile(simple_average[simple_average > 0], 0.75, na.rm = TRUE) ~ "High",
      TRUE ~ "Extreme"
    ),
    simple_avg_cat = factor(simple_avg_cat, levels = c("Zero", "Low", "Low-Medium", "Medium", "High", "Extreme"))
  )

# === Step 9.4: Create log exports ===
exports_grouped <- exports_grouped %>%
  mutate(log_export = log(export_value + 1))

# === Step 9.5: Triple Difference Regression ===
# EU dummy
exports_grouped <- exports_grouped %>%
  mutate(eu = if_else(Trade_Type == "EU export", 1, 0))

# Run model with time fixed effects (Year) and categorical tariff exposure interacted with EU
model_ddd_fe <- lm(
  log_export ~ simple_avg_cat * eu + factor(Year),
  data = exports_grouped
)

# === Step 9.6: Output Results ===
tidy_ddd_fe <- tidy(model_ddd_fe)
glance_ddd_fe <- glance(model_ddd_fe)

print(tidy_ddd_fe)
print(glance_ddd_fe)














