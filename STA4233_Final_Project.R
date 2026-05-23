# 1. Load required packages

packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "lubridate",
  "fastDummies", "corrplot", "janitor", "stringr", "readr"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}



# 2. Read the Raw-Data and Calendar worksheets

raw_data <- read_excel("RawData.xlsx", sheet = "Raw-Data")
calendar <- read_excel("RawData.xlsx", sheet = "Calendar")



# 3. Check original data structure

original_rows <- nrow(raw_data)
original_cols <- ncol(raw_data)

str(raw_data)
summary(raw_data)

missing_original <- raw_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

write_csv(missing_original, "Table_1_Original_Missing_Values.csv")



# 4. Clean column names for easier R work

raw_data <- raw_data %>%
  clean_names()

calendar <- calendar %>%
  clean_names()



# 5. Convert date columns correctly

raw_data <- raw_data %>%
  mutate(
    po_download_date = as.Date(po_download_date),
    ship_date = as.Date(ship_date),
    receipt_date = as.Date(receipt_date)
  )

calendar <- calendar %>%
  mutate(
    start_date = as.Date(start_date),
    end_date = as.Date(end_date)
  )



# 6. User-created function to find calendar Quarter and Year

get_calendar_value <- function(date_value, calendar_data, return_column) {
  matched_row <- calendar_data %>%
    filter(date_value >= start_date & date_value <= end_date)
  
  if (nrow(matched_row) == 0) {
    return(NA)
  } else {
    return(matched_row[[return_column]][1])
  }
}



# 7. Add Quarter and Year using Receipt Date

raw_data <- raw_data %>%
  rowwise() %>%
  mutate(
    quarter = get_calendar_value(receipt_date, calendar, "quarter"),
    year = get_calendar_value(receipt_date, calendar, "year")
  ) %>%
  ungroup()



# 8. Calculate lead times

raw_data <- raw_data %>%
  mutate(
    in_transit_lead_time = as.numeric(receipt_date - ship_date),
    manufacturing_lead_time = as.numeric(ship_date - po_download_date)
  )



# 9. Check missing values after creating lead times

missing_after_leadtime <- raw_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

write_csv(missing_after_leadtime, "Table_2_Missing_After_Lead_Time.csv")



# 10. Check unusual categorical values before cleaning

lob_before <- raw_data %>% count(lob, sort = TRUE)
origin_before <- raw_data %>% count(origin, sort = TRUE)
ship_mode_before <- raw_data %>% count(ship_mode, sort = TRUE)
quarter_before <- raw_data %>% count(quarter, sort = TRUE)

write_csv(lob_before, "Table_3_LOB_Before_Cleaning.csv")
write_csv(origin_before, "Table_4_Origin_Before_Cleaning.csv")
write_csv(ship_mode_before, "Table_5_Ship_Mode_Before_Cleaning.csv")
write_csv(quarter_before, "Table_6_Quarter_Before_Cleaning.csv")



# 11. Clean categorical columns

raw_data <- raw_data %>%
  mutate(
    lob = str_squish(str_to_upper(lob)),
    origin = str_squish(str_to_upper(origin)),
    ship_mode = str_squish(str_to_upper(ship_mode)),
    quarter = str_squish(str_to_upper(quarter))
  ) %>%
  mutate(
    ship_mode = case_when(
      ship_mode == "FASTBOAT" ~ "FAST BOAT",
      TRUE ~ ship_mode
    )
  )



# 12. User-created function to calculate mode

get_mode <- function(x) {
  x <- x[!is.na(x)]
  unique_values <- unique(x)
  unique_values[which.max(tabulate(match(x, unique_values)))]
}



# 13. Clean Quarter and Year missing values

quarter_mode <- get_mode(raw_data$quarter)
year_mode <- get_mode(raw_data$year)

raw_data <- raw_data %>%
  mutate(
    quarter = ifelse(is.na(quarter), quarter_mode, quarter),
    year = ifelse(is.na(year), year_mode, year)
  )



# 14. Identify rows with mostly missing values

raw_data <- raw_data %>%
  mutate(missing_count = rowSums(is.na(.)))

rows_mostly_missing <- raw_data %>%
  filter(missing_count >= 4)

rows_removed <- nrow(rows_mostly_missing)

clean_data <- raw_data %>%
  filter(missing_count < 4) %>%
  select(-missing_count)



# 15. Mark impossible lead times as missing

clean_data <- clean_data %>%
  mutate(
    in_transit_lead_time = ifelse(in_transit_lead_time < 0, NA, in_transit_lead_time),
    manufacturing_lead_time = ifelse(manufacturing_lead_time < 0, NA, manufacturing_lead_time)
  )



# 16. User-created function to clean extreme outliers using IQR rule

clean_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  lower_limit <- q1 - 1.5 * iqr_value
  upper_limit <- q3 + 1.5 * iqr_value
  
  x <- ifelse(x < lower_limit | x > upper_limit, NA, x)
  return(x)
}



# 17. Clean extreme lead time values

clean_data <- clean_data %>%
  mutate(
    in_transit_lead_time = clean_outliers_iqr(in_transit_lead_time),
    manufacturing_lead_time = clean_outliers_iqr(manufacturing_lead_time)
  )



# 18. Impute missing lead times using median by Origin and Ship Mode

clean_data <- clean_data %>%
  group_by(origin, ship_mode) %>%
  mutate(
    in_transit_lead_time = ifelse(
      is.na(in_transit_lead_time),
      median(in_transit_lead_time, na.rm = TRUE),
      in_transit_lead_time
    ),
    manufacturing_lead_time = ifelse(
      is.na(manufacturing_lead_time),
      median(manufacturing_lead_time, na.rm = TRUE),
      manufacturing_lead_time
    )
  ) %>%
  ungroup()



# 19. If any missing lead time still remains, impute with overall median

clean_data <- clean_data %>%
  mutate(
    in_transit_lead_time = ifelse(
      is.na(in_transit_lead_time),
      median(in_transit_lead_time, na.rm = TRUE),
      in_transit_lead_time
    ),
    manufacturing_lead_time = ifelse(
      is.na(manufacturing_lead_time),
      median(manufacturing_lead_time, na.rm = TRUE),
      manufacturing_lead_time
    )
  )



# 20. Check final missing values

final_missing <- clean_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

write_csv(final_missing, "Table_7_Final_Missing_Values.csv")



# 21. Final cleaned category counts

lob_after <- clean_data %>% count(lob, sort = TRUE)
origin_after <- clean_data %>% count(origin, sort = TRUE)
ship_mode_after <- clean_data %>% count(ship_mode, sort = TRUE)
quarter_after <- clean_data %>% count(quarter, sort = TRUE)

write_csv(lob_after, "Table_8_LOB_After_Cleaning.csv")
write_csv(origin_after, "Table_9_Origin_After_Cleaning.csv")
write_csv(ship_mode_after, "Table_10_Ship_Mode_After_Cleaning.csv")
write_csv(quarter_after, "Table_11_Quarter_After_Cleaning.csv")



# 22. Row count table for report

final_rows <- nrow(clean_data)

row_count_table <- data.frame(
  Description = c(
    "Original rows received",
    "Rows removed during cleaning",
    "Rows used for future modeling"
  ),
  Count = c(original_rows, rows_removed, final_rows)
)

write_csv(row_count_table, "Table_12_Row_Counts.csv")



# 23. Descriptive statistics for numeric variables

numeric_summary <- clean_data %>%
  summarise(
    in_transit_min = min(in_transit_lead_time),
    in_transit_q1 = quantile(in_transit_lead_time, 0.25),
    in_transit_median = median(in_transit_lead_time),
    in_transit_mean = mean(in_transit_lead_time),
    in_transit_q3 = quantile(in_transit_lead_time, 0.75),
    in_transit_max = max(in_transit_lead_time),
    in_transit_sd = sd(in_transit_lead_time),
    
    manufacturing_min = min(manufacturing_lead_time),
    manufacturing_q1 = quantile(manufacturing_lead_time, 0.25),
    manufacturing_median = median(manufacturing_lead_time),
    manufacturing_mean = mean(manufacturing_lead_time),
    manufacturing_q3 = quantile(manufacturing_lead_time, 0.75),
    manufacturing_max = max(manufacturing_lead_time),
    manufacturing_sd = sd(manufacturing_lead_time)
  )

write_csv(numeric_summary, "Table_13_Numeric_Descriptive_Statistics.csv")



# 24. Descriptive statistics for categorical variables

categorical_summary <- bind_rows(
  clean_data %>% count(lob) %>% mutate(variable = "LOB", category = lob) %>% select(variable, category, n),
  clean_data %>% count(origin) %>% mutate(variable = "Origin", category = origin) %>% select(variable, category, n),
  clean_data %>% count(ship_mode) %>% mutate(variable = "Ship Mode", category = ship_mode) %>% select(variable, category, n),
  clean_data %>% count(quarter) %>% mutate(variable = "Quarter", category = quarter) %>% select(variable, category, n),
  clean_data %>% count(year) %>% mutate(variable = "Year", category = as.character(year)) %>% select(variable, category, n)
)

write_csv(categorical_summary, "Table_14_Categorical_Descriptive_Statistics.csv")



# 25. Plot 1: In-transit lead time distribution

plot1 <- ggplot(clean_data, aes(x = in_transit_lead_time)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Figure 1. In-transit Lead Time Distribution",
    x = "In-transit Lead Time",
    y = "Number of Shipments"
  )

ggsave("Figure_1_In_Transit_Lead_Time_Distribution.png", plot1, width = 8, height = 5)



# 26. Plot 2: Manufacturing lead time distribution

plot2 <- ggplot(clean_data, aes(x = manufacturing_lead_time)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Figure 2. Manufacturing Lead Time Distribution",
    x = "Manufacturing Lead Time",
    y = "Number of Shipments"
  )

ggsave("Figure_2_Manufacturing_Lead_Time_Distribution.png", plot2, width = 8, height = 5)



# 27. Plot 3: In-transit lead time by ship mode

plot3 <- ggplot(clean_data, aes(x = ship_mode, y = in_transit_lead_time)) +
  geom_boxplot() +
  labs(
    title = "Figure 3. In-transit Lead Time by Ship Mode",
    x = "Ship Mode",
    y = "In-transit Lead Time"
  )

ggsave("Figure_3_In_Transit_By_Ship_Mode.png", plot3, width = 8, height = 5)



# 28. Plot 4: In-transit lead time by origin

plot4 <- ggplot(clean_data, aes(x = origin, y = in_transit_lead_time)) +
  geom_boxplot() +
  labs(
    title = "Figure 4. In-transit Lead Time by Origin",
    x = "Origin",
    y = "In-transit Lead Time"
  )

ggsave("Figure_4_In_Transit_By_Origin.png", plot4, width = 8, height = 5)



# 29. Plot 5: Average in-transit lead time by ship mode

avg_ship_mode <- clean_data %>%
  group_by(ship_mode) %>%
  summarise(
    avg_in_transit_lead_time = mean(in_transit_lead_time),
    .groups = "drop"
  )

write_csv(avg_ship_mode, "Table_15_Average_In_Transit_By_Ship_Mode.csv")

plot5 <- ggplot(avg_ship_mode, aes(x = ship_mode, y = avg_in_transit_lead_time)) +
  geom_col() +
  labs(
    title = "Figure 5. Average In-transit Lead Time by Ship Mode",
    x = "Ship Mode",
    y = "Average In-transit Lead Time"
  )

ggsave("Figure_5_Average_In_Transit_By_Ship_Mode.png", plot5, width = 8, height = 5)



# 30. Plot 6: Average in-transit lead time by origin

avg_origin <- clean_data %>%
  group_by(origin) %>%
  summarise(
    avg_in_transit_lead_time = mean(in_transit_lead_time),
    .groups = "drop"
  )

write_csv(avg_origin, "Table_16_Average_In_Transit_By_Origin.csv")

plot6 <- ggplot(avg_origin, aes(x = origin, y = avg_in_transit_lead_time)) +
  geom_col() +
  labs(
    title = "Figure 6. Average In-transit Lead Time by Origin",
    x = "Origin",
    y = "Average In-transit Lead Time"
  )

ggsave("Figure_6_Average_In_Transit_By_Origin.png", plot6, width = 8, height = 5)



# 31. Plot 7: In-transit lead time vs manufacturing lead time

plot7 <- ggplot(clean_data, aes(x = manufacturing_lead_time, y = in_transit_lead_time)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Figure 7. In-transit Lead Time vs Manufacturing Lead Time",
    x = "Manufacturing Lead Time",
    y = "In-transit Lead Time"
  )

ggsave("Figure_7_In_Transit_vs_Manufacturing.png", plot7, width = 8, height = 5)



# 32. Prepare data for correlation analysis using one-hot encoding

correlation_data <- clean_data %>%
  select(
    in_transit_lead_time,
    manufacturing_lead_time,
    lob,
    origin,
    ship_mode,
    quarter,
    year
  ) %>%
  dummy_cols(
    select_columns = c("lob", "origin", "ship_mode", "quarter"),
    remove_first_dummy = FALSE,
    remove_selected_columns = TRUE
  )



# 33. Calculate correlation matrix

correlation_matrix <- cor(correlation_data, use = "complete.obs")

write_csv(
  as.data.frame(correlation_matrix),
  "Table_17_Full_Correlation_Matrix.csv"
)



# 34. Correlation of each predictor with In-transit Lead Time

correlation_with_in_transit <- as.data.frame(correlation_matrix) %>%
  select(in_transit_lead_time) %>%
  tibble::rownames_to_column("predictor") %>%
  filter(predictor != "in_transit_lead_time") %>%
  mutate(abs_correlation = abs(in_transit_lead_time)) %>%
  arrange(desc(abs_correlation))

write_csv(
  correlation_with_in_transit,
  "Table_18_Correlation_With_In_Transit_Lead_Time.csv"
)



# 35. Correlation plot

png("Figure_8_Correlation_Plot.png", width = 900, height = 700)
corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  tl.col = "black"
)
dev.off()



# 36. Save cleaned final dataset

write_csv(clean_data, "Cleaned_RawData_Final.csv")



# 37. Print important final results

print("Original rows:")
print(original_rows)

print("Rows removed:")
print(rows_removed)

print("Final rows used for analysis:")
print(final_rows)

print("Final missing values:")
print(final_missing)

print("Correlation with In-transit Lead Time:")
print(correlation_with_in_transit)