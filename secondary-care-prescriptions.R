# install.packages(c("dplyr", "stringr", "httr", "jsonlite", "readr", "purrr"))

library(dplyr)
library(stringr)
library(httr)
library(jsonlite)
library(readr)
library(purrr)

# 1. Configuration & API Endpoint
scmd_api_url <- "https://opendata.nhsbsa.net/api/3/action/package_show?id=finalised-secondary-care-medicines-data-scmd-with-indicative-price"
user_agent_string <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"

api_response <- GET(scmd_api_url)

if (status_code(api_response) != 200) {
  stop("Could not pull data manifest from the NHS Open Data Portal.")
}

api_data <- fromJSON(content(api_response, "text", encoding = "UTF-8"))
resources <- api_data$result$resources

# Isolate direct CSV file target URLs
scmd_csv_urls <- resources %>%
  filter(format == "text/csv" | str_detect(tolower(url), "\\.csv")) %>%
  pull(url) %>%
  unique()

message("Identified ", length(scmd_csv_urls), " monthly SCMD files to scan.")

# Initialize storage for our output steps
collated_scmd_list <- list()

for (file_url in scmd_csv_urls) {
  
  period_label <- str_extract(file_url, "SCMD_FINAL_[0-9]+")
  if (is.na(period_label)) period_label <- basename(file_url)
  
  message("-> Processing Month: ", period_label)
  
  # Set up a safe temporary file destination for the download
  tmp_csv <- tempfile(fileext = ".csv")
  
  download_status <- tryCatch({
    GET(
      url = file_url,
      config = add_headers(`User-Agent` = user_agent_string),
      write_disk(tmp_csv, overwrite = TRUE)
    )
  }, error = function(e) {
    NULL
  })
  
  if (is.null(download_status) || status_code(download_status) != 200) {
    message("   Warning: Skipping file download error on endpoint: ", file_url)
    unlink(tmp_csv)
    next
  }
  
  # Stream input to prevent R workspace RAM crashes
  monthly_raw <- tryCatch({
    read_csv(
      tmp_csv,
      col_types = cols_only(
        YEAR_MONTH = col_character(),
        VMP_PRODUCT_NAME = col_character(),
        INDICATIVE_COST = col_double()
      ),
      progress = FALSE
    )
  }, error = function(e) {
    NULL
  })
  
  if (is.null(monthly_raw)) {
    unlink(tmp_csv)
    next
  }
  
  # Filter rows based on clinical drug string matches and assign clean master groups
  monthly_matched <- monthly_raw %>%
    mutate(
      prod_lower = tolower(VMP_PRODUCT_NAME),
      DRUG_GROUP = case_when(
        str_detect(prod_lower, "pembrolizumab") ~ "Pembrolizumab",
        str_detect(prod_lower, "daratumumab")   ~ "Daratumumab",
        str_detect(prod_lower, "nivolumab")     ~ "Nivolumab",
        str_detect(prod_lower, "sacubitril")    ~ "Sacubitril formulations",
        str_detect(prod_lower, "lisdexamfetamine|dimesylate") ~ "Lisdexamfetamine Dimesylate",
        str_detect(prod_lower, "empagliflozin")  ~ "Empagliflozin",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(DRUG_GROUP))
  
  # If matching records were pulled, aggregate costs to product level
  if (nrow(monthly_matched) > 0) {
    monthly_aggregated <- monthly_matched %>%
      group_by(YEAR_MONTH, DRUG_GROUP) %>%
      summarise(
        MONTH_INDICATIVE_COST = sum(INDICATIVE_COST, na.rm = TRUE),
        RECORDS_COUNT = n(),
        .groups = "drop"
      )
    
    collated_scmd_list[[period_label]] <- monthly_aggregated
    message("Logged ", sum(monthly_aggregated$RECORDS_COUNT), " row entries.")
  } else {
    message("Notice: No matching targeted strings found in this dataset.")
  }
  
  # Wipe temporary storage footprint cleanly before next loop iteration
  unlink(tmp_csv)
}

# 3. Combine and Export Results
if (length(collated_scmd_list) > 0) {
  final_scmd_report <- bind_rows(collated_scmd_list) %>%
    group_by(YEAR_MONTH, DRUG_GROUP) %>%
    summarise(
      TOTAL_INDICATIVE_COST = sum(MONTH_INDIC_COST = MONTH_INDICATIVE_COST, na.rm = TRUE),
      TOTAL_RECORDS_MERGED = sum(RECORDS_COUNT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(DRUG_GROUP, YEAR_MONTH)
  
  output_filename <- "collated_scmd_text_search_costs.csv"
  write.csv(final_scmd_report, output_filename, row.names = FALSE)
  
  message("File exported to path: ", getwd(), "/", output_filename)
  print(head(final_scmd_report, 15))
} else {
  message("Error: Text matching sequence yielded zero output rows. Verify source spelling.")
}
