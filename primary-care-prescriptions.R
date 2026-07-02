# Install required packages if you do not have them installed:
# install.packages(c("rvest", "dplyr", "stringr", "readxl", "purrr", "httr"))

library(rvest)
library(dplyr)
library(stringr)
library(readxl)
library(purrr)
library(httr)

# 1. Configuration
target_bnf_codes <- c("0205052AE", "0404000U0", "0601023AN","0208020Y0","0601023AG")
base_url <- "https://www.nhsbsa.nhs.uk"
main_page_url <- "https://www.nhsbsa.nhs.uk/statistical-collections/prescription-cost-analysis-england"

message("Step 1: Scraping the main collection landing page...")
main_html <- read_html(main_page_url)

# Extract individual year landing pages from the main hub page
year_links <- main_html %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  unique() %>%
  # Keep only links pointing to annual prescription cost analysis collections
  keep(~ str_detect(.x, "prescription-cost-analysis-england-")) %>%
  # Fill in relative paths to absolute URLs
  map_chr(~ if (str_starts(.x, "http")) .x else paste0(base_url, .x)) %>%
  unique()

# Initialize an empty list to capture data from each scraped year
collated_list <- list()

message("Step 2: Processing available annual publication pages...")
for (page_url in year_links) {
  
  # Extract the year signature from the URL string for data tracking
  year_label <- str_extract(page_url, "(?<=prescription-cost-analysis-england-)[a-zA-Z0-9-]+$")
  if (is.na(year_label)) year_label <- basename(page_url)
  
  message("\n--------------------------------------------------")
  message("Analyzing Publication Page: ", year_label)
  
  page_html <- tryCatch(read_html(page_url), error = function(e) {
    message("Warning: Skipping unreachable page -> ", page_url)
    NULL
  })
  
  if (is.null(page_html)) next
  
  # Scrape all anchor tags on the specific year's page
  anchors <- page_html %>% html_nodes("a")
  link_texts <- html_text(anchors)
  link_hrefs <- html_attr(anchors, "href")
  
  # Look for "National summary tables". Prioritize financial year versions when available.
  target_idx <- which(str_detect(tolower(link_texts), "national summary tables.*financial year"))
  if (length(target_idx) == 0) {
    # Fallback to any general national summary table link or calendar year variation
    target_idx <- which(str_detect(tolower(link_texts), "national summary tables"))
  }
  
  if (length(target_idx) == 0) {
    message("Notice: No National Summary Table resource found on this page (likely a placeholder). Skipping.")
    next
  }
  
  # Get the download URL for the first matching workbook asset
  file_path <- link_hrefs[target_idx[1]]
  file_url <- if (str_starts(file_path, "http")) file_path else paste0(base_url, file_path)
  
  message("Downloading spreadsheet asset: ", file_url)
  tmp_file <- tempfile(fileext = ".xlsx")
  
  download_err <- tryCatch({
    download.file(file_url, tmp_file, mode = "wb", quiet = TRUE)
    FALSE
  }, error = function(e) TRUE)
  
  if (download_err) {
    message("Warning: File download failed for target URL.")
    next
  }
  
  # Confirm the existence of the Chemical_Substances tab
  all_sheets <- excel_sheets(tmp_file)
  target_sheet <- all_sheets[str_detect(tolower(all_sheets), "chemical_substances")]
  
  if (length(target_sheet) == 0) {
    message("Warning: 'Chemical_Substances' tab not found in this Excel file.")
    unlink(tmp_file)
    next
  }
  
  # NHS BSA tables contain a few metadata rows before the data headers.
  # We read a small chunk first to dynamically locate where the true headers begin.
  preview_sheet <- read_excel(tmp_file, sheet = target_sheet[1], col_names = FALSE, n_max = 20)
  
  header_row <- which(apply(preview_sheet, 1, function(row) {
    any(str_detect(tolower(as.character(row)), "substance code"), na.rm = TRUE)
  }))[1]
  
  # Re-read with the correct skip row index applied
  if (!is.na(header_row)) {
    year_df <- read_excel(tmp_file, sheet = target_sheet[1], skip = header_row - 1)
  } else {
    year_df <- read_excel(tmp_file, sheet = target_sheet[1])
  }
  
  # Dynamically capture the column mapping (e.g., 'BNF Chemical Substance Code')
  bnf_col_name <- colnames(year_df)[str_detect(tolower(colnames(year_df)), "substance code")][1]
  
  if (!is.na(bnf_col_name)) {
    # Filter rows defensively by stripping whitespace strings if present
    filtered_df <- year_df %>%
      rename(bnf_code_temp = !!bnf_col_name) %>% 
      mutate(bnf_code_clean = str_trim(as.character(bnf_code_temp))) %>%
      filter(bnf_code_clean %in% target_bnf_codes) %>%
      mutate(publication_year = year_label) %>% # Track source year
      select(-bnf_code_clean) %>%
      rename(!!bnf_col_name := bnf_code_temp) # Revert name back to original formatting
    
    collated_list[[year_label]] <- filtered_df
    message("Successfully extracted and filtered ", nrow(filtered_df), " matching rows.")
  } else {
    message("Warning: Could not identify a valid BNF Substance Code column.")
  }
  
  # Clean up local temporary file footprint
  unlink(tmp_file)
}

# 3. Collate and Export
message("\n--------------------------------------------------")
message("Step 3: Merging all extracted datasets...")

if (length(collated_list) > 0) {
  # bind_rows combines unequal layouts safely by aligning common names and filling gaps with NA
  final_collated_data <- bind_rows(collated_list)
  
  output_file <- "collated_nhs_pca_filtered.csv"
  write.csv(final_collated_data, output_file, row.names = FALSE)
  
  message("Collation complete!")
  message("Merged data saved to: ", getwd(), "/", output_file)
  print(head(final_collated_data))
} else {
  message("Error: No data could be compiled. Double check connection parameters.")
}