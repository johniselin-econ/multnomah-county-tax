# =============================================================================
# utils.R
# Shared utility functions for the multnomah-county-tax R code
#
# Functions:
#   read_api_key(api_codes_path, label) — Parse API key from CSV file
# =============================================================================

#' Read an API key from a CSV file
#'
#' Searches column 1 for a row matching `label` (case-insensitive),
#' then returns the value in column 2 (stripped of quotes and whitespace).
#'
#' @param api_codes_path Path to the CSV file containing API keys
#' @param label Label to search for in column 1 (e.g., "ipums", "census")
#' @return Character string with the API key
read_api_key <- function(api_codes_path, label) {
  if (!file.exists(api_codes_path)) {
    stop("API codes file not found at: ", api_codes_path, call. = FALSE)
  }

  api_codes <- tryCatch(
    read.delim(api_codes_path, sep = ",", header = TRUE, stringsAsFactors = FALSE),
    error = function(e) {
      read.delim(api_codes_path, sep = ",", header = FALSE, stringsAsFactors = FALSE)
    }
  )

  key <- NA_character_
  if (ncol(api_codes) >= 2) {
    col1 <- tolower(trimws(as.character(api_codes[[1]])))
    idx  <- which(col1 == tolower(label))
    if (length(idx) >= 1) {
      key <- as.character(api_codes[idx[1], 2])
    }
  }

  # Strip quotes and trim whitespace
  key <- trimws(gsub('"', '', key))

  if (is.na(key) || key == "") {
    stop("Could not parse a '", label, "' API key from: ", api_codes_path, call. = FALSE)
  }

  key
}
