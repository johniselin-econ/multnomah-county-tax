# =============================================================================
# audit_paper_numbers.R
#
# Check the in-text numbers register against the manuscript.
#
#   results/paper_numbers.csv   one row per number the paper quotes in prose
#                               (written by project_log_number in Stata)
#   <Overleaf>/Conway_Iselin_Rork_2026.tex   the manuscript
#
# For every registered key, searches the .tex for the `formatted` string (tex
# escapes such as \% and \$ stripped first; a thousands-separated variant is
# also tried) and reports:
#   - FOUND          the string occurs at least once (line numbers listed)
#   - NOT_FOUND      it does not: the prose is stale, or rounded differently
#   - MISSING_VALUE  the pipeline logged a missing value
# Informational: exits 0 either way. A chance match elsewhere in the text is
# possible, so read the matched lines, not just the status.
#
# Writes results/_runs/paper_numbers_audit_<date>.csv and prints a summary.
# Run standalone:  Rscript code/R/audit_paper_numbers.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
})

# ---- Paths (same resolution as audit_paper_artifacts.R) ---------------------
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = TRUE)),
  error = function(e) NULL
)
repo_root <- if (!is.null(script_dir)) normalizePath(file.path(script_dir, "..", "..")) else getwd()

register_path <- file.path(repo_root, "results", "paper_numbers.csv")
report_dir    <- file.path(repo_root, "results", "_runs")

source(file.path(repo_root, "code", "R", "utils.R"))
oth_path    <- resolve_oth_path(load_user_settings(repo_root), repo_root)
tex_default <- if (nzchar(oth_path)) paste0(oth_path, "Conway_Iselin_Rork_2026.tex") else ""
tex_path    <- Sys.getenv("OVERLEAF_TEX", unset = tex_default)
tex_available <- nzchar(tex_path) && file.exists(tex_path)

if (!file.exists(register_path)) {
  cat("paper_numbers audit: no register at", register_path,
      "\n  (nothing has called project_log_number yet, or results/ was cleaned).\n")
  quit(status = 0)
}

# Stata writes a missing value as an empty field and formats it as "."
register <- read_csv(register_path, show_col_types = FALSE, na = c("", "NA", "."),
                     col_types = cols(.default = col_character(), value = col_double()))
stopifnot(all(c("key", "value", "formatted", "source_script") %in% names(register)))

cat("paper_numbers audit:", nrow(register), "registered numbers from",
    n_distinct(register$source_script), "scripts\n")

if (!tex_available) {
  cat("Tex not reachable (", if (nzchar(tex_path)) tex_path else "no path configured",
      "): printing the register only.\n", sep = "")
  print(register |> select(key, formatted, unit, source_script), n = Inf)
  quit(status = 0)
}

# ---- Normalise the tex so prose spellings match the register ----------------
# Trailing % comments dropped first (a stale value parked in a `% TODO` tail
# must not count as present), then \% -> %, \$ -> $, \, (thin space)
# removed, ~ -> space; --- and -- kept.
tex_lines <- readLines(tex_path, warn = FALSE, encoding = "UTF-8") |>
  str_replace_all("(?<!\\\\)%.*$", "") |>
  str_replace_all("\\\\%", "%") |>
  str_replace_all("\\\\\\$", "$") |>
  str_replace_all("\\\\,", "") |>
  str_replace_all("~", " ")
is_comment <- str_detect(tex_lines, "^\\s*%")

# Spellings to try for one registered number: the formatted string as logged,
# and the same with thousands separators when the magnitude calls for it.
spellings <- function(formatted, value) {
  out <- formatted
  if (!is.na(value) && abs(value) >= 1000) {
    digits <- str_count(str_replace(formatted, "^.*\\.", ""), "[0-9]")
    if (!str_detect(formatted, "\\.")) digits <- 0
    out <- c(out, formatC(value, format = "f", big.mark = ",", digits = digits))
  }
  unique(out)
}

# A number token: not glued to another digit, or a decimal point that continues
# into a digit, on either side (so 5.1 does not match inside 5.1.3 or 15.1).
token_regex <- function(s) {
  regex(paste0("(?<![0-9.])", str_replace_all(s, "([.+*?^$()\\[\\]{}|\\\\-])", "\\\\\\1"), "(?!\\.?[0-9])"))
}

audit_one <- function(key, value, formatted) {
  if (is.na(value) || formatted %in% c("", ".")) {
    return(tibble(status = "MISSING_VALUE", n_matches = 0L, lines = ""))
  }
  hits <- map(spellings(formatted, value), function(s) {
    which(str_detect(tex_lines, token_regex(s)) & !is_comment)
  }) |> unlist() |> unique() |> sort()
  tibble(
    status    = if (length(hits) > 0) "FOUND" else "NOT_FOUND",
    n_matches = length(hits),
    lines     = paste(hits, collapse = ";")
  )
}

audit <- register |>
  mutate(res = pmap(list(key, value, formatted), audit_one)) |>
  tidyr::unnest(res) |>
  select(key, value, formatted, unit, source_script, status, n_matches, lines, note)

# ---- Report -----------------------------------------------------------------
dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
today      <- format(Sys.Date(), "%Y-%m-%d")
report_out <- file.path(report_dir, sprintf("paper_numbers_audit_%s.csv", today))
write_csv(audit, report_out)

cat("\nStatus counts:\n")
print(audit |> count(status), n = Inf)

not_found <- audit |> filter(status != "FOUND")
if (nrow(not_found) > 0) {
  cat("\nNot found in the tex (stale prose, different rounding, or missing value):\n")
  print(not_found |> select(key, formatted, unit, source_script, status), n = Inf)
}

cat("\nReport:", report_out, "\n")
