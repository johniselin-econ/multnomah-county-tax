# =============================================================================
# audit_paper_numbers.R
#
# Check the in-text numbers register against the manuscript.
#
#   results/paper_numbers.csv   one row per number the paper quotes in prose
#                               (written by project_log_number in Stata)
#   <Overleaf>/Conway_Iselin_Rork_2026.tex   the manuscript
#
# The paper and the slides quote these numbers through the macro file the
# pipeline writes next to the register (results/paper_numbers.tex, mirrored to
# the Overleaf tables/ folder): \pn{<key>} prints the formatted value. For
# every \pn{} use in either .tex, and every registered key, reports:
#   - USED           registered and used (doc / line numbers listed)
#   - UNKNOWN_KEY    used in a .tex but not registered: prints ?? in the PDF
#   - UNUSED         registered but quoted nowhere through \pn{}. If the
#                    formatted string still appears as a literal number, the
#                    `literal_lines` column lists where (a candidate to convert;
#                    a chance match is possible); otherwise the key is either
#                    diagnostic-only or should be dropped
#   - MISSING_VALUE  the pipeline logged a missing value
# Informational: exits 0 either way.
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
tex_paths <- audit_tex_paths(oth_path)
tex_paths <- tex_paths[nzchar(tex_paths)]

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

if (length(tex_paths) == 0) {
  cat("No .tex reachable (set OVERLEAF_TEX / OVERLEAF_SLIDES_TEX): printing the register only.\n")
  print(register |> select(key, formatted, unit, source_script), n = Inf)
  quit(status = 0)
}

# Trailing % comments dropped first (a value parked in a `% TODO` tail must
# not count), then \% -> %, \$ -> $, \, (thin space) removed, ~ -> space.
read_tex <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8") |>
    str_replace_all("(?<!\\\\)%.*$", "") |>
    str_replace_all("\\\\%", "%") |>
    str_replace_all("\\\\\\$", "$") |>
    str_replace_all("\\\\,", "") |>
    str_replace_all("~", " ")
}
tex <- imap(tex_paths, function(p, doc) read_tex(p))

# "<doc>:<line>" for every line matching `pattern`
where <- function(pattern) {
  imap(tex, function(lines, doc) {
    hits <- which(str_detect(lines, pattern))
    if (length(hits)) paste0(doc, ":", hits) else character()
  }) |> unlist(use.names = FALSE) |> paste(collapse = ";")
}

# ---- Macro uses -------------------------------------------------------------
uses <- imap_dfr(tex, function(lines, doc) {
  m <- str_match_all(lines, "\\\\pn\\{([^}]+)\\}")
  imap_dfr(m, function(x, ln) if (nrow(x)) tibble(key = x[, 2], doc = doc, line = ln))
})
if (nrow(uses) == 0) uses <- tibble(key = character(), doc = character(), line = integer())
use_lines <- uses |>
  group_by(key) |>
  summarise(n_uses = n(), use_lines = paste0(doc, ":", line, collapse = ";"), .groups = "drop")

# ---- Literal (hand-typed) occurrences of an unused key's value ---------------
# The formatted string, and the same with thousands separators when the
# magnitude calls for it, as a number token not glued to another digit or a
# decimal point that continues into a digit (so 5.1 misses 5.1.3 and 15.1).
spellings <- function(formatted, value) {
  out <- formatted
  if (!is.na(value) && abs(value) >= 1000) {
    digits <- if (str_detect(formatted, "\\.")) str_count(str_replace(formatted, "^.*\\.", ""), "[0-9]") else 0
    out <- c(out, formatC(value, format = "f", big.mark = ",", digits = digits))
  }
  unique(out)
}
token_regex <- function(s) {
  paste0("(?<![0-9.])", str_replace_all(s, "([.+*?^$()\\[\\]{}|\\\\-])", "\\\\\\1"), "(?!\\.?[0-9])")
}
literal_lines <- function(formatted, value) {
  if (is.na(value) || formatted %in% c("", ".")) return("")
  where(paste(map_chr(spellings(formatted, value), token_regex), collapse = "|"))
}

audit <- full_join(register, use_lines, by = "key") |>
  mutate(
    n_uses = coalesce(n_uses, 0L),
    status = case_when(
      is.na(source_script)                        ~ "UNKNOWN_KEY",
      is.na(value) | formatted %in% c("", ".")    ~ "MISSING_VALUE",
      n_uses > 0                                  ~ "USED",
      TRUE                                        ~ "UNUSED"
    ),
    literal_lines = if_else(status == "UNUSED",
                            map2_chr(formatted, value, literal_lines), "")
  ) |>
  select(key, value, formatted, unit, source_script, status, n_uses, use_lines,
         literal_lines, note) |>
  arrange(factor(status, c("UNKNOWN_KEY", "MISSING_VALUE", "UNUSED", "USED")), key)

# ---- Report -----------------------------------------------------------------
dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
today      <- format(Sys.Date(), "%Y-%m-%d")
report_out <- file.path(report_dir, sprintf("paper_numbers_audit_%s.csv", today))
write_csv(audit, report_out)

cat("\nStatus counts:\n")
print(audit |> count(status), n = Inf)

cat("Read:", paste(names(tex_paths), tex_paths, sep = " = ", collapse = "; "), "\n")
flagged <- audit |> filter(status != "USED")
if (nrow(flagged) > 0) {
  cat("\nNot used through \\pn{} (UNKNOWN_KEY prints ?? in the PDF):\n")
  print(flagged |> select(key, formatted, status, use_lines, literal_lines), n = Inf)
}

cat("\nReport:", report_out, "\n")
