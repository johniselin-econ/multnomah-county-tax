# =============================================================================
# Author:  John Iselin
# Purpose: Build resources/wfh_teleworkable_occ.csv -- the Dingel & Neiman
#          (2020) teleworkable share for every ACS public-use occupation code
#          (IPUMS OCC), for both code vintages:
#            occ_vintage 2010: 2010 Census codes (ACS through 2017)
#            occ_vintage 2018: 2018 Census codes (ACS 2018+)
#          Inputs and construction: resources/wfh_crosswalk/README.md.
#          Consumed by the WFH heterogeneity panel in 02_did_analysis.do.
# Usage:   Rscript code/R/build_wfh_crosswalk.R   (from the repo root)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(stringr)
  library(purrr)
})

dir_in   <- "resources/wfh_crosswalk"
file_out <- "resources/wfh_teleworkable_occ.csv"

MILITARY_SHARE <- 0       # 55-xxxx: not rated by O*NET; treated as not teleworkable
NO_OCC_CODE    <- 9920    # unemployed, no work in last 5 years -> "no occupation" in the DiD

# Typos in the Census PUMS lists: no SOC major group 40 exists (49-9095 is
# manufactured building and mobile home installers, inside OCC 7630 / 7640);
# the 2013 list writes military code 55-9830 without its dash.
SOC_TOKEN_FIXES <- c("40-9095" = "49-9095", "559830" = "55-9830")

# ---- 1. SOC 2010 shares from Dingel & Neiman ---------------------------------
# O*NET-SOC 2010 code = SOC 2010 detailed code + ".xx" extension.
dn <- read.csv(file.path(dir_in, "dn_occupations_workathome.csv"),
               colClasses = c(onetsoccode = "character"))
stopifnot(!anyNA(dn$teleworkable), all(dn$teleworkable %in% c(0, 1)))

soc2010 <- dn |>
  mutate(soc = substr(onetsoccode, 1, 7)) |>
  group_by(soc) |>
  summarise(share = mean(teleworkable), .groups = "drop")

# ---- 2. SOC 2018 shares via the BLS crosswalk --------------------------------
bls <- read_excel(file.path(dir_in, "bls_soc_2010_to_2018.xlsx"), skip = 8,
                  col_types = "text", .name_repair = "minimal")
bls <- tibble(soc2010 = str_trim(bls[[1]]), soc2018 = str_trim(bls[[3]])) |>
  filter(str_detect(soc2010, "^\\d{2}-\\d{4}$"), str_detect(soc2018, "^\\d{2}-\\d{4}$"))

soc2018 <- bls |>
  inner_join(soc2010, by = c("soc2010" = "soc")) |>
  group_by(soc = soc2018) |>
  summarise(share = mean(share), .groups = "drop")

# ---- 3. Parse the Census PUMS occupation lists --------------------------------
# A row with a 4-digit code in column 1 opens an occupation. Its SOC token is
# column 2, unless the next row reads "Combines:", in which case the following
# rows' column-2 SOC tokens (column 1 blank) are its components.
parse_pums <- function(path, sheet) {
  x <- read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text",
                  .name_repair = "minimal")
  c1 <- str_trim(x[[1]]); c2 <- str_trim(x[[2]])
  out <- list(); i <- 1; n <- length(c1)
  while (i <= n) {
    if (!is.na(c1[i]) && str_detect(c1[i], "^\\d{4}$")) {
      occ <- as.integer(c1[i]); comps <- c2[i]
      if (i < n && identical(c2[i + 1], "Combines:")) {
        comps <- character(0); j <- i + 2
        while (j <= n && is.na(c1[j]) && !is.na(c2[j]) && str_detect(c2[j], "^\\d{2}-[0-9X]{4}$")) {
          comps <- c(comps, c2[j]); j <- j + 1
        }
        if (length(comps) == 0) stop("No components after 'Combines:' for OCC ", occ)
        i <- j - 1
      }
      fix <- comps %in% names(SOC_TOKEN_FIXES)
      comps[fix] <- SOC_TOKEN_FIXES[comps[fix]]
      out[[length(out) + 1]] <- tibble(occ = occ, soc_token = comps)
    }
    i <- i + 1
  }
  bind_rows(out)
}

# SOC token -> regex over detailed codes: X wildcards; codes ending in 0 are
# broad (xx-xxx0), minor (xx-xx00 / xx-x000) or major groups, expanded as prefixes.
token_regex <- function(tok) {
  if (str_detect(tok, "X")) return(paste0("^", str_replace_all(tok, "X", "\\\\d"), "$"))
  digits <- str_remove(tok, "-")
  if (!str_ends(digits, "0")) return(paste0("^", tok, "$"))
  stem <- str_remove(digits, "0+$")
  paste0("^", substr(stem, 1, 2), "-", substr(stem, 3, nchar(stem)))
}

# Residual fill (D&N's rule, unweighted): an unrated SOC code takes the mean of
# rated codes sharing its first four digits.
fill_share <- function(tok_codes, soc_tbl) {
  hit <- soc_tbl$share[soc_tbl$soc %in% tok_codes]
  if (length(hit)) return(mean(hit))
  pref <- unique(substr(tok_codes, 1, 5))
  fam <- soc_tbl$share[substr(soc_tbl$soc, 1, 5) %in% pref]
  if (length(fam)) mean(fam) else NA_real_
}

share_for_token <- function(tok, soc_tbl) {
  if (str_starts(tok, "55-")) return(MILITARY_SHARE)
  rx <- token_regex(tok)
  matched <- soc_tbl$soc[str_detect(soc_tbl$soc, rx)]
  if (length(matched)) return(mean(soc_tbl$share[soc_tbl$soc %in% matched]))
  fill_share(tok, soc_tbl)
}

build_vintage <- function(path, sheet, soc_tbl, vintage) {
  parse_pums(path, sheet) |>
    filter(occ != NO_OCC_CODE) |>
    mutate(tok_share = map_dbl(soc_token, share_for_token, soc_tbl = soc_tbl)) |>
    group_by(occ) |>
    summarise(n_soc_tokens = n(),
              n_unrated    = sum(is.na(tok_share)),
              teleworkable_share = mean(tok_share), .groups = "drop") |>
    mutate(occ_vintage = vintage, .before = 1)
}

xw <- bind_rows(
  build_vintage(file.path(dir_in, "census_pums_occ_2013.xlsx"), 1,     soc2010, 2010L),
  build_vintage(file.path(dir_in, "census_pums_occ_2018.xlsx"), "ACS", soc2018, 2018L)
)

# ---- 4. Checks: every code rated, shares in [0,1], one row per code ----------
unrated <- filter(xw, n_unrated > 0)
if (nrow(unrated)) {
  print(unrated, n = Inf)
  stop(nrow(unrated), " occupation codes have an SOC component with no rating")
}
stopifnot(!anyDuplicated(xw[c("occ_vintage", "occ")]),
          all(xw$teleworkable_share >= 0 & xw$teleworkable_share <= 1))

xw <- select(xw, occ_vintage, occ, teleworkable_share, n_soc_tokens) |>
  mutate(teleworkable_share = round(teleworkable_share, 6))
write.csv(xw, file_out, row.names = FALSE)

cat("Wrote", file_out, ":", nrow(xw), "codes\n")
print(xw |> group_by(occ_vintage) |>
        summarise(codes = n(), share_mean = mean(teleworkable_share),
                  at_half = sum(teleworkable_share == 0.5),
                  teleworkable_05 = sum(teleworkable_share >= 0.5)))
