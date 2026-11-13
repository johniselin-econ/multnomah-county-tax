# =============================================================================
# donor_onepagers.R
# One page for Multnomah and one per Policy-match donor county: how its
# migration flows look before and after 2020, and whether the donor is a
# credible placebo for Multnomah in the individual-level DiD
# (02_did_placebo.do).
#
# Each county page has
#   1. basic statistics and (donors) the placebo screens, beside Multnomah's;
#   2. migration rates 2016 onward against Multnomah (other donors in grey);
#   3. maps: where its movers went / came from before 2020, and the change in
#      AGI flows after 2020 computed as in paper Figure 6;
#   4. top partner counties, flow-concentration measures, and the placebo
#      College x Year event study.
# Page 1 indexes the donors and their screens; page 2 compares the
# flow-concentration measures across Multnomah and all donors.
#
# The Figure 6 flow-rate change is defined in 02_descriptives.do for Multnomah
# only; partner_flow_change() below restates it for any focal county and is
# checked against that script's output for Multnomah.
#
# Run from the repo root, after 02_descriptives.do, 02_sdid_analysis.do and
# 02_did_placebo.do:
#   Rscript code/R/donor_onepagers.R
#
# Inputs:  resources/donor_pools.csv, resources/state_policy_2019_2020.csv
#          data/demographic/county_adjacency.csv (download_public_data.R)
#          data/working/sdid_analysis_data.dta, irs_county_flow.dta,
#            irs_county_gross.dta, multnomah_partner_flows_agi.csv
#          results/did/did_placebo_results.csv, results/did/did_placebo_es.csv
# Outputs: results/donor_onepagers/donor_onepagers.pdf (all pages)
#          results/donor_onepagers/county_<fips>_<county>.pdf (one per county)
#          results/donor_onepagers/flow_concentration.csv (returns and AGI)
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(haven)
  library(readr)
  library(tigris)
  library(here)
  library(patchwork)
  library(gridExtra)
  library(grid)
})

options(tigris_use_cache = TRUE)
source(here("code", "R", "colors_plotplainblind.R"))

# ---- Constants ---------------------------------------------------------------
MULT_FIPS       <- 41051
# IRS filing-year windows, as in 02_descriptives.do (2020 left out)
PRE_YEARS       <- 2018:2019
POST_YEARS      <- 2021:2022
TREND_START     <- 2016
POLICY_YEAR     <- 2020        # enactment; vertical line drawn after it
N_TOP_PARTNERS  <- 5           # partner counties listed per direction
N_TOP_FLOWS     <- 10          # flows in the top-N concentration measure
MAP_HALF_SIDE_M <- 150000      # half-side of the square map window (EPSG:5070)
# 2010 boundaries: the IRS files keep Connecticut's old counties throughout
MAP_COUNTY_YEAR <- 2010
MAP_SIMPLIFY_M  <- 300         # boundary simplification tolerance; keeps the PDF small
RATE_PER        <- 100000      # Figure 6 flow rates are per 100,000 of partner base
CHANGE_CAP_QUANTILE <- 0.95    # change-map colour scale capped at this quantile of |change|
PAGE_W          <- 8.5
PAGE_H          <- 11
BASE_PT         <- 7

col_donor  <- PPB_SEA          # as in 02_did_placebo.do
col_multn  <- PPB_VERMILLION
col_others <- "grey78"

panel_path      <- here("data", "working", "sdid_analysis_data.dta")
flow_path       <- here("data", "working", "irs_county_flow.dta")
gross_path      <- here("data", "working", "irs_county_gross.dta")
fig6_path       <- here("data", "working", "multnomah_partner_flows_agi.csv")
adjacency_path  <- here("data", "demographic", "county_adjacency.csv")
pools_path      <- here("resources", "donor_pools.csv")
policy_path     <- here("resources", "state_policy_2019_2020.csv")
placebo_path    <- here("results", "did", "did_placebo_results.csv")
placebo_es_path <- here("results", "did", "did_placebo_es.csv")
out_dir         <- here("results", "donor_onepagers")

for (p in c(panel_path, flow_path, gross_path, fig6_path, adjacency_path, pools_path,
            policy_path, placebo_path, placebo_es_path)) {
  if (!file.exists(p)) stop("donor_onepagers: input not found: ", p, call. = FALSE)
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load --------------------------------------------------------------------
donor_fips <- read_csv(pools_path, show_col_types = FALSE) |>
  filter(sample_policy == 1, fips != MULT_FIPS) |>
  pull(fips)
focal_fips <- c(MULT_FIPS, donor_fips)

placebo <- read_csv(placebo_path, show_col_types = FALSE)
stopifnot(setequal(placebo$fips, focal_fips))
# Screen (b) cutoff = largest distance among donors that pass it
irs_cutoff <- max(placebo$irs_dist[placebo$pass_irs %in% 1])
placebo <- placebo |>
  mutate(
    irs_rank = if_else(fips == MULT_FIPS, NA_integer_,
                       rank(if_else(fips == MULT_FIPS, NA_real_, irs_dist),
                            na.last = "keep", ties.method = "min")),
    county_short = sub(" County$", "", county_name)
  )

placebo_es <- read_csv(placebo_es_path, show_col_types = FALSE)

panel <- read_dta(panel_path) |>
  zap_labels() |> zap_formats() |>
  filter(fips %in% focal_fips, year >= TREND_START) |>
  select(fips, year, state_fips, state_name, county_name,
         pop_census, pop_density, percent_urban, median_income,
         share_under_24, share_over_65, msahodays,
         sample_stringency, sample_narrow, sample_urban75_covid, sample_demog,
         n1_out_3, n1_in_3, n1_out_5, n1_in_5, agi_out_3, agi_in_3, agi_out_5, agi_in_5,
         agi_out_rate_irs, agi_in_rate_irs, agi_net_rate_irs, n2_out_rate_acs2)

policy <- read_csv(policy_path, show_col_types = FALSE) |>
  select(state_fips, state_abbr, top_rate_2019, top_rate_2020,
         gov_party_2019, gov_party_2020)

window_of <- function(year) if_else(year %in% PRE_YEARS, "pre", "post")

flows <- read_dta(flow_path, col_select = c(year, fips_o, fips_d, n1, agi)) |>
  zap_labels() |>
  filter(year %in% c(PRE_YEARS, POST_YEARS), fips_o != fips_d,
         fips_o %in% focal_fips | fips_d %in% focal_fips) |>
  mutate(period = window_of(year))

# Every county's AGI base (non-movers + all movers) by window, for the
# Figure 6 rates; counties not observed in all four years are dropped, as in
# 02_descriptives.do.
agi_base <- read_dta(gross_path, col_select = c(year, fips, agi_out_1, agi_out_2)) |>
  zap_labels() |>
  filter(year %in% c(PRE_YEARS, POST_YEARS)) |>
  filter(n() == length(c(PRE_YEARS, POST_YEARS)), .by = fips) |>
  mutate(period = window_of(year)) |>
  summarise(base = sum(agi_out_1 + agi_out_2), .by = c(fips, period)) |>
  pivot_wider(names_from = period, values_from = base, names_prefix = "base_")

adjacency <- read_csv(adjacency_path, show_col_types = FALSE)
# States sharing a border with each state, from the county pairs
state_adjacency <- adjacency |>
  transmute(state = fips %/% 1000, neighbor_state = neighbor_fips %/% 1000) |>
  filter(state != neighbor_state) |>
  distinct()

state_abbr <- tigris::fips_codes |>
  distinct(state_code, state) |>
  transmute(STATE = state_code, st = state)

counties_sf <- counties(cb = TRUE, year = MAP_COUNTY_YEAR, progress_bar = FALSE) |>
  st_transform(5070) |>
  mutate(fips = as.numeric(paste0(STATE, COUNTY))) |>
  left_join(state_abbr, by = "STATE") |>
  select(fips, NAME, st) |>
  st_simplify(preserveTopology = TRUE, dTolerance = MAP_SIMPLIFY_M)
stopifnot(!anyDuplicated(counties_sf$fips), !anyNA(counties_sf$st), !any(st_is_empty(counties_sf)))
states_sf <- states(cb = TRUE, year = 2023, progress_bar = FALSE) |>
  st_transform(5070) |>
  st_simplify(preserveTopology = TRUE, dTolerance = MAP_SIMPLIFY_M)
county_labels <- st_drop_geometry(counties_sf) |>
  mutate(partner = paste0(NAME, ", ", st)) |>
  select(fips, partner)

# ---- Helpers -----------------------------------------------------------------
fmt_num <- function(x, digits = 0) formatC(x, format = "f", digits = digits, big.mark = ",")
fmt_pct <- function(x, digits = 1) paste0(formatC(x, format = "f", digits = digits), "%")
fmt_b   <- function(b, se, digits = 2) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), b, se)
}
years_lab <- function(y) paste0(min(y), "–", substr(max(y), 3, 4))
PRE_LAB  <- years_lab(PRE_YEARS)
POST_LAB <- years_lab(POST_YEARS)

window_mean <- function(d, var, years) {
  v <- d[[var]][d$year %in% years]
  stopifnot(length(v) == length(years), !anyNA(v))
  mean(v)
}

focal_short  <- function(f) placebo$county_short[placebo$fips == f]
focal_colour <- function(f) if (f == MULT_FIPS) col_multn else col_donor

# A table grob stretched to its panel, first column left-aligned; rows whose
# first cell is in bold_labels are set in bold
table_grob <- function(df, col_widths, base_size = BASE_PT, bold_labels = character()) {
  th <- ttheme_minimal(
    base_size = base_size,
    core    = list(fg_params = list(hjust = 0, x = 0.02),
                   padding = unit(c(2, 2.2), "mm")),
    colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold"),
                   padding = unit(c(2, 2.2), "mm"))
  )
  g <- tableGrob(df, rows = NULL, theme = th)
  g$widths <- unit(col_widths / sum(col_widths), "npc")
  g <- gtable::gtable_add_grob(
    g, segmentsGrob(x0 = 0, x1 = 1, y0 = 0, y1 = 0, gp = gpar(lwd = 0.8)),
    t = 1, l = 1, r = ncol(g)
  )
  bold_rows <- which(df[[1]] %in% bold_labels) + 1          # +1 for the header row
  for (i in which(g$layout$name == "core-fg" & g$layout$t %in% bold_rows)) {
    g$grobs[[i]] <- editGrob(g$grobs[[i]], gp = gpar(fontface = "bold"))
  }
  g$vp <- viewport(y = 1, just = "top", height = sum(g$heights))
  g
}

theme_page <- function() {
  theme_minimal(base_size = BASE_PT) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.2, colour = "grey90"),
      plot.title = element_text(face = "bold", size = BASE_PT + 1),
      plot.subtitle = element_text(size = BASE_PT - 0.5, colour = "grey30"),
      strip.text = element_text(face = "bold", size = BASE_PT, hjust = 0),
      legend.position = "bottom",
      legend.key.height = unit(2.5, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      plot.margin = margin(3, 5, 3, 5)
    )
}

# ---- Partner flows and concentration ------------------------------------------
# Flows between focal county f and each partner county, summed over each
# window's years, with the focal county's total domestic movers alongside.
partner_flows <- function(f) {
  movers <- panel |>
    filter(fips == f, year %in% c(PRE_YEARS, POST_YEARS)) |>
    mutate(period = window_of(year)) |>
    summarise(across(matches("^(n1|agi)_(out|in)_[35]$"), sum), .by = period) |>
    pivot_longer(-period, names_to = c("measure", "direction", "type"),
                 names_pattern = "^(n1|agi)_(out|in)_([35])$") |>
    pivot_wider(names_from = type, values_from = value, names_prefix = "movers_")
  stopifnot(!anyNA(movers))
  bind_rows(
    flows |> filter(fips_o == f) |> transmute(period, direction = "out", partner_fips = fips_d, n1, agi),
    flows |> filter(fips_d == f) |> transmute(period, direction = "in",  partner_fips = fips_o, n1, agi)
  ) |>
    summarise(across(c(n1, agi), sum), .by = c(period, direction, partner_fips)) |>
    pivot_longer(c(n1, agi), names_to = "measure", values_to = "flow") |>
    left_join(movers, by = c("period", "direction", "measure")) |>
    mutate(share = 100 * flow / movers_3)       # movers_3 = all domestic movers
}

# Concentration of a county's domestic migration, % of its domestic movers:
#   border_county  to / from a county adjacent to it
#   within_state   to / from elsewhere in its own state (exact, IRS totals)
#   border_state   to / from a state bordering its own. Lower bound: the IRS
#                  suppresses small county pairs, so only disclosed flows count
#   top_flows      its N_TOP_FLOWS largest county-pair flows
#   disclosed      all county pairs the IRS reports individually
flow_concentration <- function(f, pf) {
  own_state <- f %/% 1000
  border_counties <- adjacency$neighbor_fips[adjacency$fips == f]
  border_states <- state_adjacency$neighbor_state[state_adjacency$state == own_state]
  pf |>
    summarise(
      border_county = sum(share[partner_fips %in% border_counties]),
      within_state  = 100 * (1 - first(movers_5) / first(movers_3)),   # movers_5 = out of state
      border_state  = sum(share[(partner_fips %/% 1000) %in% border_states]),
      top_flows     = sum(head(sort(share, decreasing = TRUE), N_TOP_FLOWS)),
      disclosed     = sum(share),
      .by = c(measure, direction, period)
    ) |>
    mutate(fips = f, .before = 1)
}

# Paper Figure 6 for any focal county: AGI flow to (from) each partner per
# RATE_PER of the partner's AGI base, post window minus pre window. A partner
# missing from a window counts as a zero flow.
partner_flow_change <- function(pf) {
  pf |>
    filter(measure == "agi") |>
    select(direction, partner_fips, period, flow) |>
    pivot_wider(names_from = period, values_from = flow, values_fill = 0) |>
    inner_join(agi_base, by = c("partner_fips" = "fips")) |>
    mutate(rate_change = RATE_PER * (post / base_post - pre / base_pre))
}

flows_by_county <- lapply(setNames(focal_fips, focal_fips), partner_flows)
concentration <- bind_rows(lapply(focal_fips, function(f) {
  flow_concentration(f, flows_by_county[[as.character(f)]])
}))

# Parity with 02_descriptives.do: Multnomah's Figure 6 rate changes
local({
  ours <- partner_flow_change(flows_by_county[[as.character(MULT_FIPS)]]) |>
    select(direction, fips = partner_fips, rate_change) |>
    pivot_wider(names_from = direction, values_from = rate_change, names_prefix = "ours_")
  theirs <- read_csv(fig6_path, show_col_types = FALSE) |>
    select(fips, out_pre, out_post, in_pre, in_post, out_rate_change, in_rate_change) |>
    left_join(ours, by = "fips")
  chk_out <- theirs |> filter(out_pre != 0 | out_post != 0, !is.na(out_rate_change))
  chk_in  <- theirs |> filter(in_pre != 0 | in_post != 0, !is.na(in_rate_change))
  stopifnot(nrow(chk_out) > 0, nrow(chk_in) > 0,
            isTRUE(all.equal(chk_out$ours_out, chk_out$out_rate_change, tolerance = 1e-6)),
            isTRUE(all.equal(chk_in$ours_in, chk_in$in_rate_change, tolerance = 1e-6)))
})

write_csv(
  concentration |>
    left_join(placebo |> select(fips, state_name, county_name), by = "fips") |>
    relocate(state_name, county_name, .after = fips) |>
    arrange(fips != MULT_FIPS, fips, measure, desc(direction), desc(period)),
  file.path(out_dir, "flow_concentration.csv")
)

conc_rows <- c(border_county = "Border county", within_state = "Within state",
               border_state = "Bordering state", top_flows = sprintf("Top %d flows", N_TOP_FLOWS))

# ---- Panel 1: statistics and screens ------------------------------------------
county_stats <- function(f) {
  d  <- panel |> filter(fips == f)
  d1 <- d |> filter(year == max(PRE_YEARS))
  pol <- policy |> filter(state_fips == d1$state_fips)
  stopifnot(nrow(d1) == 1, nrow(pol) == 1)
  rate_pair <- function(var) {
    sprintf("%.2f → %.2f", window_mean(d, var, PRE_YEARS), window_mean(d, var, POST_YEARS))
  }
  c(
    "Population, 2020 Census"                  = fmt_num(d1$pop_census),
    "Density (people per sq. mile)"            = fmt_num(d1$pop_density),
    "Urban share of population"                = fmt_pct(100 * d1$percent_urban, 0),
    "Median household income (ACS)"            = paste0("$", fmt_num(d1$median_income)),
    "Share under 24 / 65 and over"             = sprintf("%.0f%% / %.0f%%", 100 * d1$share_under_24,
                                                         100 * d1$share_over_65),
    "State top income-tax rate, 2019 / 2020 (%)" = sprintf("%.2f / %.2f", pol$top_rate_2019, pol$top_rate_2020),
    "Governor's party, 2019 / 2020"            = paste(pol$gov_party_2019, "/", pol$gov_party_2020),
    "Days under stay-at-home order"            = fmt_num(d1$msahodays),
    "IRS AGI out-migration rate (%)"           = rate_pair("agi_out_rate_irs"),
    "IRS AGI in-migration rate (%)"            = rate_pair("agi_in_rate_irs"),
    "IRS AGI net migration rate (%)"           = rate_pair("agi_net_rate_irs")
  )
}

mult_stats <- county_stats(MULT_FIPS)

stats_table <- function(f) {
  s <- county_stats(f)
  df <- data.frame(names(s), unname(s), unname(mult_stats), check.names = FALSE)
  names(df) <- c(paste0("→ = ", PRE_LAB, " to ", POST_LAB, " average"), focal_short(f), "Multnomah")
  if (f == MULT_FIPS) return(table_grob(df[, 1:2], c(2.45, 2.25), base_size = BASE_PT - 0.5))
  table_grob(df, c(2.45, 1.15, 1.1), base_size = BASE_PT - 0.5)
}

did_cell <- function(row, o) {
  if (row$acs_ok == 0) return("n/a")
  digits <- if (o == "in_48") 4 else 2
  fmt_b(row[[paste0("b", o)]], row[[paste0("se", o)]], digits)
}

screen_table <- function(f) {
  r <- placebo |> filter(fips == f)
  m <- placebo |> filter(fips == MULT_FIPS)
  d1 <- panel |> filter(fips == f, year == max(PRE_YEARS))
  pass <- function(x) if (is.na(x)) "n/a" else if (x == 1) "pass" else "FAIL"
  other_pools <- c("stringency" = d1$sample_stringency, "narrow" = d1$sample_narrow,
                   "urban-COVID" = d1$sample_urban75_covid, "demog." = d1$sample_demog)
  other_pools <- names(other_pools)[other_pools == 1]
  rows <- list(
    c("Used as a placebo (both screens)?", if (r$in_screen == 1) "YES" else "NO", ""),
    c("Origin county identified in the ACS?", if (r$acs_ok == 1) "yes" else "NO", ""),
    c("(a) Own pre-trend, joint p-value",
      if (r$acs_ok == 1) sprintf("%.2f — %s", r$pre_p, pass(r$pass_pretrend)) else "n/a",
      sprintf("%.2f", m$pre_p)),
    c("(b) IRS slope distance from Multnomah",
      sprintf("%.2f — %s", r$irs_dist, pass(r$pass_irs)),
      sprintf("cutoff %.2f", irs_cutoff)),
    c("      rank among donors (1 = closest)", sprintf("%d of %d", r$irs_rank, length(donor_fips)), ""),
    c("      AGI out / in slope, 2016–19 (pp/yr)",
      sprintf("%.2f / %.2f", r$irs_slope_out, r$irs_slope_in),
      sprintf("%.2f / %.2f", m$irs_slope_out, m$irs_slope_in)),
    c("In NJ or NY (top-rate rises 2020–21)?", if (r$nj_ny == 1) "YES" else "no", ""),
    c("Other donor pools",
      if (length(other_pools)) paste(other_pools, collapse = "; ") else "none", ""),
    c("DiD, out-migration: pp (se)", did_cell(r, "out"), did_cell(m, "out")),
    c("DiD, out-of-state: pp (se)", did_cell(r, "out_state"), did_cell(m, "out_state")),
    c("DiD, in-migration: pp (se)", did_cell(r, "in_48"), did_cell(m, "in_48"))
  )
  df <- as.data.frame(do.call(rbind, rows))
  names(df) <- c("Screens; College × Post DiD", "This county", "Multnomah")
  table_grob(df, c(2.05, 1.6, 1.0), base_size = BASE_PT - 0.5)
}

# Multnomah's own page: the estimates the placebos are compared against
multnomah_did_table <- function() {
  m <- placebo |> filter(fips == MULT_FIPS)
  rows <- list(
    c("Own pre-trend, joint p-value (screen a)", sprintf("%.2f", m$pre_p)),
    c("AGI out / in slope, 2016–19 (pp/yr)", sprintf("%.2f / %.2f", m$irs_slope_out, m$irs_slope_in)),
    c("Donor IRS-slope distance cutoff (screen b)", sprintf("%.2f", irs_cutoff)),
    c("Donors passing both screens", sprintf("%d of %d", sum(placebo$in_screen %in% 1), length(donor_fips))),
    c("DiD, out-migration: pp (se)", did_cell(m, "out")),
    c("DiD, out-of-state: pp (se)", did_cell(m, "out_state")),
    c("DiD, in-migration: pp (se)", did_cell(m, "in_48"))
  )
  df <- as.data.frame(do.call(rbind, rows))
  names(df) <- c("College × Post DiD (the treated estimates)", "Multnomah")
  table_grob(df, c(2.6, 2.05), base_size = BASE_PT - 0.5)
}

# ---- Panel 2: migration trends -------------------------------------------------
trend_series <- c(
  agi_out_rate_irs = "IRS: AGI out-migration rate (%)",
  agi_in_rate_irs  = "IRS: AGI in-migration rate (%)",
  agi_net_rate_irs = "IRS: AGI net migration rate (%)",
  n2_out_rate_acs2 = "ACS: college out-migration (%)"
)
trend_long <- panel |>
  select(fips, year, all_of(names(trend_series))) |>
  pivot_longer(-c(fips, year), names_to = "series", values_to = "rate") |>
  filter(!is.na(rate)) |>
  mutate(series = factor(trend_series[series], levels = trend_series))

trend_plot <- function(f) {
  short <- focal_short(f)
  others <- if (f == MULT_FIPS) "Donor counties" else "Other donors"
  who_levels <- unique(c(short, "Multnomah", others))
  colours <- setNames(c(focal_colour(f), col_multn, col_others), c(short, "Multnomah", others))[who_levels]
  widths  <- setNames(c(0.8, 0.8, 0.25), c(short, "Multnomah", others))[who_levels]
  d <- trend_long |>
    mutate(who = case_when(fips == f ~ short, fips == MULT_FIPS ~ "Multnomah", TRUE ~ others),
           who = factor(who, levels = who_levels))
  ggplot(d, aes(year, rate, group = fips, colour = who, linewidth = who)) +
    geom_line(data = ~ filter(.x, who == others)) +
    geom_line(data = ~ filter(.x, who != others)) +
    geom_point(data = ~ filter(.x, who != others), size = 0.8) +
    geom_vline(xintercept = POLICY_YEAR + 0.5, linewidth = 0.3) +
    facet_wrap(~series, nrow = 1, scales = "free_y", drop = FALSE) +
    scale_colour_manual(values = colours, name = NULL, drop = FALSE) +
    scale_linewidth_manual(values = widths, guide = "none") +
    scale_x_continuous(breaks = seq(TREND_START, max(d$year), 2)) +
    labs(x = NULL, y = NULL, title = "Migration rates before and after 2020") +
    theme_page() +
    theme(legend.position = "top", legend.justification = "right",
          legend.box.margin = margin(-12, 0, -6, 0))
}

# ---- Panel 3: partner-county maps and tables ------------------------------------
partner_maps <- function(f, pf) {
  focal <- counties_sf |> filter(fips == f)
  stopifnot(nrow(focal) == 1)
  ctr <- st_coordinates(st_centroid(st_geometry(focal)))
  xlim <- ctr[1] + c(-1, 1) * MAP_HALF_SIDE_M
  ylim <- ctr[2] + c(-1, 1) * MAP_HALF_SIDE_M
  window <- st_as_sfc(st_bbox(c(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]),
                              crs = st_crs(counties_sf)))
  near <- counties_sf[lengths(st_intersects(counties_sf, window)) > 0, ] |> filter(fips != f)
  states_near <- states_sf[lengths(st_intersects(states_sf, window)) > 0, ]
  directions <- data.frame(direction = c("out", "in"))

  map_layers <- function(map_data, fill_var) {
    ggplot() +
      geom_sf(data = map_data, aes(fill = .data[[fill_var]]), colour = "white", linewidth = 0.1) +
      geom_sf(data = states_near, fill = NA, colour = "grey20", linewidth = 0.35) +
      geom_sf(data = focal, fill = "grey25", colour = "black", linewidth = 0.3) +
      facet_wrap(~label, nrow = 1) +
      coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, datum = NA) +
      theme_page() +
      theme(panel.border = element_rect(fill = NA, colour = "grey60", linewidth = 0.3),
            panel.spacing = unit(2, "mm"),
            strip.text = element_text(face = "plain", lineheight = 1.05),
            legend.title = element_text(size = BASE_PT - 0.5))
  }
  bar <- function() guide_colourbar(barwidth = unit(32, "mm"), barheight = unit(2, "mm"),
                                    title.position = "top")

  # Levels before 2020: share of the county's domestic movers, returns
  shares <- pf |> filter(measure == "n1", period == "pre")
  level_labels <- shares |>
    summarise(in_view = sum(share[partner_fips %in% near$fips]), .by = direction) |>
    mutate(label = sprintf("%s, %s\n%.0f%% of movers within the map",
                           if_else(direction == "out", "Destinations", "Origins"), PRE_LAB, in_view))
  level_data <- near |>
    cross_join(directions) |>
    left_join(shares |> select(direction, partner_fips, share),
              by = c("direction", "fips" = "partner_fips")) |>
    left_join(level_labels |> select(direction, label), by = "direction") |>
    mutate(label = factor(label, levels = level_labels$label[match(c("out", "in"), level_labels$direction)]))
  levels_map <- map_layers(level_data, "share") +
    scale_fill_gradient(low = ppb_tint(PPB_SEA, 0.88), high = ppb_shade(PPB_SEA, 0.45),
                        trans = "sqrt", na.value = "grey95", limits = c(0, NA),
                        name = "% of the county's domestic movers (returns)", guide = bar()) +
    labs(title = "Where its movers went and came from")

  # Change after 2020, as paper Figure 6; counties without a flow in both
  # windows are left grey, as in that figure
  change <- partner_flow_change(pf) |> filter(pre > 0, post > 0)
  change_labels <- c(out = "Out-migration (destination view)\n", `in` = "In-migration (origin view)\n")
  change_data <- near |>
    cross_join(directions) |>
    left_join(change |> select(direction, partner_fips, rate_change),
              by = c("direction", "fips" = "partner_fips")) |>
    mutate(label = factor(change_labels[direction], levels = change_labels))
  # Figure 6 scales each map to its largest change; with two maps on one
  # scale that washes out everything but one partner, so the scale is capped
  limit <- quantile(abs(change_data$rate_change), CHANGE_CAP_QUANTILE, na.rm = TRUE, names = FALSE)
  change_map <- map_layers(change_data, "rate_change") +
    scale_fill_gradient2(low = col_div_low, mid = "white", high = col_div_high, midpoint = 0,
                         na.value = "grey90", limits = c(-limit, limit), oob = scales::squish,
                         breaks = scales::breaks_extended(3),
                         name = sprintf("Rate change per %s of partner-county AGI (scale capped)",
                                        format(RATE_PER, big.mark = ",", scientific = FALSE)),
                         guide = bar()) +
    labs(title = sprintf("Change in AGI flows, %s to %s (as Figure 6)", PRE_LAB, POST_LAB))

  levels_map + change_map + plot_layout(widths = c(1, 1))
}

partner_table <- function(pf) {
  top <- function(dir) {
    wide <- pf |>
      filter(measure == "n1", direction == dir) |>
      select(partner_fips, period, share) |>
      pivot_wider(names_from = period, values_from = share) |>
      arrange(desc(pre)) |>
      head(N_TOP_PARTNERS) |>
      left_join(county_labels, by = c("partner_fips" = "fips"))
    stopifnot(!anyNA(wide$partner))
    # A partner missing from one window fell below the IRS disclosure threshold
    cell <- function(x) if_else(is.na(x), "n.d.", sprintf("%.1f", x))
    data.frame(partner = wide$partner, pre = cell(wide$pre), post = cell(wide$post))
  }
  heads <- c("Top destinations", "Top origins")
  head_row <- function(txt) data.frame(partner = txt, pre = "", post = "")
  df <- bind_rows(head_row(heads[1]), top("out"), head_row(heads[2]), top("in"))
  names(df) <- c("% of movers", PRE_LAB, POST_LAB)
  table_grob(df, c(2.0, 0.85, 0.85), base_size = BASE_PT - 0.3, bold_labels = heads)
}

# Concentration measures (returns) for one county, wide: one row per
# direction x measure, pre and post columns
concentration_wide <- function(f) {
  concentration |>
    filter(fips == f, measure == "n1") |>
    select(direction, period, all_of(names(conc_rows))) |>
    pivot_longer(all_of(names(conc_rows)), names_to = "row", values_to = "share") |>
    pivot_wider(names_from = period, values_from = share)
}

concentration_table <- function(f) {
  cell <- function(x) sprintf("%.0f", x)
  one <- function(g) concentration_wide(g) |> mutate(pre = cell(pre), post = cell(post))
  wide <- one(f) |>
    left_join(one(MULT_FIPS), by = c("direction", "row"), suffix = c("", "_mult"))
  heads <- c(out = "Out-movers, to:", `in` = "In-movers, from:")
  block <- function(dir) {
    w <- wide |> filter(direction == dir)
    w <- w[match(names(conc_rows), w$row), ]
    bind_rows(
      data.frame(label = heads[[dir]], pre = "", post = "", pre_mult = "", post_mult = ""),
      data.frame(label = paste0("   ", conc_rows), pre = w$pre, post = w$post,
                 pre_mult = w$pre_mult, post_mult = w$post_mult)
    )
  }
  df <- bind_rows(block("out"), block("in"))
  if (f == MULT_FIPS) {
    df <- df[, 1:3]
    names(df) <- c("% of movers", PRE_LAB, POST_LAB)
    return(table_grob(df, c(1.6, 0.9, 0.9), base_size = BASE_PT - 0.3, bold_labels = heads))
  }
  names(df) <- c("% of movers", PRE_LAB, POST_LAB, "Mult. pre", "Mult. post")
  table_grob(df, c(1.7, 0.8, 0.8, 0.9, 0.95), base_size = BASE_PT - 0.3, bold_labels = heads)
}

# ---- Panel 4: placebo event study -------------------------------------------------
es_outcomes <- c(out = "Out-migration", out_state = "Out-of-state")

es_plot <- function(f) {
  short <- focal_short(f)
  who_levels <- unique(c(short, "Multnomah"))
  colours <- setNames(c(focal_colour(f), col_multn), c(short, "Multnomah"))[who_levels]
  d <- placebo_es |>
    filter(fips %in% c(f, MULT_FIPS), outcome %in% names(es_outcomes))
  base <- d |> distinct(fips, outcome) |> mutate(year = 2019, b = 0, se = 0)
  d <- bind_rows(d, base) |>
    mutate(who = factor(if_else(fips == f, short, "Multnomah"), levels = who_levels),
           side = year > POLICY_YEAR,
           outcome = factor(es_outcomes[outcome], levels = es_outcomes),
           x = year + case_when(f == MULT_FIPS ~ 0, fips == f ~ -0.12, TRUE ~ 0.12))
  ggplot(d, aes(x, b, colour = who, group = interaction(who, side))) +
    geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept = POLICY_YEAR + 0.5, linewidth = 0.3) +
    geom_linerange(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), linewidth = 0.35) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 0.9) +
    facet_wrap(~outcome, nrow = 1) +
    scale_colour_manual(values = colours, name = NULL) +
    scale_x_continuous(breaks = seq(TREND_START, max(d$year), 2),
                       labels = function(y) sprintf("'%02d", y %% 100)) +
    labs(x = NULL, y = "College × Year (pp)",
         title = if (f == MULT_FIPS) "DiD event study (base 2019, 95% CI)"
                 else "Placebo event study (base 2019, 95% CI)") +
    theme_page() +
    theme(legend.position = "bottom", legend.box.margin = margin(-6, 0, 0, 0))
}

no_acs_note <- function() {
  wrap_elements(full = textGrob(
    "Not identified as an origin county in the\nACS public-use file, so no individual-level\nplacebo DiD can be run for this county.",
    gp = gpar(fontsize = BASE_PT + 1, col = "grey30")
  ))
}

# ---- Page assembly ---------------------------------------------------------------
page_header <- function(r, page_no, n_pages) {
  is_mult <- r$fips == MULT_FIPS
  status <- if (is_mult) {
    "Treated county (Preschool for All income tax, effective 2021)"
  } else if (r$in_screen == 1) {
    "Used as a placebo: passes both pre-2020 screens"
  } else {
    fails <- c(if (r$acs_ok == 0) "not identified in the ACS",
               if (r$pass_pretrend %in% 0) "fails the own pre-trend screen (a)",
               if (r$pass_irs %in% 0) "fails the IRS-trend screen (b)")
    paste0("Not used as a placebo: ", paste(fails, collapse = "; "))
  }
  grobTree(
    textGrob(sprintf("%s, %s", r$county_name, r$state_name), x = 0.005, y = 0.72, just = "left",
             gp = gpar(fontsize = 15, fontface = "bold")),
    textGrob(status, x = 0.005, y = 0.22, just = "left",
             gp = gpar(fontsize = 9.5, fontface = "bold",
                       col = if (is_mult || r$in_screen != 1) col_multn else col_donor)),
    textGrob(sprintf("%s · FIPS %05d · page %d of %d",
                     if (is_mult) "Treated county" else "Policy-match donor pool", r$fips, page_no, n_pages),
             x = 0.995, y = 0.72, just = "right", gp = gpar(fontsize = 7.5, col = "grey40")),
    segmentsGrob(x0 = 0, x1 = 1, y0 = 0, y1 = 0, gp = gpar(lwd = 1))
  )
}

build_page <- function(f, page_no, n_pages) {
  r <- placebo |> filter(fips == f)
  pf <- flows_by_county[[as.character(f)]]
  top_right <- if (f == MULT_FIPS) multnomah_did_table() else screen_table(f)
  did_panel <- if (r$acs_ok == 1) es_plot(f) else no_acs_note()
  wrap_elements(full = page_header(r, page_no, n_pages)) /
    (wrap_elements(full = stats_table(f)) + wrap_elements(full = top_right) +
       plot_layout(widths = c(1, 1.12))) /
    trend_plot(f) /
    partner_maps(f, pf) /
    (wrap_elements(full = partner_table(pf)) + wrap_elements(full = concentration_table(f)) + did_panel +
       plot_layout(widths = c(1, 1.45, 1.45))) +
    plot_layout(heights = c(0.55, 2.35, 1.75, 2.75, 2.75)) &
    theme(plot.background = element_rect(fill = "white", colour = NA))
}

title_block <- function(title) {
  wrap_elements(full = grobTree(
    textGrob(title, x = 0.005, y = 0.6, just = "left", gp = gpar(fontsize = 15, fontface = "bold")),
    segmentsGrob(x0 = 0, x1 = 1, y0 = 0, y1 = 0, gp = gpar(lwd = 1))
  ))
}
notes_block <- function(lines) {
  wrap_elements(full = textGrob(paste(lines, collapse = "\n"), x = 0.005, y = 0.95, just = c("left", "top"),
                                gp = gpar(fontsize = BASE_PT + 0.5, lineheight = 1.25)))
}

# ---- Index page: donors and screens ----------------------------------------------
index_page <- function(page_of) {
  cell <- function(b, se, digits = 2) if_else(is.na(b), "n/a", fmt_b(b, se, digits))
  df <- placebo |>
    mutate(page = page_of[as.character(fips)]) |>
    arrange(page) |>
    left_join(panel |> filter(year == max(PRE_YEARS)) |> select(fips, pop_census), by = "fips") |>
    transmute(
      Page = as.character(page),
      County = paste0(county_short, ", ", state_name),
      `Pop. 2020` = fmt_num(pop_census),
      `Used` = case_when(fips == MULT_FIPS ~ "treated", in_screen == 1 ~ "yes", TRUE ~ "no"),
      `(a) Pre-trend p` = if_else(is.na(pre_p), "n/a", sprintf("%.2f", pre_p)),
      `(b) IRS dist.` = if_else(fips == MULT_FIPS, "", sprintf("%.2f", irs_dist)),
      `Out-migration` = cell(bout, seout),
      `Out-of-state` = cell(bout_state, seout_state),
      `In-migration` = cell(bin_48, sein_48, 4)
    )
  notes <- c(
    "How to read these pages.  Each Policy-match donor county is treated as if it were Multnomah in the individual-level",
    "difference-in-differences (College × Post, ACS, 2016–2024 without 2020). A donor is used as a placebo only if it passes two",
    "pre-2020 screens: (a) its own out-migration event study shows no college / non-college pre-trend (joint p-value on the",
    "2016–2018 College × Year coefficients above the threshold in globals.do); (b) its 2016–2019 IRS AGI out- and in-migration",
    sprintf("rate slopes are among the closest half of donors to Multnomah's (distance at most %.2f). The three right-hand columns are", irs_cutoff),
    "the College × Post estimates in percentage points, standard errors in parentheses.",
    sprintf("IRS windows on the county pages: %s (before) and %s (after), by filing year; 2020 is left out.", PRE_LAB, POST_LAB)
  )
  title_block("Policy-match donor counties as placebos for Multnomah") /
    wrap_elements(full = table_grob(df, c(0.5, 2.5, 1, 0.8, 1.3, 1.2, 1.3, 1.3, 1.6), base_size = BASE_PT + 0.5,
                                    bold_labels = as.character(page_of[as.character(MULT_FIPS)]))) /
    notes_block(notes) +
    plot_layout(heights = c(0.5, 5.4, 4.3))
}

# ---- Comparison page: flow concentration across counties ----------------------------
comparison_page <- function(page_of) {
  pair <- function(pre, post) sprintf("%.0f → %.0f", pre, post)
  long <- bind_rows(lapply(focal_fips, function(f) concentration_wide(f) |> mutate(fips = f)))
  donor_median <- long |>
    filter(fips != MULT_FIPS) |>
    summarise(pre = median(pre), post = median(post), .by = c(direction, row)) |>
    mutate(County = "Donor median", order = 1.5)
  counties <- long |>
    left_join(placebo |> select(fips, county_short, state_name), by = "fips") |>
    mutate(County = paste0(county_short, ", ", state_name), order = page_of[as.character(fips)] - 2)
  col_names <- c(out.border_county = "Out: border cty", out.within_state = "Out: in-state",
                 out.border_state = "Out: border st.", out.top_flows = sprintf("Out: top %d", N_TOP_FLOWS),
                 in.border_county = "In: border cty", in.within_state = "In: in-state",
                 in.border_state = "In: border st.", in.top_flows = sprintf("In: top %d", N_TOP_FLOWS))
  df <- bind_rows(counties, donor_median) |>
    mutate(cell = pair(pre, post), col = paste(direction, row, sep = ".")) |>
    select(order, County, col, cell) |>
    pivot_wider(names_from = col, values_from = cell) |>
    arrange(order) |>
    select(County, all_of(names(col_names)))
  names(df) <- c("County", col_names)
  notes <- c(
    sprintf("Each cell is the %s share → the %s share, in percent of the county's domestic movers (IRS returns, summed over the", PRE_LAB, POST_LAB),
    "window's filing years). \"Out\" columns describe where people leaving the county went; \"In\" columns where arrivals came from.",
    "   Border county: a county adjacent to it (Census county adjacency file, water boundaries included), in any state.",
    "   In-state: elsewhere in its own state. Exact, from the IRS same-state and different-state totals.",
    "   Border state: a state that shares a border with its own. This is a lower bound: the IRS reports a county pair only above",
    "      a disclosure threshold, so small flows to scattered counties in those states are not counted.",
    sprintf("   Top %d: its %d largest county-to-county flows in that window.", N_TOP_FLOWS, N_TOP_FLOWS),
    "The same measures on an AGI basis are in flow_concentration.csv, with the share of movers the IRS discloses by county pair."
  )
  title_block("Migration concentration before and after 2020") /
    wrap_elements(full = table_grob(df, c(2.0, rep(1.25, 8)), base_size = BASE_PT,
                                    bold_labels = c("Multnomah, Oregon", "Donor median"))) /
    notes_block(notes) +
    plot_layout(heights = c(0.5, 5.6, 4.1))
}

# ---- Render -------------------------------------------------------------------------
# Multnomah, then screened placebos, then the rest; alphabetical by state and
# county within. Pages 1 and 2 are the index and the comparison table.
county_order <- placebo |>
  arrange(fips != MULT_FIPS, desc(in_screen), state_name, county_name) |>
  pull(fips)
page_of <- setNames(seq_along(county_order) + 2L, county_order)
n_pages <- length(county_order) + 2L

pages <- c(list(index_page(page_of), comparison_page(page_of)),
           lapply(county_order, function(f) build_page(f, page_of[[as.character(f)]], n_pages)))

page_margin <- theme(plot.margin = margin(18, 18, 12, 18),
                     plot.caption = element_text(size = BASE_PT - 1, colour = "grey35", hjust = 0,
                                                 lineheight = 1.1))
page_notes <- paste0(
  "Sources: IRS SOI county migration files by filing year (rates as % of the county's base-year AGI; mover shares use returns); ",
  "ACS 1-year PUMS, adults 25+.\n",
  "Bordering-state shares are lower bounds (IRS disclosure). Change maps: grey = no flow reported in one or both windows.\n",
  "DiD: 02_did_placebo.do, College × Year with the county as the focal county, 2016–2024 without 2020; bars are 95% confidence intervals."
)
print_page <- function(i) {
  caption <- if (i > 2) page_notes else NULL
  print(pages[[i]] + plot_annotation(caption = caption, theme = page_margin))
}

combined_path <- file.path(out_dir, "donor_onepagers.pdf")
cairo_pdf(combined_path, width = PAGE_W, height = PAGE_H, onefile = TRUE)
for (i in seq_along(pages)) print_page(i)
invisible(dev.off())
message("Saved: ", combined_path)

for (f in county_order) {
  r <- placebo |> filter(fips == f)
  slug <- gsub("[^a-z0-9]+", "_", tolower(paste(r$county_short, r$state_name)))
  cairo_pdf(file.path(out_dir, sprintf("county_%05d_%s.pdf", f, slug)), width = PAGE_W, height = PAGE_H)
  print_page(page_of[[as.character(f)]])
  invisible(dev.off())
}
message("Saved: ", length(county_order), " county pages in ", out_dir)
