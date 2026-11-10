# =============================================================================
# fig_diagrams.R
# Creates conceptual diagrams for the paper
#
# Outputs:
#   results/fig_empirical_approach.pdf
#   results/fig_data_comparison.pdf
# =============================================================================

library(grid)

# --- Shared plotplainblind palette ---
# Locate palette relative to this script (works when sourced or run interactively)
.this_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "code/R")
source(file.path(.this_dir, "colors_plotplainblind.R"))

irs_blue   <- col_irs_border;  irs_fill  <- col_irs_fill;  irs_light <- col_irs_light
acs_red    <- col_acs_border;  acs_fill  <- col_acs_fill;  acs_light <- col_acs_light
green_plus <- col_positive
red_minus  <- col_negative
desc_col   <- col_desc_text
dark_col   <- col_dark_text
dash_col   <- col_dash_line
arrow_col  <- col_arrow
fam        <- "sans"

# --- Balanced ACS county count, read from the Stata in-text numbers register ---
# (counties the ACS public-use microdata identify in every year 2016-2024;
# logged by 02_diagnostics.do as acs_n_counties_balanced_panel)
n_acs_balanced <- read_paper_number(".", "acs_n_counties_balanced_panel")

# --- Shared helpers ---
bx <- function(cx, cy, w, h, fill, border, lwd = 1.5, r = unit(3, "mm")) {
  grid.roundrect(
    x = unit(cx, "native"), y = unit(cy, "native"),
    width = unit(w, "native"), height = unit(h, "native"),
    r = r, gp = gpar(fill = fill, col = border, lwd = lwd)
  )
}

tx <- function(x, y, label, sz = 9, face = "plain", col = "black", j = "centre") {
  grid.text(
    label, x = unit(x, "native"), y = unit(y, "native"),
    just = j, gp = gpar(fontsize = sz, fontface = face, col = col, fontfamily = fam)
  )
}

ar <- function(x0, y0, x1, y1) {
  grid.lines(
    x = unit(c(x0, x1), "native"), y = unit(c(y0, y1), "native"),
    arrow = arrow(length = unit(2, "mm"), type = "closed"),
    gp = gpar(col = arrow_col, fill = arrow_col, lwd = 1.2)
  )
}

dl <- function(x0, y0, x1, y1) {
  grid.lines(
    x = unit(c(x0, x1), "native"), y = unit(c(y0, y1), "native"),
    gp = gpar(col = dash_col, lwd = 1.5, lty = "dashed")
  )
}

ln <- function(x0, y0, x1, y1, col = "#CCCCCC", lwd = 0.5) {
  grid.lines(
    x = unit(c(x0, x1), "native"), y = unit(c(y0, y1), "native"),
    gp = gpar(col = col, lwd = lwd)
  )
}

# Check mark drawn as a two-segment polyline (native units, centred on x, y),
# so it renders identically on every device and font (a U+2714 glyph fell
# back to a box or a thin tick depending on the installed fonts).
chk_mark <- function(x, y, col, w = 1.8, lwd = 2.4) {
  grid.lines(
    x = unit(c(x - 0.9 * w, x - 0.25 * w, x + w), "native"),
    y = unit(c(y, y - 0.7 * w, y + 0.9 * w), "native"),
    gp = gpar(col = col, lwd = lwd, lineend = "round", linejoin = "round")
  )
}

open_pdf <- function(file, width, height) {
  if (capabilities("cairo")) {
    cairo_pdf(file, width = width, height = height)
  } else {
    pdf(file, width = width, height = height)
  }
}


# ==========================================================================
# FIGURE 1: EMPIRICAL APPROACH
# ==========================================================================
draw_empirical_approach <- function(out_file, with_title = FALSE) {

  # Crop ~28 units of empty space below the Step 3 box (yscale lower bound
  # raised from 0 to 28) and shorten the PDF accordingly so the LaTeX figure
  # note doesn't get pushed down. Net effect on box-in-inches: roughly +6%
  # taller (PDF height 13->11 vs yscale span 155->124).
  open_pdf(out_file, width = 10, height = 11)
  grid.newpage()

  pushViewport(viewport(
    x = 0.5, y = 0.5, width = 0.94, height = 0.96,
    xscale = c(0, 130), yscale = c(28, 152)
  ))

  # Step 1 colors (turquoise family)
  s1_fill <- ppb_tint(PPB_TURQUOISE, 0.70); s1_bdr <- PPB_TURQUOISE; s1_txt <- ppb_shade(PPB_TURQUOISE, 0.40)
  # SDID colors (sea family)
  sd_fill <- ppb_tint(PPB_SEA, 0.70); sd_bdr <- PPB_SEA; sd_txt <- ppb_shade(PPB_SEA, 0.40)
  # Output box colors
  ob_fill <- "white"; ob_bdr <- PPB_GREY; ob_txt <- dark_col
  # IRS sub colors (sea family)
  irs_sf <- irs_light; irs_sb <- col_irs_border; irs_txt <- col_irs_text
  irs_of <- "white";   irs_ob <- col_irs_border
  # ACS sub colors (vermillion family)
  acs_sf <- acs_light; acs_sb <- col_acs_border; acs_txt <- col_acs_text
  acs_of <- "white";   acs_ob <- col_acs_border
  # Step 3 colors (orangebrown family)
  s3_fill <- ppb_tint(PPB_ORANGEBROWN, 0.70); s3_bdr <- PPB_ORANGEBROWN; s3_txt <- ppb_shade(PPB_ORANGEBROWN, 0.40)

  # --- TITLE (only when with_title=TRUE; paper version omits it because
  # LaTeX \caption{} provides the title) ---
  if (isTRUE(with_title)) {
    tx(65, 148, "Empirical Approach", sz = 20, face = "bold")
  }

  # Box widths and line breaks are sized for DejaVu Sans, the widest font the
  # "sans" family resolves to (the cluster); narrower fonts only add margin.

  # --- STEP 1 ---
  bx(65, 140, 100, 11, NA, s1_bdr, r = unit(5, "mm"))
  tx(65, 143, "Are people moving?",
     sz = 15, face = "bold.italic", col = s1_txt)
  tx(65, 139.5,
     "Step 1: Comparable migration rates (AGI, returns, exemptions) for IRS and ACS",
     sz = 10.5, col = desc_col)
  tx(65, 137,
     "\u2192 Synthetic Difference\u2013in\u2013Differences applied to both sources",
     sz = 10, face = "italic", col = desc_col)

  ar(65, 134.5, 65, 132.5)

  # --- SDID BOX ---
  bx(65, 124.5, 104, 16, NA, sd_bdr, r = unit(4, "mm"))
  tx(65, 130, "Synthetic Difference\u2013in\u2013Differences (SDID)",
     sz = 14, face = "bold", col = sd_txt)
  tx(65, 126.5,
     "One treated unit (Multnomah County)",
     sz = 10, col = desc_col)
  tx(65, 124,
     "Donor pools: All, Policy match, COVID, Demographic, Stringency, Narrow",
     sz = 10, col = desc_col)
  tx(65, 121.5,
     "Highlighted specs: IRS / ACS College \u00d7 {All, Stringency, Policy match},",
     sz = 10, col = desc_col)
  tx(65, 119.2,
     "with covariates, excluding 2020",
     sz = 10, col = desc_col)

  ar(48, 116.5, 42, 115)
  ar(82, 116.5, 88, 115)

  # --- OUTPUT BOXES ---
  bx(42, 111, 32, 8, NA, ob_bdr, lwd = 1)
  tx(42, 111, "Specification Curves", sz = 12, face = "bold", col = ob_txt)

  bx(88, 111, 32, 8, NA, ob_bdr, lwd = 1)
  tx(88, 111, "Event Studies", sz = 12, face = "bold", col = ob_txt)

  # --- DASHED LINE 1 ---
  dl(3, 102.5, 127, 102.5)
  tx(65, 104.5, "Step 2: Source\u2013Specific Analyses",
     sz = 13, face = "bold.italic", col = dash_col)

  # --- IRS & ACS MAIN BOXES ---
  bx(33, 94, 52, 12, NA, irs_blue, r = unit(5, "mm"))
  tx(33, 98, "Where are people moving?",
     sz = 14, face = "bold.italic", col = irs_txt)
  tx(33, 94.5, "IRS County\u2013to\u2013County Flows",
     sz = 12, face = "bold", col = irs_txt)
  tx(33, 91, "Geographic patterns of in/out flows",
     sz = 10.5, col = desc_col)

  bx(97, 94, 52, 12, NA, acs_red, r = unit(5, "mm"))
  tx(97, 98, "Who is moving?",
     sz = 14, face = "bold.italic", col = acs_txt)
  tx(97, 94.5, "ACS Individual\u2013Level Data",
     sz = 12, face = "bold", col = acs_txt)
  tx(97, 91, "Conditional on demographic characteristics",
     sz = 10.5, col = desc_col)

  ar(20, 88, 17, 83.5)
  ar(46, 88, 49, 83.5)
  ar(84, 88, 81, 83.5)
  ar(110, 88, 113, 83.5)

  # --- IRS SUB-BOXES (title + up to three centred description lines) ---
  bx(17, 76, 30, 15, NA, irs_sb)
  tx(17, 80.5, "Descriptive Maps", sz = 12, face = "bold", col = irs_txt)
  tx(17, 76.5, "Change in AGI flows", sz = 10, col = desc_col)
  tx(17, 74.2, "to/from Multnomah", sz = 10, col = desc_col)

  bx(49, 76, 30, 15, NA, irs_sb)
  tx(49, 80.5, "PPML Flow Models", sz = 12, face = "bold", col = irs_txt)
  tx(49, 76.5, expression(italic(M)[ijt] * " with flow FE,"), sz = 10, col = desc_col)
  tx(49, 74.2, "time\u2013varying controls", sz = 10, col = desc_col)

  # --- ACS SUB-BOXES ---
  bx(81, 76, 30, 15, NA, acs_sb)
  tx(81, 80.5, "Conditional Means", sz = 12, face = "bold", col = acs_txt)
  tx(81, 77.3, "Migration rates by", sz = 10, col = desc_col)
  tx(81, 75, "income, education,", sz = 10, col = desc_col)
  tx(81, 72.7, "age, # of children", sz = 10, col = desc_col)

  bx(113, 76, 30, 15, NA, acs_sb)
  tx(113, 80.5, "DiD Models", sz = 12, face = "bold", col = acs_txt)
  tx(113, 77.3, "College education as", sz = 10, col = desc_col)
  tx(113, 75, "treatment proxy", sz = 10, col = desc_col)
  tx(113, 72.7, "Lower 48 + DC; CA/OR/WA", sz = 10, col = desc_col)

  ar(43, 68.5, 39, 65.5)
  ar(55, 68.5, 59, 65.5)
  ar(108, 68.5, 102, 65.5)
  ar(118, 68.5, 121.5, 65.5)

  # --- PPML SUB-SUB-BOXES ---
  bx(38, 61.5, 19, 8, NA, irs_ob, lwd = 1)
  tx(38, 61.5, "Event Studies", sz = 10, face = "bold", col = ob_txt)

  bx(60, 61.5, 19, 8, NA, irs_ob, lwd = 1)
  tx(60, 61.5, "Placebo Tests", sz = 10, face = "bold", col = ob_txt)

  # --- DiD SUB-SUB-BOXES (inside the 0-130 frame) ---
  bx(101, 61.5, 19, 8, NA, acs_ob, lwd = 1)
  tx(101, 61.5, "Out\u2013Migration", sz = 10, face = "bold", col = ob_txt)

  bx(121.5, 61.5, 17, 8, NA, acs_ob, lwd = 1)
  tx(121.5, 61.5, "In\u2013Migration", sz = 10, face = "bold", col = ob_txt)

  # --- DASHED LINE 2 + converging arrows to Step 3 ---
  dl(3, 53, 127, 53)
  ar(50, 53, 58, 48)
  ar(80, 53, 72, 48)

  # --- STEP 3 ---
  bx(65, 41, 90, 13, NA, s3_bdr, r = unit(5, "mm"))
  tx(65, 44.5, "Step 3: Estimated Effects on Tax Revenues",
     sz = 14, face = "bold", col = s3_txt)
  tx(65, 40.5,
     "Combine migration estimates with the tax-rate structure",
     sz = 10.5, col = desc_col)
  tx(65, 38,
     "to quantify the revenue impact",
     sz = 10.5, col = desc_col)

  popViewport()
  dev.off()
  cat("Saved:", out_file, "\n")
}


# ==========================================================================
# FIGURE 2: DATA SOURCES AND MIGRATION MEASURES
# ==========================================================================
draw_data_comparison <- function(out_file, with_title = FALSE) {

  # Empty space below the units boxes cropped (yscale lower bound 40 -> 50,
  # PDF height 12 -> 11, so a native unit keeps the same physical height).
  open_pdf(out_file, width = 11, height = 11)
  grid.newpage()

  pushViewport(viewport(
    x = 0.5, y = 0.5, width = 0.94, height = 0.96,
    xscale = c(0, 130), yscale = c(50, 170)
  ))

  # -----------------------------------------------------------------------
  # TITLE
  # -----------------------------------------------------------------------
  if (isTRUE(with_title)) {
    tx(65, 165, "Data Sources and Migration Measures", sz = 20, face = "bold")
  }

  # -----------------------------------------------------------------------
  # PANEL A: Data Source Comparison
  # -----------------------------------------------------------------------
  tx(65, 159, "Panel A: Data Source Comparison",
     sz = 15, face = "bold.italic", col = dark_col)

  # IRS box (outline only \u2014 bigger / no colored fill)
  bx(33, 142, 54, 26, NA, irs_blue, r = unit(5, "mm"))
  tx(33, 152, "IRS Statistics of Income (SOI)",
     sz = 14, face = "bold", col = col_irs_text)

  irs_x <- 9
  tx(irs_x, 148,   "+  Near\u2013universal coverage (all tax filers)",
     sz = 10.5, col = green_plus, j = "left")
  tx(irs_x, 145.5, "+  Unambiguous residence (tax return address)",
     sz = 10.5, col = green_plus, j = "left")
  tx(irs_x, 143,   "+  County\u2013to\u2013county flows with AGI",
     sz = 10.5, col = green_plus, j = "left")
  tx(irs_x, 140,   "\u2212  No individual characteristics (age, educ.)",
     sz = 10.5, col = red_minus, j = "left")
  tx(irs_x, 137.5, "\u2212  Available only through 2022 (2 post years)",
     sz = 10.5, col = red_minus, j = "left")
  tx(irs_x, 135,   "\u2212  Aggregate: county\u2013pair is unit of observation",
     sz = 10.5, col = red_minus, j = "left")

  # ACS box (outline only)
  bx(97, 142, 54, 26, NA, acs_red, r = unit(5, "mm"))
  tx(97, 152, "American Community Survey (ACS)",
     sz = 14, face = "bold", col = col_acs_text)

  acs_x <- 73
  tx(acs_x, 148,   "+  Individual\u2013level data with demographics",
     sz = 10.5, col = green_plus, j = "left")
  tx(acs_x, 145.5, "+  Available through 2024 (4 post years)",
     sz = 10.5, col = green_plus, j = "left")
  tx(acs_x, 143,   "+  Enables DiD with education as treatment proxy",
     sz = 10.5, col = green_plus, j = "left")
  tx(acs_x, 140,   "\u2212  1% sample: small N, measurement error",
     sz = 10.5, col = red_minus, j = "left")
  tx(acs_x, 137.5, paste0("\u2212  Small counties suppressed (", n_acs_balanced, " identified)"),
     sz = 10.5, col = red_minus, j = "left")
  tx(acs_x, 135,   "\u2212  Residence definition may capture temp. moves",
     sz = 10.5, col = red_minus, j = "left")

  # -----------------------------------------------------------------------
  # DASHED LINE
  # -----------------------------------------------------------------------
  dl(5, 126, 125, 126)

  # -----------------------------------------------------------------------
  # PANEL B: Migration Measures
  # -----------------------------------------------------------------------
  tx(65, 123, "Panel B: Migration Measures",
     sz = 15, face = "bold.italic", col = dark_col)

  # --- Table ---
  # Column boundaries (sized for DejaVu Sans; each cell holds two lines)
  tl <- 6; tr <- 124    # table left/right
  c1 <- 32               # end of Measure col
  c2 <- 79               # end of Definition col
  c3 <- 90               # end of IRS col
  c4 <- 104              # end of ACS col

  # Row boundaries (top to bottom)
  rh  <- 119             # header top
  r0  <- 115.5           # header bottom / row 1 top
  r1  <- 107.5           # row 1 bottom / row 2 top
  r2  <- 99.5            # row 2 bottom / row 3 top
  r3  <- 91.5            # row 3 bottom

  # Header background
  grid.rect(
    x = unit((tl + tr) / 2, "native"), y = unit((rh + r0) / 2, "native"),
    width = unit(tr - tl, "native"), height = unit(rh - r0, "native"),
    gp = gpar(fill = ppb_shade(PPB_SEA, 0.30), col = NA)
  )

  # Alternating row shading (row 2 gets light grey)
  grid.rect(
    x = unit((tl + tr) / 2, "native"), y = unit((r1 + r2) / 2, "native"),
    width = unit(tr - tl, "native"), height = unit(r1 - r2, "native"),
    gp = gpar(fill = "#F0F0F0", col = NA)
  )

  # Table border
  grid.rect(
    x = unit((tl + tr) / 2, "native"), y = unit((rh + r3) / 2, "native"),
    width = unit(tr - tl, "native"), height = unit(rh - r3, "native"),
    gp = gpar(fill = NA, col = "#CCCCCC", lwd = 0.5)
  )

  # Horizontal lines
  ln(tl, r0, tr, r0, col = "#CCCCCC", lwd = 1)
  ln(tl, r1, tr, r1, col = "#EEEEEE", lwd = 0.5)
  ln(tl, r2, tr, r2, col = "#EEEEEE", lwd = 0.5)

  # Header text
  hdr_y <- (rh + r0) / 2
  tx((tl + c1) / 2, hdr_y, "Measure",    sz = 11.5, face = "bold", col = "white")
  tx((c1 + c2) / 2, hdr_y, "Definition", sz = 11.5, face = "bold", col = "white")
  tx((c2 + c3) / 2, hdr_y, "IRS",        sz = 11.5, face = "bold", col = "white")
  tx((c3 + c4) / 2, hdr_y, "ACS",        sz = 11.5, face = "bold", col = "white")
  tx((c4 + tr) / 2, hdr_y, "Used In",    sz = 11.5, face = "bold", col = "white")

  # "Not available" dash; the check mark is drawn by chk_mark() (vector path).
  na_sym <- "\u2014\u2014"

  # Each row: measure label and definition on two lines (upper / lower),
  # check mark (smaller, so it stays inside the 8-unit row) and method.
  up <- 1.4; lo <- -1.4
  chk_w <- 1.4

  # Row 1: Individual migration -- ACS only
  y1 <- (r0 + r1) / 2
  tx(tl + 2, y1 + up, "Individual migration",
     sz = 10.5, face = "italic", col = dark_col, j = "left")
  tx(tl + 2, y1 + lo, expression(italic(M)[hit]),
     sz = 10.5, col = dark_col, j = "left")
  tx(c1 + 2, y1 + up, "1 if person h moved into (out of)",
     sz = 10.5, col = desc_col, j = "left")
  tx(c1 + 2, y1 + lo, "county i in year t",
     sz = 10.5, col = desc_col, j = "left")
  tx((c2 + c3) / 2, y1, na_sym, sz = 9, col = PPB_GREY)
  chk_mark((c3 + c4) / 2, y1 + 1, col = green_plus, w = chk_w)
  tx((c3 + c4) / 2, y1 - 2.2, paste0("(", n_acs_balanced, " counties)"), sz = 7, col = desc_col)
  tx((c4 + tr) / 2, y1, "DiD", sz = 10, face = "bold", col = irs_blue)

  # Row 2: County-pair flows -- IRS only (ACS not used for flows)
  y2 <- (r1 + r2) / 2
  tx(tl + 2, y2 + up, "County\u2013pair flows",
     sz = 10.5, face = "italic", col = dark_col, j = "left")
  tx(tl + 2, y2 + lo, expression(italic(M)[ijt]),
     sz = 10.5, col = dark_col, j = "left")
  tx(c1 + 2, y2 + up, "Individuals, returns, or AGI moving",
     sz = 10.5, col = desc_col, j = "left")
  tx(c1 + 2, y2 + lo, "from county i to county j in year t",
     sz = 10.5, col = desc_col, j = "left")
  chk_mark((c2 + c3) / 2, y2, col = green_plus, w = chk_w)
  tx((c3 + c4) / 2, y2, na_sym, sz = 9, col = PPB_GREY)
  tx((c4 + tr) / 2, y2, "PPML", sz = 10, face = "bold", col = irs_blue)

  # Row 3: County migration rates -- both sources
  y3 <- (r2 + r3) / 2
  tx(tl + 2, y3 + up, "County migration rates",
     sz = 10.5, face = "italic", col = dark_col, j = "left")
  tx(tl + 2, y3 + lo, expression(italic(M)[it]),
     sz = 10.5, col = dark_col, j = "left")
  tx(c1 + 2, y3 + up, "In-, out-, and net in-migration rate",
     sz = 10.5, col = desc_col, j = "left")
  tx(c1 + 2, y3 + lo, "for county i in year t",
     sz = 10.5, col = desc_col, j = "left")
  chk_mark((c2 + c3) / 2, y3, col = green_plus, w = chk_w)
  chk_mark((c3 + c4) / 2, y3, col = green_plus, w = chk_w)
  tx((c4 + tr) / 2, y3, "SDID", sz = 10, face = "bold", col = irs_blue)

  # -----------------------------------------------------------------------
  # KEY BOX
  # -----------------------------------------------------------------------
  bx(65, 83.5, 96, 10, NA, PPB_ORANGEBROWN, lwd = 1, r = unit(4, "mm"))
  tx(65, 85.5,
     "Key: County migration rates can be computed from both sources, enabling",
     sz = 11, col = dark_col)
  tx(65, 82,
     "head\u2013to\u2013head comparison via SDID before using source\u2013specific methods.",
     sz = 11, face = "italic", col = dark_col)

  # -----------------------------------------------------------------------
  # UNITS OF MIGRATION
  # -----------------------------------------------------------------------
  tx(65, 74.5, "Units of Migration", sz = 15, face = "bold", col = dark_col)

  unit_h <- 12; unit_w <- 36; unit_y <- 65.5

  # AGI (outline only)
  bx(22,  unit_y, unit_w, unit_h, NA, PPB_SKY)
  tx(22,  unit_y + 3, "AGI (income)", sz = 12, face = "bold", col = dark_col)
  tx(22,  unit_y,     "IRS: AGI",              sz = 10.5, col = irs_blue)
  tx(22,  unit_y - 2.5, "ACS: Household income",  sz = 10.5, col = acs_red)

  # Returns (outline only)
  bx(65,  unit_y, unit_w, unit_h, NA, PPB_SKY)
  tx(65,  unit_y + 3, "Returns (households)", sz = 12, face = "bold", col = dark_col)
  tx(65,  unit_y,     "IRS: Returns",         sz = 10.5, col = irs_blue)
  tx(65,  unit_y - 2.5, "ACS: Households",       sz = 10.5, col = acs_red)

  # Exemptions (outline only)
  bx(108, unit_y, unit_w, unit_h, NA, PPB_SKY)
  tx(108, unit_y + 3, "Exemptions (individuals)",
     sz = 12, face = "bold", col = dark_col)
  tx(108, unit_y,     "IRS: Exemptions",       sz = 10.5, col = irs_blue)
  tx(108, unit_y - 2.5, "ACS: Individuals",       sz = 10.5, col = acs_red)

  # -----------------------------------------------------------------------
  # NOTES \u2014 only when with_title=TRUE; paper version uses LaTeX \figurenotes{}
  # -----------------------------------------------------------------------
  if (isTRUE(with_title)) {
    tx(65, 54,
       paste0("Notes: IRS data covers tax years 2016\u20132022. ",
              "ACS data covers 2016\u20132024. AGI = Adjusted Gross Income."),
       sz = 9.5, face = "italic", col = desc_col)
  }

  popViewport()
  dev.off()
  cat("Saved:", out_file, "\n")
}


# ==========================================================================
# Generate both figures
# ==========================================================================
out_dir <- file.path("results", "figures")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Paper versions: no in-figure title or bottom notes (LaTeX \caption{} and
# \figurenotes{} provide those). The "_titled" variants keep the title and
# notes for use in slides or standalone reference.
draw_empirical_approach(file.path(out_dir, "fig_empirical_approach.pdf"))
draw_empirical_approach(file.path(out_dir, "fig_empirical_approach_titled.pdf"),
                        with_title = TRUE)
draw_data_comparison(file.path(out_dir, "fig_data_comparison.pdf"))
draw_data_comparison(file.path(out_dir, "fig_data_comparison_titled.pdf"),
                     with_title = TRUE)

# Overleaf mirror — paper versions only (the titled variants stay local).
if (exists("cfg")) {
  copied <- overleaf_copy(cfg, file.path(out_dir, c("fig_empirical_approach.pdf", "fig_data_comparison.pdf")))
  if (length(copied) > 0) cat("Overleaf: copied", length(copied), "diagrams to", cfg$dir_ol_fig, "\n")
}
