# =============================================================================
# fig_diagrams.R
# Generate conceptual diagrams for the paper (Figures 2 and 3)
#
# Figure 2: Data Sources and Migration Measures
# Figure 3: Empirical Approach Flowchart
#
# Uses grid graphics for precise layout control
# =============================================================================

library(grid)
library(ggplot2)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Draw a rounded rectangle with text
draw_box <- function(x, y, w, h, label, gp_fill = gpar(fill = "white"),
                     gp_border = gpar(col = "black", lwd = 1.5),
                     gp_text = gpar(fontsize = 9, fontface = "plain"),
                     just = "centre", r = unit(3, "mm")) {
  grid.roundrect(x = x, y = y, width = w, height = h,
                 gp = gpar(fill = gp_fill$fill, col = gp_border$col,
                            lwd = gp_border$lwd),
                 r = r, just = just)
  grid.text(label, x = x, y = y, gp = gp_text, just = "centre")
}

# Draw an arrow between two points
draw_arrow <- function(x0, y0, x1, y1, gp = gpar(lwd = 1.5)) {
  grid.lines(x = unit(c(x0, x1), "npc"),
             y = unit(c(y0, y1), "npc"),
             gp = gp,
             arrow = arrow(length = unit(2, "mm"), type = "closed"))
}

# =============================================================================
# FIGURE 2: DATA SOURCES AND MIGRATION MEASURES
# =============================================================================

make_fig_data_comparison <- function(outfile) {

  if (grepl("\\.pdf$", outfile)) {
    pdf(outfile, width = 10, height = 8.5)
  } else {
    png(outfile, width = 10, height = 8.5, units = "in", res = 300)
  }

  grid.newpage()
  pushViewport(viewport(width = 0.95, height = 0.95))

  # ---- Title ----
  grid.text("Data Sources and Migration Measures",
            x = 0.5, y = 0.97,
            gp = gpar(fontsize = 14, fontface = "bold"))

  # ---- TOP SECTION: Data Source Comparison ----
  grid.text("Panel A: Data Source Comparison",
            x = 0.5, y = 0.92,
            gp = gpar(fontsize = 11, fontface = "bold.italic"))

  # IRS Box
  irs_x <- 0.25; irs_y <- 0.78; bw <- 0.42; bh <- 0.22
  grid.roundrect(x = irs_x, y = irs_y, width = bw, height = bh,
                 gp = gpar(fill = "#D6EAF8", col = "#2C3E50", lwd = 2),
                 r = unit(4, "mm"))
  grid.text("IRS Statistics of Income (SOI)", x = irs_x, y = irs_y + 0.085,
            gp = gpar(fontsize = 11, fontface = "bold", col = "#2C3E50"))

  irs_text <- c(
    "+ Near-universal coverage (all tax filers)",
    "+ Unambiguous residence (tax return address)",
    "+ County-to-county flows with AGI",
    "- No individual characteristics (age, educ.)",
    "- Available only through 2022 (2 post years)",
    "- Aggregate: county-pair is unit of observation"
  )
  for (i in seq_along(irs_text)) {
    col <- if (startsWith(irs_text[i], "+")) "#196F3D" else "#922B21"
    grid.text(irs_text[i], x = irs_x - 0.19, y = irs_y + 0.05 - i * 0.022,
              gp = gpar(fontsize = 7.5, col = col), just = "left")
  }

  # ACS Box
  acs_x <- 0.75; acs_y <- 0.78
  grid.roundrect(x = acs_x, y = acs_y, width = bw, height = bh,
                 gp = gpar(fill = "#FADBD8", col = "#2C3E50", lwd = 2),
                 r = unit(4, "mm"))
  grid.text("American Community Survey (ACS)", x = acs_x, y = acs_y + 0.085,
            gp = gpar(fontsize = 11, fontface = "bold", col = "#2C3E50"))

  acs_text <- c(
    "+ Individual-level data with demographics",
    "+ Available through 2024 (4 post years)",
    "+ Enables DiD with education as treatment proxy",
    "- 1% sample: small N, measurement error",
    "- County suppressed for small counties (389 obs.)",
    "- Residence definition may capture temp. moves"
  )
  for (i in seq_along(acs_text)) {
    col <- if (startsWith(acs_text[i], "+")) "#196F3D" else "#922B21"
    grid.text(acs_text[i], x = acs_x - 0.19, y = acs_y + 0.05 - i * 0.022,
              gp = gpar(fontsize = 7.5, col = col), just = "left")
  }

  # ---- Divider ----
  grid.lines(x = c(0.05, 0.95), y = c(0.635, 0.635),
             gp = gpar(col = "grey60", lwd = 1, lty = 2))

  # ---- BOTTOM SECTION: Migration Measures ----
  grid.text("Panel B: Migration Measures",
            x = 0.5, y = 0.61,
            gp = gpar(fontsize = 11, fontface = "bold.italic"))

  # Table header
  hdr_y <- 0.565
  grid.rect(x = 0.5, y = hdr_y, width = 0.88, height = 0.035,
            gp = gpar(fill = "#2C3E50", col = NA))
  grid.text("Measure", x = 0.12, y = hdr_y,
            gp = gpar(fontsize = 9, fontface = "bold", col = "white"), just = "left")
  grid.text("Definition", x = 0.32, y = hdr_y,
            gp = gpar(fontsize = 9, fontface = "bold", col = "white"), just = "left")
  grid.text("IRS", x = 0.72, y = hdr_y,
            gp = gpar(fontsize = 9, fontface = "bold", col = "white"))
  grid.text("ACS", x = 0.82, y = hdr_y,
            gp = gpar(fontsize = 9, fontface = "bold", col = "white"))
  grid.text("Used In", x = 0.91, y = hdr_y,
            gp = gpar(fontsize = 9, fontface = "bold", col = "white"))

  # Table rows
  measures <- data.frame(
    measure_txt = c(
      "Individual migration (Mhit)",
      "County-pair flows (Mijt)",
      "County migration rates (Mit)"
    ),
    definition = c(
      "1 if person h moved in/out of county i in year t",
      "# individuals/returns/AGI from county i to j in t",
      "In-, out-, net in-migration rate for county i in t"
    ),
    irs = c("--", "\u2713", "\u2713"),
    acs = c("\u2713 (389 counties)", "\u2713 (limited)", "\u2713"),
    used = c("DiD (Sec. 4B)", "PPML (Sec. 4B)", "SDID (Sec. 4A)"),
    stringsAsFactors = FALSE
  )

  for (i in 1:3) {
    row_y <- hdr_y - i * 0.045
    bg <- if (i %% 2 == 1) "#F2F4F4" else "white"
    grid.rect(x = 0.5, y = row_y, width = 0.88, height = 0.04,
              gp = gpar(fill = bg, col = NA))
    grid.text(measures$measure_txt[i], x = 0.12, y = row_y,
              gp = gpar(fontsize = 8.5, fontface = "italic"), just = "left")
    grid.text(measures$definition[i], x = 0.32, y = row_y,
              gp = gpar(fontsize = 8), just = "left")
    grid.text(measures$irs[i], x = 0.72, y = row_y,
              gp = gpar(fontsize = 8.5))
    grid.text(measures$acs[i], x = 0.82, y = row_y,
              gp = gpar(fontsize = 8))
    grid.text(measures$used[i], x = 0.91, y = row_y,
              gp = gpar(fontsize = 7.5, col = "#5B2C6F"))
  }

  # ---- KEY INSIGHT BOX ----
  grid.roundrect(x = 0.5, y = 0.32, width = 0.85, height = 0.08,
                 gp = gpar(fill = "#FEF9E7", col = "#B7950B", lwd = 1.5),
                 r = unit(3, "mm"))
  grid.text("Key: County migration rates can be computed from both sources, enabling",
            x = 0.5, y = 0.335, gp = gpar(fontsize = 9))
  grid.text("head-to-head comparison via SDID before using source-specific methods.",
            x = 0.5, y = 0.305, gp = gpar(fontsize = 9, fontface = "italic"))

  # ---- BOTTOM: Units available ----
  grid.text("Units of Migration", x = 0.5, y = 0.24,
            gp = gpar(fontsize = 10, fontface = "bold"))

  units_data <- data.frame(
    unit = c("AGI (income)", "Returns (households)", "Exemptions (individuals)"),
    irs_label = c("IRS: AGI", "IRS: Returns", "IRS: Exemptions"),
    acs_label = c("ACS: Household income", "ACS: Households", "ACS: Individuals"),
    stringsAsFactors = FALSE
  )

  for (i in 1:3) {
    bx <- 0.17 + (i - 1) * 0.33
    by <- 0.16
    grid.roundrect(x = bx, y = by, width = 0.28, height = 0.11,
                   gp = gpar(fill = "#EBF5FB", col = "#2980B9", lwd = 1),
                   r = unit(3, "mm"))
    grid.text(units_data$unit[i], x = bx, y = by + 0.03,
              gp = gpar(fontsize = 9, fontface = "bold"))
    grid.text(paste0("IRS: ", sub(".*: ", "", units_data$irs_label[i])),
              x = bx, y = by - 0.005,
              gp = gpar(fontsize = 7.5, col = "#2471A3"))
    grid.text(paste0("ACS: ", sub(".*: ", "", units_data$acs_label[i])),
              x = bx, y = by - 0.03,
              gp = gpar(fontsize = 7.5, col = "#C0392B"))
  }

  # ---- Notes ----
  grid.text(
    "Notes: IRS data covers tax years 2016-2022. ACS data covers 2016-2024. AGI = Adjusted Gross Income.",
    x = 0.5, y = 0.05,
    gp = gpar(fontsize = 7, col = "grey40", fontface = "italic")
  )

  popViewport()
  dev.off()
  message("  Created: ", outfile)
}


# =============================================================================
# FIGURE 3: EMPIRICAL APPROACH FLOWCHART
# =============================================================================

make_fig_empirical_approach <- function(outfile) {

  if (grepl("\\.pdf$", outfile)) {
    pdf(outfile, width = 11, height = 8)
  } else {
    png(outfile, width = 11, height = 8, units = "in", res = 300)
  }

  grid.newpage()
  pushViewport(viewport(width = 0.96, height = 0.96))

  # ---- Title ----
  grid.text("Empirical Approach",
            x = 0.5, y = 0.97,
            gp = gpar(fontsize = 14, fontface = "bold"))

  # ---- TOP ROW: Starting point ----
  # Both data sources box
  grid.roundrect(x = 0.5, y = 0.88, width = 0.55, height = 0.065,
                 gp = gpar(fill = "#D5F5E3", col = "#1E8449", lwd = 2),
                 r = unit(4, "mm"))
  grid.text("Step 1: Comparable Measures from Both Sources",
            x = 0.5, y = 0.885,
            gp = gpar(fontsize = 11, fontface = "bold", col = "#1E8449"))
  grid.text("County-level migration rates (AGI, returns, exemptions) for IRS & ACS",
            x = 0.5, y = 0.865,
            gp = gpar(fontsize = 8.5))

  # Arrow down to SDID
  grid.lines(x = c(0.5, 0.5), y = c(0.845, 0.81),
             gp = gpar(lwd = 2),
             arrow = arrow(length = unit(2.5, "mm"), type = "closed"))

  # ---- SDID BOX ----
  grid.roundrect(x = 0.5, y = 0.765, width = 0.70, height = 0.08,
                 gp = gpar(fill = "#EBF5FB", col = "#2471A3", lwd = 2),
                 r = unit(4, "mm"))
  grid.text("Synthetic Difference-in-Differences (SDID)",
            x = 0.5, y = 0.78,
            gp = gpar(fontsize = 11, fontface = "bold", col = "#2471A3"))
  grid.text(paste0("One treated unit (Multnomah) | Donor pools: All, Urban (top 5%), Urban-Covid | ",
                   "With/without covariates | Excl. 2020"),
            x = 0.5, y = 0.755,
            gp = gpar(fontsize = 7.5))

  # SDID outputs
  sdid_out_y <- 0.685
  for (i in 1:3) {
    ox <- 0.2 + (i - 1) * 0.3
    labs <- c("Specification Curves\n(Figure 4)",
              "Event Studies\n(Figure 5)",
              "Results: Tables")
    grid.roundrect(x = ox, y = sdid_out_y, width = 0.22, height = 0.05,
                   gp = gpar(fill = "#F8F9F9", col = "#2471A3", lwd = 1),
                   r = unit(2, "mm"))
    grid.text(labs[i], x = ox, y = sdid_out_y,
              gp = gpar(fontsize = 7.5, lineheight = 0.85))
    grid.lines(x = c(ox, ox), y = c(0.725, 0.71),
               gp = gpar(lwd = 1, col = "#2471A3"),
               arrow = arrow(length = unit(1.5, "mm"), type = "closed"))
  }

  # ---- Divider line ----
  grid.lines(x = c(0.05, 0.95), y = c(0.645, 0.645),
             gp = gpar(col = "grey60", lwd = 1.5, lty = 2))
  grid.text("Step 2: Source-Specific Analyses",
            x = 0.5, y = 0.635,
            gp = gpar(fontsize = 10, fontface = "bold.italic", col = "grey40"))

  # ---- LEFT BRANCH: IRS Flows ----
  irs_col <- "#2C3E50"
  irs_fill <- "#D6EAF8"

  # IRS header
  grid.roundrect(x = 0.25, y = 0.585, width = 0.42, height = 0.055,
                 gp = gpar(fill = irs_fill, col = irs_col, lwd = 2),
                 r = unit(4, "mm"))
  grid.text("IRS County-to-County Flows",
            x = 0.25, y = 0.59,
            gp = gpar(fontsize = 11, fontface = "bold", col = irs_col))
  grid.text("Geographic patterns: where are people moving?",
            x = 0.25, y = 0.57,
            gp = gpar(fontsize = 8, fontface = "italic"))

  # IRS Descriptive
  grid.roundrect(x = 0.13, y = 0.49, width = 0.2, height = 0.065,
                 gp = gpar(fill = "#F2F4F4", col = irs_col, lwd = 1),
                 r = unit(3, "mm"))
  grid.text("Descriptive Maps", x = 0.13, y = 0.50,
            gp = gpar(fontsize = 9, fontface = "bold"))
  grid.text("Change in AGI flows\nto/from Multnomah\n(Figures 6a, 6b)",
            x = 0.13, y = 0.475,
            gp = gpar(fontsize = 7, lineheight = 0.85))

  # IRS PPML
  grid.roundrect(x = 0.37, y = 0.49, width = 0.2, height = 0.065,
                 gp = gpar(fill = "#F2F4F4", col = irs_col, lwd = 1),
                 r = unit(3, "mm"))
  grid.text("PPML Flow Models", x = 0.37, y = 0.50,
            gp = gpar(fontsize = 9, fontface = "bold"))
  grid.text("Mijt with flow FE,\ntime-varying controls\n(Equation 3)",
            x = 0.37, y = 0.475,
            gp = gpar(fontsize = 7, lineheight = 0.85))

  # Arrows from IRS header
  grid.lines(x = c(0.18, 0.13), y = c(0.555, 0.525),
             gp = gpar(lwd = 1.5, col = irs_col),
             arrow = arrow(length = unit(2, "mm"), type = "closed"))
  grid.lines(x = c(0.32, 0.37), y = c(0.555, 0.525),
             gp = gpar(lwd = 1.5, col = irs_col),
             arrow = arrow(length = unit(2, "mm"), type = "closed"))

  # IRS PPML outputs
  ppml_out_y <- 0.40
  ppml_labs <- c("Event Studies\n(Figure 7)", "Placebo Tests\n(Figure 8)")
  for (i in 1:2) {
    ox <- 0.28 + (i - 1) * 0.18
    grid.roundrect(x = ox, y = ppml_out_y, width = 0.16, height = 0.045,
                   gp = gpar(fill = "white", col = irs_col, lwd = 1),
                   r = unit(2, "mm"))
    grid.text(ppml_labs[i], x = ox, y = ppml_out_y,
              gp = gpar(fontsize = 7, lineheight = 0.85))
  }
  grid.lines(x = c(0.37, 0.33), y = c(0.455, 0.425),
             gp = gpar(lwd = 1, col = irs_col),
             arrow = arrow(length = unit(1.5, "mm"), type = "closed"))
  grid.lines(x = c(0.37, 0.41), y = c(0.455, 0.425),
             gp = gpar(lwd = 1, col = irs_col),
             arrow = arrow(length = unit(1.5, "mm"), type = "closed"))

  # ---- RIGHT BRANCH: ACS Individual ----
  acs_col <- "#922B21"
  acs_fill <- "#FADBD8"

  # ACS header
  grid.roundrect(x = 0.75, y = 0.585, width = 0.42, height = 0.055,
                 gp = gpar(fill = acs_fill, col = acs_col, lwd = 2),
                 r = unit(4, "mm"))
  grid.text("ACS Individual-Level Data",
            x = 0.75, y = 0.59,
            gp = gpar(fontsize = 11, fontface = "bold", col = acs_col))
  grid.text("Who is moving? Conditional on characteristics",
            x = 0.75, y = 0.57,
            gp = gpar(fontsize = 8, fontface = "italic"))

  # ACS Descriptive
  grid.roundrect(x = 0.63, y = 0.49, width = 0.2, height = 0.065,
                 gp = gpar(fill = "#FDF2F0", col = acs_col, lwd = 1),
                 r = unit(3, "mm"))
  grid.text("Conditional Means", x = 0.63, y = 0.50,
            gp = gpar(fontsize = 9, fontface = "bold"))
  grid.text("Migration rates by\nincome, education, age\n(Figure 9)",
            x = 0.63, y = 0.475,
            gp = gpar(fontsize = 7, lineheight = 0.85))

  # ACS DiD
  grid.roundrect(x = 0.87, y = 0.49, width = 0.2, height = 0.065,
                 gp = gpar(fill = "#FDF2F0", col = acs_col, lwd = 1),
                 r = unit(3, "mm"))
  grid.text("DiD Models", x = 0.87, y = 0.50,
            gp = gpar(fontsize = 9, fontface = "bold"))
  grid.text("College educ. as proxy\nfor treatment (Eq. 5)\nSamples 1, 2, 3",
            x = 0.87, y = 0.475,
            gp = gpar(fontsize = 7, lineheight = 0.85))

  # Arrows from ACS header
  grid.lines(x = c(0.68, 0.63), y = c(0.555, 0.525),
             gp = gpar(lwd = 1.5, col = acs_col),
             arrow = arrow(length = unit(2, "mm"), type = "closed"))
  grid.lines(x = c(0.82, 0.87), y = c(0.555, 0.525),
             gp = gpar(lwd = 1.5, col = acs_col),
             arrow = arrow(length = unit(2, "mm"), type = "closed"))

  # ACS DiD outputs
  did_out_y <- 0.40
  did_labs <- c("Out-Migration\n(Figure 10)", "In-Migration\n(Figure 11)")
  for (i in 1:2) {
    ox <- 0.78 + (i - 1) * 0.18
    grid.roundrect(x = ox, y = did_out_y, width = 0.16, height = 0.045,
                   gp = gpar(fill = "white", col = acs_col, lwd = 1),
                   r = unit(2, "mm"))
    grid.text(did_labs[i], x = ox, y = did_out_y,
              gp = gpar(fontsize = 7, lineheight = 0.85))
  }
  grid.lines(x = c(0.87, 0.83), y = c(0.455, 0.425),
             gp = gpar(lwd = 1, col = acs_col),
             arrow = arrow(length = unit(1.5, "mm"), type = "closed"))
  grid.lines(x = c(0.87, 0.91), y = c(0.455, 0.425),
             gp = gpar(lwd = 1, col = acs_col),
             arrow = arrow(length = unit(1.5, "mm"), type = "closed"))

  # ---- BOTTOM: Combined conclusion ----
  grid.lines(x = c(0.05, 0.95), y = c(0.35, 0.35),
             gp = gpar(col = "grey60", lwd = 1.5, lty = 2))

  # Convergence arrows
  grid.lines(x = c(0.25, 0.40), y = c(0.355, 0.30),
             gp = gpar(lwd = 2, col = irs_col),
             arrow = arrow(length = unit(2.5, "mm"), type = "closed"))
  grid.lines(x = c(0.75, 0.60), y = c(0.355, 0.30),
             gp = gpar(lwd = 2, col = acs_col),
             arrow = arrow(length = unit(2.5, "mm"), type = "closed"))

  # Combined results box
  grid.roundrect(x = 0.5, y = 0.265, width = 0.55, height = 0.065,
                 gp = gpar(fill = "#F9EBEA", col = "#7B241C", lwd = 2),
                 r = unit(4, "mm"))
  grid.text("Step 3: Estimated Effects on Tax Revenues",
            x = 0.5, y = 0.275,
            gp = gpar(fontsize = 11, fontface = "bold", col = "#7B241C"))
  grid.text("Combine migration estimates with tax rate structure to quantify revenue impact",
            x = 0.5, y = 0.25,
            gp = gpar(fontsize = 8.5))

  # ---- Legend ----
  grid.text("Outcome Variables:",
            x = 0.08, y = 0.17,
            gp = gpar(fontsize = 8, fontface = "bold"), just = "left")

  outcomes <- c("In-migration rate", "Out-migration rate", "Net in-migration rate")
  for (i in 1:3) {
    grid.text(paste0("\u2022 ", outcomes[i]),
              x = 0.10, y = 0.17 - i * 0.025,
              gp = gpar(fontsize = 7.5), just = "left")
  }

  grid.text("Key Controls:",
            x = 0.35, y = 0.17,
            gp = gpar(fontsize = 8, fontface = "bold"), just = "left")

  controls <- c(
    "County & year fixed effects",
    "Per capita income, unemployment rate",
    "Median property tax (ACS counties)",
    "Covid-19 severity matching (donor pool)"
  )
  for (i in seq_along(controls)) {
    grid.text(paste0("\u2022 ", controls[i]),
              x = 0.37, y = 0.17 - i * 0.025,
              gp = gpar(fontsize = 7.5), just = "left")
  }

  grid.text("Donor Pool Restrictions:",
            x = 0.68, y = 0.17,
            gp = gpar(fontsize = 8, fontface = "bold"), just = "left")

  pools <- c(
    "Exclude AK, HI, CA, WA, OR",
    "All remaining counties",
    "Top 5% urban by population",
    "Urban + Covid-matched"
  )
  for (i in seq_along(pools)) {
    grid.text(paste0("\u2022 ", pools[i]),
              x = 0.70, y = 0.17 - i * 0.025,
              gp = gpar(fontsize = 7.5), just = "left")
  }

  popViewport()
  dev.off()
  message("  Created: ", outfile)
}


# =============================================================================
# GENERATE BOTH FIGURES
# =============================================================================

message("Generating Figure 2: Data comparison...")
make_fig_data_comparison(file.path(paper_figures, "fig_data_comparison.pdf"))

message("Generating Figure 3: Empirical approach...")
make_fig_empirical_approach(file.path(paper_figures, "fig_empirical_approach.pdf"))

message("Conceptual diagrams complete.")
