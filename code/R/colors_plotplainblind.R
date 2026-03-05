# =============================================================================
# colors_plotplainblind.R
# Shared plotplainblind (Wong 2011) colorblind-safe palette
#
# Provides:
#   - PPB_*: base hex values for the 9 plotplainblind colors
#   - ppb_tint() / ppb_shade(): lighten/darken any hex color
#   - Semantic color variables for consistent use across figures
#   - scale_fill_ppb_*() / scale_color_ppb_*(): ggplot2 scale functions
#
# Usage:
#   source("code/R/colors_plotplainblind.R")
# =============================================================================

# --- Base plotplainblind palette (Wong 2011) ---
PPB_BLACK       <- "#000000"   # p1
PPB_GREY        <- "#999999"   # p2 (gs10)
PPB_SKY         <- "#56B4E9"   # p3
PPB_TURQUOISE   <- "#009E73"   # p4
PPB_REDDISH     <- "#CC79A7"   # p5
PPB_VERMILLION  <- "#D55E00"   # p6
PPB_SEA         <- "#0072B2"   # p7
PPB_ORANGEBROWN <- "#E69F00"   # p8
PPB_ANANAS      <- "#F0E442"   # p9

# Full palette vector (named, in scheme order)
ppb_palette <- c(
  black       = PPB_BLACK,
  grey        = PPB_GREY,
  sky         = PPB_SKY,
  turquoise   = PPB_TURQUOISE,
  reddish     = PPB_REDDISH,
  vermillion  = PPB_VERMILLION,
  sea         = PPB_SEA,
  orangebrown = PPB_ORANGEBROWN,
  ananas      = PPB_ANANAS
)


# --- Tint/shade helpers ---

#' Lighten a hex color by mixing with white
#' @param hex Character hex color (e.g., "#0072B2")
#' @param amount Numeric 0-1; 0 = original, 1 = white
ppb_tint <- function(hex, amount = 0.3) {
  rgb_vals <- col2rgb(hex)[, 1]
  tinted <- rgb_vals + (255 - rgb_vals) * amount
  rgb(tinted[1], tinted[2], tinted[3], maxColorValue = 255)
}

#' Darken a hex color by mixing with black
#' @param hex Character hex color (e.g., "#0072B2")
#' @param amount Numeric 0-1; 0 = original, 1 = black
ppb_shade <- function(hex, amount = 0.3) {
  rgb_vals <- col2rgb(hex)[, 1]
  shaded <- rgb_vals * (1 - amount)
  rgb(shaded[1], shaded[2], shaded[3], maxColorValue = 255)
}


# --- Semantic color assignments ---

# Migration direction
col_out       <- PPB_SEA          # Out-migration / IRS data
col_in        <- PPB_VERMILLION   # In-migration / ACS data
col_mult      <- PPB_ORANGEBROWN  # Multnomah County highlight
col_oregon    <- PPB_SKY          # Oregon (secondary reference)
col_ref       <- PPB_GREY         # Reference/null lines

# Positive/negative indicators
col_positive  <- PPB_TURQUOISE    # + (pro)
col_negative  <- PPB_VERMILLION   # - (con)

# Specification curve colors
col_sig_notpref   <- PPB_SEA          # Significant, not preferred
col_insig_notpref <- PPB_SKY          # Insignificant, not preferred
col_sig_pref      <- PPB_VERMILLION   # Significant, preferred
col_insig_pref    <- PPB_ORANGEBROWN  # Insignificant, preferred
col_zero_line     <- PPB_REDDISH      # Zero reference line (spec curves)

# Map colors
col_portland      <- PPB_SEA          # Portland city
col_vancouver     <- PPB_TURQUOISE    # Vancouver city
col_metro_bdy     <- PPB_REDDISH      # Metro boundary
col_pool_out      <- PPB_TURQUOISE    # Pool: out-only
col_pool_in       <- PPB_SEA          # Pool: in-only
col_pool_both     <- PPB_ORANGEBROWN  # Pool: both
col_pool_mult     <- PPB_ANANAS       # Pool: Multnomah (distinct)
col_div_low       <- PPB_SEA          # Diverging gradient: low
col_div_high      <- PPB_VERMILLION   # Diverging gradient: high

# Diagram colors (tinted fills, semantic borders/text)
col_irs_border <- PPB_SEA
col_irs_fill   <- ppb_tint(PPB_SEA, 0.70)
col_irs_light  <- ppb_tint(PPB_SEA, 0.85)
col_irs_text   <- ppb_shade(PPB_SEA, 0.40)

col_acs_border <- PPB_VERMILLION
col_acs_fill   <- ppb_tint(PPB_VERMILLION, 0.70)
col_acs_light  <- ppb_tint(PPB_VERMILLION, 0.85)
col_acs_text   <- ppb_shade(PPB_VERMILLION, 0.40)

# Misc diagram colors
col_desc_text  <- "#666666"
col_dark_text  <- "#333333"
col_dash_line  <- PPB_GREY
col_arrow      <- "#555555"


# --- ggplot2 scale functions ---

#' Discrete fill scale using pool map colors
scale_fill_ppb_pool <- function(...) {
  ggplot2::scale_fill_manual(
    values = c(
      "Other"     = "gray95",
      "Out-only"  = col_pool_out,
      "In-only"   = col_pool_in,
      "Both"      = col_pool_both,
      "Multnomah" = col_pool_mult
    ),
    ...
  )
}

#' Diverging fill scale (blue-white-vermillion)
scale_fill_ppb_diverging <- function(name = "% Change", ...) {
  ggplot2::scale_fill_gradient2(
    low = col_div_low,
    mid = "white",
    high = col_div_high,
    midpoint = 0,
    na.value = "gray90",
    name = name,
    ...
  )
}
