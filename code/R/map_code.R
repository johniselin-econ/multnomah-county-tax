# ============================================================
# Multnomah Migration Maps + US County Pool Map (portable paths)
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(tigris)
  library(readxl)
  library(here)
  library(patchwork)
  library(cowplot)
})

options(tigris_use_cache = TRUE)

# --- Shared plotplainblind palette ---
source(here("code", "R", "colors_plotplainblind.R"))

home_base <- here()
results_dir <- file.path(home_base, "results")
data_dir    <- file.path(home_base, "data")
maps_dir    <- file.path(results_dir, "maps")

# Create maps directory if it doesn't exist
if (!dir.exists(maps_dir)) {
  dir.create(maps_dir, recursive = TRUE)
}

# ---- Layout constants --------------------------------------------------------
MAP_PAD         <- 50000   # padding around bounding boxes (meters, EPSG:3857/5070)
NUDGE_CITY_Y    <- 15000   # vertical nudge for city labels (meters)
NUDGE_CITY_DOWN <- -15000  # downward nudge for city labels below point
NUDGE_MULT_Y    <- -12000  # Multnomah label vertical nudge
NUDGE_MULT_X    <- 40000   # Multnomah label horizontal nudge
NUDGE_STATE_Y   <- -30000  # state label vertical nudge

# Inset layout (fraction of canvas)
INSET_MAIN_W    <- 0.62
INSET_X         <- 0.63
INSET_Y         <- 0.15
INSET_W         <- 0.36
INSET_H         <- 0.70
ZOOM_BOX_RIGHT  <- 0.27
ZOOM_BOX_TOP    <- 0.595
ZOOM_BOX_BOTTOM <- 0.4

# Shared theme sizes (legend, line widths)
LEGEND_KEY_W    <- grid::unit(2, "cm")    # legend colour-bar width
LEGEND_KEY_H    <- grid::unit(0.3, "cm")  # legend colour-bar height
LW_STATE        <- 0.25                   # state-border linewidth
LW_COUNTY_FLOW  <- 0.05                   # county-border linewidth in flow maps

# ------------------------------------------------------------
# Relative file paths (edit only if your repo layout differs)
# ------------------------------------------------------------
filepath1   <- file.path(maps_dir, "map1.png")
filepath1_minimal <- file.path(maps_dir, "map1_minimal.png")
filepath1_full <- file.path(maps_dir, "map1_full.png")
filepath2   <- file.path(maps_dir, "map2.png")
filepath2_minimal <- file.path(maps_dir, "map2_minimal.png")
filepath2_full <- file.path(maps_dir, "map2_full.png")
filepath_combined <- file.path(maps_dir, "map_combined.png")
filepath_combined_minimal <- file.path(maps_dir, "map_combined_minimal.png")
filepath_combined_full <- file.path(maps_dir, "map_combined_full.png")
filepath_us <- file.path(maps_dir, "map_us_pool.png")

metro_path  <- file.path(data_dir, "mapping", "Metro_District_Boundary", "Metro_District_Boundary.shp")
county_path <- file.path(data_dir, "working", "acs_county_sample.xlsx")

# ------------------------------------------------------------
# 1. LOAD COUNTIES (OR + WA) + County Pool
# ------------------------------------------------------------
or_counties <- counties("OR", cb = TRUE, year = 2023) |> st_transform(3857)
wa_counties <- counties("WA", cb = TRUE, year = 2023) |> st_transform(3857)

multnomah <- or_counties |> filter(NAME == "Multnomah")

pool <- readxl::read_excel(county_path)

# ------------------------------------------------------------
# 2. LOAD SELECTED CITIES
# ------------------------------------------------------------
or_places <- places("OR", cb = TRUE, year = 2023) |> st_transform(3857)
wa_places <- places("WA", cb = TRUE, year = 2023) |> st_transform(3857)

selected_or <- c("Portland","Eugene","Salem","Bend")
selected_wa <- c("Vancouver","Seattle","Tacoma","Spokane")

major_cities <- bind_rows(
  or_places |> filter(NAME %in% selected_or),
  wa_places |> filter(NAME %in% selected_wa)
)

city_points <- major_cities |> st_centroid()

# ------------------------------------------------------------
# 3. STATE POLYGONS AND LABEL POINTS
# ------------------------------------------------------------
or_state <- st_union(or_counties)
wa_state <- st_union(wa_counties)

or_centroid <- st_centroid(or_state)
wa_centroid <- st_centroid(wa_state)

map_bg_theme <- function(border = FALSE, border_color = PPB_VERMILLION) {
  base <- theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  if (border) {
    base <- base + theme(
      panel.border = element_rect(color = border_color, fill = NA, linewidth = 1.5)
    )
  }

  base
}

save_map <- function(plot_obj, path, width, height) {
  ggsave(path, plot_obj, width = width, height = height, dpi = 300, bg = "white")
  message("Saved: ", path)
}

build_overview_map <- function(label_mode = c("balanced", "minimal", "full"), show_zoom = FALSE) {
  label_mode <- match.arg(label_mode)

  p <- ggplot() +
    geom_sf(data = or_counties, fill = "gray92", color = "white", size = 0.3) +
    geom_sf(data = wa_counties, fill = "gray88", color = "white", size = 0.3) +
    geom_sf(data = multnomah, fill = col_mult, color = "black", size = 0.5)

  if (show_zoom) {
    p <- p +
      geom_sf(data = square_box, fill = NA, color = PPB_VERMILLION, linewidth = 1.2)
  }

  if (label_mode == "full") {
    p <- p +
      geom_sf_text(
        data = or_counties |> filter(NAME == "Multnomah"),
        aes(label = NAME),
        nudge_y = NUDGE_MULT_Y,
        nudge_x = NUDGE_MULT_X,
        size = 2.4,
        fontface = "bold"
      ) +
      geom_sf_text(data = or_counties |> filter(NAME != "Multnomah"), aes(label = NAME), size = 2.4) +
      geom_sf_text(data = wa_counties, aes(label = NAME), size = 2.4)
  } else {
    p <- p +
      geom_sf_text(
        data = or_counties |> filter(NAME == "Multnomah"),
        aes(label = NAME),
        nudge_y = NUDGE_MULT_Y,
        nudge_x = NUDGE_MULT_X,
        size = 2.6,
        fontface = "bold"
      )
  }

  if (label_mode != "minimal") {
    p <- p +
      geom_sf_text(
        data = or_centroid, aes(label = "OREGON"),
        nudge_y = NUDGE_STATE_Y,
        size = 6, fontface = "bold"
      ) +
      geom_sf_text(
        data = wa_centroid, aes(label = "WASHINGTON"),
        size = 6, fontface = "bold"
      )
  }

  if (label_mode == "minimal") {
    key_cities <- city_points |> filter(NAME %in% c("Portland", "Vancouver"))
    p <- p +
      geom_sf(data = key_cities, color = PPB_VERMILLION, size = 2.2) +
      geom_sf_text(
        data = key_cities |> filter(NAME == "Vancouver"),
        aes(label = NAME),
        nudge_y = NUDGE_CITY_Y,
        size = 2.6,
        fontface = "bold"
      ) +
      geom_sf_text(
        data = key_cities |> filter(NAME != "Vancouver"),
        aes(label = NAME),
        nudge_y = NUDGE_CITY_DOWN,
        size = 2.6,
        fontface = "bold"
      )
  } else {
    p <- p +
      geom_sf(data = city_points, color = PPB_VERMILLION, size = 2) +
      geom_sf_text(
        data = city_points |> filter(NAME == "Vancouver"),
        aes(label = NAME),
        nudge_y = NUDGE_CITY_Y,
        size = 2.4,
        fontface = "bold"
      ) +
      geom_sf_text(
        data = city_points |> filter(NAME != "Vancouver"),
        aes(label = NAME),
        nudge_y = NUDGE_CITY_DOWN,
        size = 2.4,
        fontface = "bold"
      )
  }

  p + map_bg_theme() + coord_sf(expand = FALSE)
}

# ============================================================
# MAP 1 — Oregon + Washington variants
# ============================================================
map1 <- build_overview_map("balanced")
map1_minimal <- build_overview_map("minimal")
map1_full <- build_overview_map("full")

save_map(map1, filepath1, width = 10, height = 8)
save_map(map1_minimal, filepath1_minimal, width = 10, height = 8)
save_map(map1_full, filepath1_full, width = 10, height = 8)

# ------------------------------------------------------------
# 4. CREATE MULTNOMAH REGION CLOSE-UP BOUNDING BOX
# ------------------------------------------------------------
bb  <- st_bbox(multnomah)

xspan <- (bb["xmax"] - bb["xmin"]) + 2 * MAP_PAD
yspan <- (bb["ymax"] - bb["ymin"]) + 2 * MAP_PAD
side  <- max(xspan, yspan) / 2

cx <- (bb["xmin"] + bb["xmax"]) / 2
cy <- (bb["ymin"] + bb["ymax"]) / 2

square_poly <- st_polygon(list(rbind(
  c(cx - side, cy - side),
  c(cx + side, cy - side),
  c(cx + side, cy + side),
  c(cx - side, cy + side),
  c(cx - side, cy - side)
)))

square_box <- st_sfc(square_poly, crs = st_crs(multnomah))

# crop data
or_cty_reg <- st_intersection(or_counties, square_box)
wa_cty_reg <- st_intersection(wa_counties, square_box)

portland  <- or_places |> filter(NAME == "Portland")
port_reg  <- st_intersection(portland, square_box)

vancouver <- wa_places |> filter(NAME == "Vancouver")
van_reg   <- st_intersection(vancouver, square_box)

selected_counties <- c(
  "Yamhill","Columbia","Washington","Marion","Polk","Clackamas",
  "Clark","Hood River","Wasco","Cowlitz","Skamania","Klickitat","Yakima"
)

# county labels
county_centroids <- bind_rows(or_cty_reg, wa_cty_reg) |>
  filter(NAME %in% selected_counties) |>
  st_centroid()

multnomah_centroid <- bind_rows(or_cty_reg, wa_cty_reg) |>
  filter(NAME == "Multnomah") |>
  st_centroid()

# City Labels
portland_centroid <- st_centroid(port_reg)

# --------------------------------------------------------
# 5. LOAD METRO CORPORATE BOUNDARY SHAPEFILE
# --------------------------------------------------------
metro <- st_read(metro_path) |> st_transform(3857)
metro_outline <- metro |>
  st_make_valid() |>
  st_union() |>
  st_intersection(square_box) |>
  st_boundary()


build_closeup_map <- function(label_mode = c("balanced", "minimal", "full"), inset = FALSE) {
  label_mode <- match.arg(label_mode)

  p <- ggplot() +
    geom_sf(data = or_cty_reg, fill = "gray90", color = "white", size = 0.4) +
    geom_sf(data = wa_cty_reg, fill = "gray85", color = "white", size = 0.4) +
    geom_sf(data = multnomah, fill = col_mult, color = "black", size = 0.6) +
    geom_sf(data = port_reg, color = col_portland, fill = col_portland, alpha = if (inset) 0.4 else 1) +
    geom_sf(data = van_reg, color = col_vancouver, fill = col_vancouver, alpha = if (inset) 0.4 else 1) +
    geom_sf(data = metro_outline, color = col_metro_bdy, linetype = "dashed", linewidth = if (inset) 0.7 else 0.9)

  if (label_mode == "full") {
    p <- p +
      geom_sf_text(data = county_centroids, aes(label = NAME), size = if (inset) 3.5 else 3)
  }

  p <- p +
    geom_sf_text(
      data = multnomah_centroid, aes(label = NAME),
      nudge_x = 10000, nudge_y = -5000,
      size = if (inset) 4.5 else 4,
      fontface = "bold"
    ) +
    geom_sf_text(
      data = portland_centroid, aes(label = NAME),
      nudge_x = 2000, nudge_y = -1000,
      size = if (inset) 3.5 else 3,
      fontface = "bold"
    ) +
    geom_sf_text(
      data = st_centroid(van_reg), aes(label = "Vancouver"),
      size = if (inset) 3.5 else 3,
      fontface = "bold"
    ) +
    geom_sf_text(
      data = st_centroid(metro_outline), aes(label = "METRO"),
      color = col_metro_bdy,
      size = if (inset) 3.5 else 3,
      fontface = "bold"
    )

  if (label_mode == "balanced") {
    p <- p +
      geom_sf_text(data = county_centroids |> filter(NAME %in% c("Clark", "Washington", "Clackamas")),
                   aes(label = NAME), size = if (inset) 3.2 else 2.8)
  }

  p + map_bg_theme(border = inset) + coord_sf(expand = FALSE)
}

# ============================================================
# MAP 2 — CLOSE-UP VARIANTS
# ============================================================
map2 <- build_closeup_map("balanced")
map2_minimal <- build_closeup_map("minimal")
map2_full <- build_closeup_map("full")

save_map(map2, filepath2, width = 10, height = 8)
save_map(map2_minimal, filepath2_minimal, width = 10, height = 8)
save_map(map2_full, filepath2_full, width = 10, height = 8)

# ============================================================
# COMBINED MAP — Overview with inset close-up
# ============================================================

map1_with_box <- build_overview_map("balanced", show_zoom = TRUE)
map1_with_box_minimal <- build_overview_map("minimal", show_zoom = TRUE)
map1_with_box_full <- build_overview_map("full", show_zoom = TRUE)

map2_inset <- build_closeup_map("balanced", inset = TRUE)
map2_inset_minimal <- build_closeup_map("minimal", inset = TRUE)
map2_inset_full <- build_closeup_map("full", inset = TRUE)

# Combine using cowplot - main map with inset positioned to the side (no overlap)
# Main map takes left portion, inset on right
# (layout constants defined in the constants block near the top of the file)

map_combined <- ggdraw() +
  draw_plot(map1_with_box, x = 0, y = 0, width = INSET_MAIN_W, height = 1) +
  draw_plot(map2_inset, x = INSET_X, y = INSET_Y, width = INSET_W, height = INSET_H) +
  # Connector lines: from zoom box corners to inset corners (dashed)
  # Top-right corner of zoom box to top-left corner of inset
  draw_line(
    x = c(ZOOM_BOX_RIGHT, INSET_X),
    y = c(ZOOM_BOX_TOP, INSET_Y + INSET_H - 0.07),
    color = PPB_VERMILLION, size = 0.6, linetype = "dashed"
  ) +
  # Bottom-right corner of zoom box to bottom-left corner of inset
  draw_line(
    x = c(ZOOM_BOX_RIGHT, INSET_X),
    y = c(ZOOM_BOX_BOTTOM, INSET_Y + 0.07),
    color = PPB_VERMILLION, size = 0.6, linetype = "dashed"
  )

map_combined_minimal <- ggdraw() +
  draw_plot(map1_with_box_minimal, x = 0, y = 0, width = INSET_MAIN_W, height = 1) +
  draw_plot(map2_inset_minimal, x = INSET_X, y = INSET_Y, width = INSET_W, height = INSET_H) +
  draw_line(
    x = c(ZOOM_BOX_RIGHT, INSET_X),
    y = c(ZOOM_BOX_TOP, INSET_Y + INSET_H - 0.07),
    color = PPB_VERMILLION, size = 0.6, linetype = "dashed"
  ) +
  draw_line(
    x = c(ZOOM_BOX_RIGHT, INSET_X),
    y = c(ZOOM_BOX_BOTTOM, INSET_Y + 0.07),
    color = PPB_VERMILLION, size = 0.6, linetype = "dashed"
  )

map_combined_full <- ggdraw() +
  draw_plot(map1_with_box_full, x = 0, y = 0, width = INSET_MAIN_W, height = 1) +
  draw_plot(map2_inset_full, x = INSET_X, y = INSET_Y, width = INSET_W, height = INSET_H) +
  draw_line(
    x = c(ZOOM_BOX_RIGHT, INSET_X),
    y = c(ZOOM_BOX_TOP, INSET_Y + INSET_H - 0.07),
    color = PPB_VERMILLION, size = 0.6, linetype = "dashed"
  ) +
  draw_line(
    x = c(ZOOM_BOX_RIGHT, INSET_X),
    y = c(ZOOM_BOX_BOTTOM, INSET_Y + 0.07),
    color = PPB_VERMILLION, size = 0.6, linetype = "dashed"
  )

save_map(map_combined, filepath_combined, width = 16, height = 10)
save_map(map_combined_minimal, filepath_combined_minimal, width = 16, height = 10)
save_map(map_combined_full, filepath_combined_full, width = 16, height = 10)

# ============================================================
# MAP 3 — Contiguous US counties by in/out sample status + state borders
# ============================================================

# Ensure FIPS + indicators are clean and joinable
pool2 <- pool %>%
  transmute(
    fips_chr = stringr::str_pad(as.character(as.integer(fips)), width = 5, pad = "0"),
    sample_migrants_in  = as.integer(sample_migrants_in),
    sample_migrants_out = as.integer(sample_migrants_out)
  ) %>%
  distinct(fips_chr, .keep_all = TRUE)

# Load boundaries
us_counties <- counties(cb = TRUE, year = 2023)
us_states   <- states(cb = TRUE, year = 2023)

# Drop territories + Alaska + Hawaii (keeps contiguous US + DC)
drop_stusps <- c("AK", "HI", "PR", "VI", "GU", "MP", "AS")
us_counties <- us_counties %>% dplyr::filter(!STUSPS %in% drop_stusps)
us_states   <- us_states   %>% dplyr::filter(!STUSPS %in% drop_stusps)

# Project to US Albers Equal Area
us_counties <- st_transform(us_counties, 5070)
us_states   <- st_transform(us_states, 5070)

# Join pool labels to counties and create 3-group classification
us_counties_plot <- us_counties %>%
  left_join(pool2, by = c("GEOID" = "fips_chr")) %>%
  mutate(
    group = case_when(
      GEOID == "41051" ~ "Multnomah",
      sample_migrants_in == 1 & sample_migrants_out == 1 ~ "Both",
      sample_migrants_in == 1 & sample_migrants_out == 0 ~ "In-only",
      sample_migrants_in == 0 & sample_migrants_out == 1 ~ "Out-only",
      TRUE ~ "Other"
    ),
    group = factor(group, levels = c("Other", "Out-only", "In-only", "Both", "Multnomah"))
  )

# Multnomah outline (keeps your earlier emphasis without adding a new fill group)
multnomah_us <- us_counties_plot %>% dplyr::filter(GEOID == "41051")

# Plot 
map_us_pool <- ggplot() +
  geom_sf(data = us_counties_plot, aes(fill = group), color = NA) +
  geom_sf(data = us_states, fill = NA, color = "gray25", linewidth = LW_STATE) +
  geom_sf(data = multnomah_us, fill = NA, color = PPB_VERMILLION, linewidth = 0.8) +
  scale_fill_ppb_pool(name = NULL) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))


save_map(map_us_pool, filepath_us, width = 12, height = 7)


# ============================================================
# MAP 4 — MIGRATION FLOW DELTA MAPS
# Pre-Post Comparison (2018-2019 vs 2021-2022)
# ============================================================

# ------------------------------------------------------------
# FILEPATHS FOR FLOW MAPS
# ------------------------------------------------------------
flows_data_dir <- file.path(data_dir, "working")  # Flow data now in working directory
flows_output_dir <- file.path(maps_dir)           # Save flow maps to maps folder

# ------------------------------------------------------------
# LOAD US COUNTIES (excluding AK and HI) - reuse us_counties from above
# Already loaded and projected to 5070 (Albers Equal Area)
# ------------------------------------------------------------

# Create numeric FIPS code for joining
us_counties_flow <- us_counties |>
 mutate(fips = as.numeric(GEOID))

# ------------------------------------------------------------
# DEFINE WEST COAST STATES
# ------------------------------------------------------------
west_coast_states <- c("06", "41", "53")
west_coast_counties <- us_counties_flow |>
 filter(STATEFP %in% west_coast_states)

orwa_states <- c("41", "53")
orwa_counties <- us_counties_flow |>
 filter(STATEFP %in% orwa_states)

# ------------------------------------------------------------
# FUNCTION TO CREATE FLOW DELTA MAP
# ------------------------------------------------------------
create_flow_map <- function(data, counties_sf, direction, measure, region_name,
                           legend_title = "% Change", coord_limits = NULL) {

 # Select appropriate columns based on direction
 if (direction == "out") {
   data <- data |>
     mutate(
       pre = out_pre,
       post = out_post
     )
 } else if (direction == "in") {
   data <- data |>
     mutate(
       pre = in_pre,
       post = in_post
     )
 } else if (direction == "net") {
   data <- data |>
     mutate(
       pre = in_pre - out_pre,
       post = in_post - out_post
     )
 }

 # Calculate percent change (delta / pre)
 data <- data |>
   mutate(
     delta = post - pre,
     pct_change = ifelse(pre != 0, 100 * delta / pre, NA_real_)
   )

 # Join with county geometries
 map_data <- counties_sf |>
   left_join(data, by = "fips")

 # Cap extreme values for better visualization
 map_data <- map_data |>
   mutate(
     pct_change_capped = case_when(
       pct_change > 200 ~ 200,
       pct_change < -100 ~ -100,
       TRUE ~ pct_change
     )
   )

 # Extract Multnomah County for highlighting
 multnomah_highlight <- map_data |> filter(GEOID == "41051")

 # Create the map
 p <- ggplot(map_data) +
   geom_sf(aes(fill = pct_change_capped), color = "gray80", linewidth = LW_COUNTY_FLOW) +
   geom_sf(data = us_states, fill = NA, color = "gray25", linewidth = LW_STATE) +
   geom_sf(data = multnomah_highlight, fill = col_mult, color = "black", linewidth = 0.5) +
   scale_fill_gradient2(
     low = col_div_low,
     mid = "white",
     high = col_div_high,
     midpoint = 0,
     na.value = "gray90",
     name = legend_title,
     limits = c(-100, 200),
     breaks = c(-100, -50, 0, 50, 100, 150, 200),
     labels = c("-100%", "-50%", "0%", "+50%", "+100%", "+150%", "+200%")
   ) +
   theme_void() +
   theme(
     legend.position = "bottom",
     legend.key.width = LEGEND_KEY_W,
     legend.key.height = LEGEND_KEY_H,
     legend.text = element_text(color = "black"),
     legend.title = element_text(color = "black"),
     plot.background = element_rect(fill = "white", color = NA),
     panel.background = element_rect(fill = "white", color = NA)
   ) +
   guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

 # Apply coordinate limits if specified (for cropping to region)
 if (!is.null(coord_limits)) {
   p <- p + coord_sf(
     xlim = coord_limits$xlim,
     ylim = coord_limits$ylim,
     expand = FALSE
   )
 }

 return(p)
}

# ------------------------------------------------------------
# FUNCTION TO CREATE RATE CHANGE MAP (using pre-calculated differences)
# ------------------------------------------------------------
create_rate_change_map <- function(data, counties_sf, direction, measure, region_name,
                                   legend_title = "Rate Change", coord_limits = NULL) {

 # Select appropriate rate_change column based on direction
 if (direction == "out") {
   data <- data |>
     mutate(rate_change = out_rate_change)
 } else if (direction == "in") {
   data <- data |>
     mutate(rate_change = in_rate_change)
 } else if (direction == "net") {
   data <- data |>
     mutate(rate_change = net_rate_change)
 }

 # Join with county geometries
 map_data <- counties_sf |>
   left_join(data, by = "fips")

 # Determine scale limits based on data range
 rate_range <- range(map_data$rate_change, na.rm = TRUE)
 max_abs <- max(abs(rate_range), na.rm = TRUE)
 # Round up to nice number for symmetric limits
 limit_val <- ceiling(max_abs * 10) / 10
 if (limit_val < 0.1) limit_val <- 0.1

 # Cap extreme values for better visualization
 map_data <- map_data |>
   mutate(
     rate_change_capped = case_when(
       rate_change > limit_val ~ limit_val,
       rate_change < -limit_val ~ -limit_val,
       TRUE ~ rate_change
     )
   )

 # Extract Multnomah County for highlighting
 multnomah_highlight <- map_data |> filter(GEOID == "41051")

 # Create the map
 p <- ggplot(map_data) +
   geom_sf(aes(fill = rate_change_capped), color = "gray80", linewidth = LW_COUNTY_FLOW) +
   geom_sf(data = us_states, fill = NA, color = "gray25", linewidth = LW_STATE) +
   geom_sf(data = multnomah_highlight, fill = col_mult, color = "black", linewidth = 0.5) +
   scale_fill_gradient2(
     low = col_div_low,
     mid = "white",
     high = col_div_high,
     midpoint = 0,
     na.value = "gray90",
     name = legend_title
   ) +
   theme_void() +
   theme(
     legend.position = "bottom",
     legend.key.width = LEGEND_KEY_W,
     legend.key.height = LEGEND_KEY_H,
     legend.text = element_text(color = "black"),
     legend.title = element_text(color = "black"),
     plot.background = element_rect(fill = "white", color = NA),
     panel.background = element_rect(fill = "white", color = NA)
   ) +
   guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

 # Apply coordinate limits if specified (for cropping to region)
 if (!is.null(coord_limits)) {
   p <- p + coord_sf(
     xlim = coord_limits$xlim,
     ylim = coord_limits$ylim,
     expand = FALSE
   )
 }

 return(p)
}

# ------------------------------------------------------------
# GENERATE ALL FLOW DELTA MAPS
# ------------------------------------------------------------

# Define west coast bounding box in EPSG:5070 (Albers Equal Area)
# These coordinates crop to just CA, OR, WA
wc_bbox <- st_bbox(west_coast_counties)
wc_coord_limits <- list(
  xlim = c(wc_bbox["xmin"] - MAP_PAD, wc_bbox["xmax"] + MAP_PAD),
  ylim = c(wc_bbox["ymin"] - MAP_PAD, wc_bbox["ymax"] + MAP_PAD)
)

orwa_bbox <- st_bbox(orwa_counties)
orwa_coord_limits <- list(
  xlim = c(orwa_bbox["xmin"] - MAP_PAD, orwa_bbox["xmax"] + MAP_PAD),
  ylim = c(orwa_bbox["ymin"] - MAP_PAD, orwa_bbox["ymax"] + MAP_PAD)
)

# Loop over measures
for (measure in c("n1", "n2", "agi")) {

 # Load flow comparison data from working directory
 csv_path <- file.path(flows_data_dir, paste0("multnomah_flow_comparison_", measure, ".csv"))

 # Check if file exists
 if (!file.exists(csv_path)) {
   message(paste0("Warning: File not found - ", csv_path))
   next
 }

 flow_data <- read_csv(csv_path, show_col_types = FALSE)

 # Loop over directions
 for (direction in c("out", "in", "net")) {

   # ---- US Map (excluding AK, HI) ----
   us_map <- create_flow_map(
     data = flow_data,
     counties_sf = us_counties_flow,
     direction = direction,
     measure = measure,
     region_name = "Continental US"
   )

   # Save US map to maps folder
   us_filepath <- file.path(flows_output_dir, paste0("map_", measure, "_", direction, "_us.png"))
   ggsave(us_filepath, us_map, width = 14, height = 9, dpi = 300, bg = "white")
   message(paste0("Saved: ", us_filepath))

   # ---- West Coast Map (cropped to only show CA, OR, WA) ----
   wc_map <- create_flow_map(
     data = flow_data,
     counties_sf = west_coast_counties,
     direction = direction,
     measure = measure,
     region_name = "West Coast (CA, OR, WA)",
     coord_limits = wc_coord_limits
   )

   # Save West Coast map to maps folder
   wc_filepath <- file.path(flows_output_dir, paste0("map_", measure, "_", direction, "_westcoast.png"))
   ggsave(wc_filepath, wc_map, width = 8, height = 14, dpi = 300, bg = "white")
   message(paste0("Saved: ", wc_filepath))

 }
}

message("All flow delta maps created successfully!")

# ------------------------------------------------------------
# GENERATE RATE CHANGE MAPS (using pre-calculated differences)
# ------------------------------------------------------------

# Loop over measures
for (measure in c("n1", "n2", "agi")) {

 # Load flow comparison data from working directory
 csv_path <- file.path(flows_data_dir, paste0("multnomah_flow_comparison_", measure, ".csv"))

 # Check if file exists
 if (!file.exists(csv_path)) {
   message(paste0("Warning: File not found - ", csv_path))
   next
 }

 flow_data <- read_csv(csv_path, show_col_types = FALSE)

 # Check if rate_change columns exist
 if (!all(c("out_rate_change", "in_rate_change", "net_rate_change") %in% names(flow_data))) {
   message(paste0("Warning: Rate change columns not found in ", csv_path))
   next
 }

 # Loop over directions for rate change maps
 for (direction in c("out", "in", "net")) {

   # ---- US Map (excluding AK, HI) ----
   us_rate_map <- create_rate_change_map(
     data = flow_data,
     counties_sf = us_counties_flow,
     direction = direction,
     measure = measure,
     region_name = "Continental US"
   )

   # Save US rate change map
   us_rate_filepath <- file.path(flows_output_dir, paste0("map_", measure, "_", direction, "_rate_change_us.png"))
   ggsave(us_rate_filepath, us_rate_map, width = 14, height = 9, dpi = 300, bg = "white")
   message(paste0("Saved: ", us_rate_filepath))

   # ---- West Coast Map (cropped to only show CA, OR, WA) ----
   wc_rate_map <- create_rate_change_map(
     data = flow_data,
     counties_sf = west_coast_counties,
     direction = direction,
     measure = measure,
     region_name = "West Coast (CA, OR, WA)",
     coord_limits = wc_coord_limits
   )

   # Save West Coast rate change map
   wc_rate_filepath <- file.path(flows_output_dir, paste0("map_", measure, "_", direction, "_rate_change_westcoast.png"))
   ggsave(wc_rate_filepath, wc_rate_map, width = 8, height = 14, dpi = 300, bg = "white")
   message(paste0("Saved: ", wc_rate_filepath))

 }
}

message("All rate change maps created successfully!")

# ============================================================
# MAP 5 — DIRECTIONAL FLOW MAPS (Partner-Normalized)
# These maps show flows FROM/TO Multnomah normalized by partner
# county population, which better captures the "flow" aspect.
#
# Out-flow map: Rate of migration FROM Multnomah TO each county
#               per 100K of DESTINATION county population
#               Red = counties receiving MORE from Multnomah
#               Multnomah = hatched (source county)
#
# In-flow map:  Rate of migration TO Multnomah FROM each county
#               per 100K of ORIGIN county population
#               Blue = counties sending LESS to Multnomah
#               Multnomah = hatched (destination county)
# ============================================================

message("Creating directional flow maps (partner-normalized)...")

# ------------------------------------------------------------
# FUNCTION TO CREATE HATCHED PATTERN FOR MULTNOMAH
# ------------------------------------------------------------
create_hatch_pattern <- function(poly, n_lines = 10, angle = 45) {
  # Get bounding box
  bb <- st_bbox(poly)

  # Create diagonal lines
  x_range <- bb["xmax"] - bb["xmin"]
  y_range <- bb["ymax"] - bb["ymin"]
  max_range <- max(x_range, y_range) * 1.5

  # Generate line coordinates
  spacing <- max_range / n_lines
  lines_list <- list()

  for (i in seq(-n_lines, n_lines * 2)) {
    offset <- i * spacing
    x1 <- bb["xmin"] - max_range + offset
    y1 <- bb["ymin"] - max_range
    x2 <- bb["xmin"] + offset
    y2 <- bb["ymin"] + max_range

    line <- st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE))
    lines_list[[i + n_lines + 1]] <- line
  }

  # Combine lines and clip to polygon
  all_lines <- st_sfc(lines_list, crs = st_crs(poly))
  all_lines <- st_sf(geometry = all_lines)
  clipped <- st_intersection(all_lines, poly)

  return(clipped)
}

# ------------------------------------------------------------
# FUNCTION TO CREATE DIRECTIONAL FLOW MAP
# ------------------------------------------------------------
create_directional_flow_map <- function(data, counties_sf, direction, measure,
                                         region_name, coord_limits = NULL) {

  # Filter to counties with positive flows in BOTH pre and post periods
  # This ensures meaningful rate comparisons
  if (direction == "out") {
    # For out-migration: need positive out_pre AND out_post
    data_filtered <- data |>
      filter(out_pre > 0 & out_post > 0) |>
      mutate(rate_change = out_rate_change)
    # Positive = more people leaving Multnomah for this county
    low_color <- col_div_low
    high_color <- col_div_high
    n_counties_with_flows <- nrow(data_filtered)
    message(paste0("  ", direction, ": ", n_counties_with_flows,
                   " counties with positive flows in both periods"))
  } else if (direction == "in") {
    # For in-migration: need positive in_pre AND in_post
    data_filtered <- data |>
      filter(in_pre > 0 & in_post > 0) |>
      mutate(rate_change = in_rate_change)
    # Negative = fewer people coming to Multnomah
    low_color <- col_div_low
    high_color <- col_div_high
    n_counties_with_flows <- nrow(data_filtered)
    message(paste0("  ", direction, ": ", n_counties_with_flows,
                   " counties with positive flows in both periods"))
  }

  # Join with county geometries
  # Counties without flows in both periods will have NA (shown in gray)
  map_data <- counties_sf |>
    left_join(data_filtered, by = "fips")

  # Determine symmetric scale limits
  rate_range <- range(map_data$rate_change, na.rm = TRUE)
  max_abs <- max(abs(rate_range), na.rm = TRUE)
  # Round up to nice number
  limit_val <- ceiling(max_abs / 10) * 10
  if (limit_val < 10) limit_val <- ceiling(max_abs)
  if (limit_val < 1) limit_val <- 1

  # Cap extreme values
  map_data <- map_data |>
    mutate(
      rate_change_capped = case_when(
        rate_change > limit_val ~ limit_val,
        rate_change < -limit_val ~ -limit_val,
        TRUE ~ rate_change
      )
    )

  # Extract Multnomah County for hatching
  multnomah_poly <- map_data |> filter(GEOID == "41051")

  # Create hatching for Multnomah (if it exists in this region)
  if (nrow(multnomah_poly) > 0) {
    multnomah_hatch <- tryCatch(
      create_hatch_pattern(multnomah_poly, n_lines = 15, angle = 45),
      error = function(e) NULL
    )
  } else {
    multnomah_hatch <- NULL
  }

  # Create the map
  p <- ggplot(map_data) +
    geom_sf(aes(fill = rate_change_capped), color = "gray80", linewidth = LW_COUNTY_FLOW) +
    geom_sf(data = us_states, fill = NA, color = "gray25", linewidth = LW_STATE)

  # Add Multnomah with hatching
  if (nrow(multnomah_poly) > 0) {
    p <- p +
      geom_sf(data = multnomah_poly, fill = "gray95", color = "black", linewidth = 0.6)

    if (!is.null(multnomah_hatch) && nrow(multnomah_hatch) > 0) {
      p <- p + geom_sf(data = multnomah_hatch, color = "gray40", linewidth = 0.3)
    }
  }

  p <- p +
    scale_fill_gradient2(
      low = low_color,
      mid = "white",
      high = high_color,
      midpoint = 0,
      na.value = "gray90",
      name = "Rate change\n(per 100K)",
      limits = c(-limit_val, limit_val)
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.key.width = LEGEND_KEY_W,
      legend.key.height = LEGEND_KEY_H,
      legend.text = element_text(color = "black"),
      legend.title = element_text(color = "black"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

  # Apply coordinate limits if specified
  if (!is.null(coord_limits)) {
    p <- p + coord_sf(
      xlim = coord_limits$xlim,
      ylim = coord_limits$ylim,
      expand = FALSE
    )
  }

  return(p)
}

# ------------------------------------------------------------
# GENERATE DIRECTIONAL FLOW MAPS
# ------------------------------------------------------------

# Loop over measures
for (measure in c("n1", "n2", "agi")) {

  # Load partner-normalized flow data
  csv_path <- file.path(flows_data_dir, paste0("multnomah_partner_flows_", measure, ".csv"))

  # Check if file exists
  if (!file.exists(csv_path)) {
    message(paste0("Warning: Partner flow file not found - ", csv_path))
    message("Run 02_descriptives.do first to create partner-normalized flow data")
    next
  }

  flow_data <- read_csv(csv_path, show_col_types = FALSE)

  # Loop over directions (out and in only, not net)
  for (direction in c("out", "in")) {

    # ---- Continental US Map ----
    us_dir_map <- create_directional_flow_map(
      data = flow_data,
      counties_sf = us_counties_flow,
      direction = direction,
      measure = measure,
      region_name = "Continental US"
    )

    # Save US map
    us_filepath <- file.path(flows_output_dir,
                              paste0("map_directional_", measure, "_", direction, "_us.png"))
    ggsave(us_filepath, us_dir_map, width = 14, height = 9, dpi = 300, bg = "white")
    message(paste0("Saved: ", us_filepath))

    # ---- West Coast Map ----
    wc_dir_map <- create_directional_flow_map(
      data = flow_data,
      counties_sf = west_coast_counties,
      direction = direction,
      measure = measure,
      region_name = "West Coast (CA, OR, WA)",
      coord_limits = wc_coord_limits
    )

    # Save West Coast map
    wc_filepath <- file.path(flows_output_dir,
                              paste0("map_directional_", measure, "_", direction, "_westcoast.png"))
    ggsave(wc_filepath, wc_dir_map, width = 8, height = 14, dpi = 300, bg = "white")
    message(paste0("Saved: ", wc_filepath))

    # ---- Oregon + Washington Map ----
    orwa_dir_map <- create_directional_flow_map(
      data = flow_data,
      counties_sf = orwa_counties,
      direction = direction,
      measure = measure,
      region_name = "Oregon & Washington",
      coord_limits = orwa_coord_limits
    )

    # Save OR+WA map
    orwa_filepath <- file.path(flows_output_dir,
                                paste0("map_directional_", measure, "_", direction, "_orwa.png"))
    ggsave(orwa_filepath, orwa_dir_map, width = 8, height = 10, dpi = 300, bg = "white")
    message(paste0("Saved: ", orwa_filepath))

  }
}

message("All directional flow maps created successfully!")

# Overleaf copy — bulk copy all map PNGs to overleaf figures directory
if (exists("cfg") && isTRUE(cfg$overleaf) && nzchar(cfg$dir_ol_fig)) {
  map_files <- list.files(maps_dir, pattern = "\\.png$", full.names = TRUE)
  if (length(map_files) > 0) {
    file.copy(map_files, file.path(cfg$dir_ol_fig, basename(map_files)), overwrite = TRUE)
    message("Overleaf: copied ", length(map_files), " maps to ", cfg$dir_ol_fig)
  }
}
