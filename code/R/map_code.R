# ============================================================
# Multnomah Migration Maps + US County Pool Map (portable paths)
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(tidyverse)
  library(tigris)
  library(readxl)
  library(here)
  library(patchwork)
  library(cowplot)
})

options(tigris_use_cache = TRUE)


home_base <- here()
results_dir <- file.path(home_base, "results")
data_dir    <- file.path(home_base, "data")
maps_dir    <- file.path(results_dir, "maps")

# Create maps directory if it doesn't exist
if (!dir.exists(maps_dir)) {
  dir.create(maps_dir, recursive = TRUE)
}

# ------------------------------------------------------------
# Relative file paths (edit only if your repo layout differs)
# ------------------------------------------------------------
filepath1   <- file.path(maps_dir, "map1.png")
filepath2   <- file.path(maps_dir, "map2.png")
filepath_combined <- file.path(maps_dir, "map_combined.png")
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

# ============================================================
# MAP 1 — Oregon + Washington (counties + selected cities)
# ============================================================
map1 <- ggplot() +
  geom_sf(data = or_counties, fill = "gray92", color = "white", size = 0.3) +
  geom_sf(data = wa_counties, fill = "gray88", color = "white", size = 0.3) +
  geom_sf(data = multnomah, fill = "yellow", color = "black", size = 0.5) +
  
  # county labels
  geom_sf_text(
    data = or_counties |> filter(NAME == "Multnomah"),
    aes(label = NAME),
    nudge_y = -12000,
    nudge_x =  40000,
    size = 2.0,
    fontface = "bold"
  ) +
  geom_sf_text(data = or_counties |> filter(NAME != "Multnomah"), aes(label = NAME), size = 2.0) +
  geom_sf_text(data = wa_counties, aes(label = NAME), size = 2.0) +
  
  # state labels
  geom_sf_text(
    data = or_centroid, aes(label = "OREGON"),
    nudge_y = -30000,
    size = 6, fontface = "bold"
  ) +
  geom_sf_text(
    data = wa_centroid, aes(label = "WASHINGTON"),
    size = 6, fontface = "bold"
  ) +
  
  # city points
  geom_sf(data = city_points, color = "red", size = 2) +
  
  # city labels: Vancouver above; others below
  geom_sf_text(
    data = city_points |> filter(NAME == "Vancouver"),
    aes(label = NAME),
    nudge_y = 15000,
    size = 2.4,
    fontface = "bold"
  ) +
  geom_sf_text(
    data = city_points |> filter(NAME != "Vancouver"),
    aes(label = NAME),
    nudge_y = -15000,
    size = 2.4,
    fontface = "bold"
  ) +
  theme_void() +
  coord_sf(expand = FALSE)

ggsave(filepath1, map1, width = 10, height = 8, dpi = 300)

# ------------------------------------------------------------
# 4. CREATE MULTNOMAH REGION CLOSE-UP BOUNDING BOX
# ------------------------------------------------------------
bb  <- st_bbox(multnomah)
pad <- 50000

xspan <- (bb["xmax"] - bb["xmin"]) + 2 * pad
yspan <- (bb["ymax"] - bb["ymin"]) + 2 * pad
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


# ============================================================
# MAP 2 — CLOSE-UP WITH PORTLAND + VANCOUVER + METRO
# ============================================================
map2 <- ggplot() +
  geom_sf(data = or_cty_reg, fill = "gray90", color = "white", size = 0.4) +
  geom_sf(data = wa_cty_reg, fill = "gray85", color = "white", size = 0.4) +
  geom_sf(data = multnomah, fill = "yellow", color = "black", size = 0.6) +
  
  # Portland + Vancouver
  geom_sf(data = port_reg, color = "#3182BD", fill = "#3182BD", size = 4) +
  geom_sf(data = van_reg,  color = "#31A354", fill = "#31A354", size = 4) +
  
  # Metro Boundary (official)
  geom_sf(data = metro_outline, color = "#9E0168",
          linetype = "dashed", linewidth = 0.9) +
  
  # labels
  geom_sf_text(data = county_centroids, aes(label = NAME), size = 3) +
  geom_sf_text(
    data = multnomah_centroid, aes(label = NAME),
    nudge_x = 10000, nudge_y = -5000,
    size = 4, fontface = "bold"
  ) +
  geom_sf_text(
    data = portland_centroid, aes(label = NAME),
    nudge_x = 2000, nudge_y = -1000,
    size = 3, fontface = "bold"
  ) +
  geom_sf_text(
    data = st_centroid(van_reg), aes(label = "Vancouver"),
    size = 3, fontface = "bold"
  ) +
  geom_sf_text(
    data = st_centroid(metro_outline), aes(label = "METRO"),
    color = "#9E0168",
    size = 3, fontface = "bold"
  ) +
  theme_void() +
  coord_sf(expand = FALSE)

ggsave(filepath2, map2, width = 10, height = 8, dpi = 300)

# ============================================================
# COMBINED MAP — Overview with inset close-up
# ============================================================

# Use the square_box directly as the zoom rectangle (already an sfc object)
zoom_rect <- square_box

# Create nudged points for Bend and Spokane (move up ~1mm in print)
bend_spokane_pts <- city_points |> filter(NAME %in% c("Bend", "Spokane"))
bend_spokane_nudged <- bend_spokane_pts |>
  mutate(geometry = geometry + c(0, 8000)) |>  # Nudge up ~1mm

  st_set_crs(st_crs(city_points))
other_city_pts <- city_points |> filter(!NAME %in% c("Bend", "Spokane", "Vancouver"))
vancouver_pt <- city_points |> filter(NAME == "Vancouver")

# Modified Map 1 with zoom rectangle indicator
map1_with_box <- ggplot() +
  geom_sf(data = or_counties, fill = "gray92", color = "white", size = 0.3) +
  geom_sf(data = wa_counties, fill = "gray88", color = "white", size = 0.3) +
  geom_sf(data = multnomah, fill = "yellow", color = "black", size = 0.5) +

  # Zoom area rectangle
  geom_sf(data = zoom_rect, fill = NA, color = "red", linewidth = 1.2, linetype = "solid") +

  # County labels (restored)
  geom_sf_text(
    data = or_counties |> filter(NAME == "Multnomah"),
    aes(label = NAME),
    nudge_y = -12000,
    nudge_x =  40000,
    size = 2.4,
    fontface = "bold"
  ) +
  geom_sf_text(data = or_counties |> filter(NAME != "Multnomah"), aes(label = NAME), size = 2.4) +
  geom_sf_text(data = wa_counties, aes(label = NAME), size = 2.4) +

  # State labels
  geom_sf_text(
    data = or_centroid, aes(label = "OREGON"),
    nudge_y = -30000,
    size = 6, fontface = "bold"
  ) +
  geom_sf_text(
    data = wa_centroid, aes(label = "WASHINGTON"),
    size = 6, fontface = "bold"
  ) +

  # City points (Bend & Spokane nudged up, others at original position)
  geom_sf(data = other_city_pts, color = "red", size = 2) +
  geom_sf(data = vancouver_pt, color = "red", size = 2) +
  geom_sf(data = bend_spokane_nudged, color = "red", size = 2) +

  # City labels: Vancouver, Bend, Spokane above; others below
  geom_sf_text(
    data = vancouver_pt,
    aes(label = NAME),
    nudge_y = 15000,
    size = 2.8, fontface = "bold"
  ) +
  geom_sf_text(
    data = bend_spokane_nudged,
    aes(label = NAME),
    nudge_y = 15000,
    size = 2.8, fontface = "bold"
  ) +
  geom_sf_text(
    data = other_city_pts,
    aes(label = NAME),
    nudge_y = -15000,
    size = 2.8, fontface = "bold"
  ) +
  theme_void() +
  coord_sf(expand = FALSE)

# Modified Map 2 (inset) - with all labels restored
map2_inset <- ggplot() +
  geom_sf(data = or_cty_reg, fill = "gray90", color = "white", size = 0.3) +
  geom_sf(data = wa_cty_reg, fill = "gray85", color = "white", size = 0.3) +
  geom_sf(data = multnomah, fill = "yellow", color = "black", size = 0.5) +

  # Portland + Vancouver
  geom_sf(data = port_reg, color = "#3182BD", fill = "#3182BD", alpha = 0.4) +
  geom_sf(data = van_reg,  color = "#31A354", fill = "#31A354", alpha = 0.4) +

  # Metro Boundary
  geom_sf(data = metro_outline, color = "#9E0168",
          linetype = "dashed", linewidth = 0.7) +

  # County labels
  geom_sf_text(data = county_centroids, aes(label = NAME), size = 3.5) +
  geom_sf_text(
    data = multnomah_centroid, aes(label = NAME),
    nudge_x = 10000, nudge_y = -5000,
    size = 4.5, fontface = "bold"
  ) +
  # Portland label
  geom_sf_text(
    data = portland_centroid, aes(label = NAME),
    nudge_x = 2000, nudge_y = -1000,
    size = 3.5, fontface = "bold"
  ) +
  # Vancouver label
  geom_sf_text(
    data = st_centroid(van_reg), aes(label = "Vancouver"),
    size = 3.5, fontface = "bold"
  ) +
  # Metro label
  geom_sf_text(
    data = st_centroid(metro_outline), aes(label = "METRO"),
    color = "#9E0168",
    size = 3.5, fontface = "bold"
  ) +
  theme_void() +
  theme(
    panel.border = element_rect(color = "red", fill = NA, linewidth = 1.5),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_sf(expand = FALSE)

# Combine using cowplot - main map with inset positioned to the side (no overlap)
# Main map takes left portion, inset on right
main_map_w <- 0.62   # Main map width (left 62%)
inset_x <- 0.63      # Inset starts at 63% from left
inset_y <- 0.15      # Centered vertically
inset_w <- 0.36      # Inset width (36% of canvas)
inset_h <- 0.70      # Inset height

# Zoom box approximate position on main map (in 0-1 coordinates)
# Adjusted for the narrower main map display
zoom_box_right <- 0.27
zoom_box_top <- 0.595
zoom_box_bottom <- 0.4

map_combined <- ggdraw() +
  draw_plot(map1_with_box, x = 0, y = 0, width = main_map_w, height = 1) +
  draw_plot(map2_inset, x = inset_x, y = inset_y, width = inset_w, height = inset_h) +
  # Connector lines: from zoom box corners to inset corners (dashed)
  # Top-right corner of zoom box to top-left corner of inset
  draw_line(
    x = c(zoom_box_right, inset_x),
    y = c(zoom_box_top, inset_y + inset_h - 0.07),
    color = "red", size = 0.6, linetype = "dashed"
  ) +
  # Bottom-right corner of zoom box to bottom-left corner of inset
  draw_line(
    x = c(zoom_box_right, inset_x),
    y = c(zoom_box_bottom, inset_y + 0.07),
    color = "red", size = 0.6, linetype = "dashed"
  )
ggsave(filepath_combined, map_combined, width = 16, height = 10, dpi = 300, bg = "white")
message("Saved: ", filepath_combined)

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
  geom_sf(data = us_states, fill = NA, color = "gray25", linewidth = 0.25) +
  geom_sf(data = multnomah_us, fill = NA, color = "red", linewidth = 0.8) +  # optional emphasis
  scale_fill_manual(
    values = c(
      "Other"     = "gray95",
      "Out-only"  = "#4C9F70",  # muted green
      "In-only"   = "#4C78A8",  # muted blue
      "Both"      = "#F2B701",  # muted amber
      "Multnomah" = "yellow"
    ),
    name = NULL
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))


ggsave(filepath_us, map_us_pool, width = 12, height = 7, dpi = 300, bg = "white")

message("Saved: ", filepath1)
message("Saved: ", filepath2)
message("Saved: ", filepath_us)


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

# ------------------------------------------------------------
# DEFINE MEASURE AND DIRECTION LABELS
# ------------------------------------------------------------
measure_labels <- list(
 n1 = "Returns",
 n2 = "Exemptions",
 agi = "AGI ($)"
)

direction_labels <- list(
 "out" = "Out-Migration from Multnomah",
 "in" = "In-Migration to Multnomah",
 "net" = "Net Migration (In - Out)"
)

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

 # Get direction and measure labels
 dir_label <- direction_labels[[direction]]
 meas_label <- measure_labels[[measure]]

 # Extract Multnomah County for highlighting
 multnomah_highlight <- map_data |> filter(GEOID == "41051")

 # Create the map
 p <- ggplot(map_data) +
   geom_sf(aes(fill = pct_change_capped), color = "gray80", linewidth = 0.05) +
   geom_sf(data = us_states, fill = NA, color = "gray25", linewidth = 0.25) +
   geom_sf(data = multnomah_highlight, fill = "yellow", color = "black", linewidth = 0.5) +
   scale_fill_gradient2(
     low = "#2166AC",
     mid = "white",
     high = "#B2182B",
     midpoint = 0,
     na.value = "gray90",
     name = legend_title,
     limits = c(-100, 200),
     breaks = c(-100, -50, 0, 50, 100, 150, 200),
     labels = c("-100%", "-50%", "0%", "+50%", "+100%", "+150%", "+200%")
   ) +
   labs(
     title = paste0(dir_label, ": ", meas_label),
     subtitle = paste0("Percent change (2018-2019 vs 2021-2022) - ", region_name),
     caption = "Note: Values capped at -100% to +200% for visualization"
   ) +
   theme_void() +
   theme(
     plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "black"),
     plot.subtitle = element_text(size = 10, hjust = 0.5, color = "black"),
     plot.caption = element_text(size = 8, hjust = 0.5, color = "black"),
     legend.position = "bottom",
     legend.key.width = unit(2, "cm"),
     legend.key.height = unit(0.3, "cm"),
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

 # Get direction and measure labels
 dir_label <- direction_labels[[direction]]
 meas_label <- measure_labels[[measure]]

 # Extract Multnomah County for highlighting
 multnomah_highlight <- map_data |> filter(GEOID == "41051")

 # Create the map
 p <- ggplot(map_data) +
   geom_sf(aes(fill = rate_change_capped), color = "gray80", linewidth = 0.05) +
   geom_sf(data = us_states, fill = NA, color = "gray25", linewidth = 0.25) +
   geom_sf(data = multnomah_highlight, fill = "yellow", color = "black", linewidth = 0.5) +
   scale_fill_gradient2(
     low = "#2166AC",
     mid = "white",
     high = "#B2182B",
     midpoint = 0,
     na.value = "gray90",
     name = legend_title
   ) +
   labs(
     title = paste0(dir_label, " Rate Change: ", meas_label),
     subtitle = paste0("Pre vs Post difference (2018-2019 vs 2021-2022) - ", region_name)
   ) +
   theme_void() +
   theme(
     plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "black"),
     plot.subtitle = element_text(size = 10, hjust = 0.5, color = "black"),
     legend.position = "bottom",
     legend.key.width = unit(2, "cm"),
     legend.key.height = unit(0.3, "cm"),
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
  xlim = c(wc_bbox["xmin"] - 50000, wc_bbox["xmax"] + 50000),
  ylim = c(wc_bbox["ymin"] - 50000, wc_bbox["ymax"] + 50000)
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
