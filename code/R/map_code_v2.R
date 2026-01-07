# ============================================================
# Multnomah Migration Maps + US County Pool Map (portable paths)
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(tidyverse)
  library(tigris)
  library(readxl)
  library(here)
})

options(tigris_use_cache = TRUE)


home_base <- here()
results_dir <- file.path(home_base, "results")
data_dir    <- file.path(home_base, "data")

# ------------------------------------------------------------
# Relative file paths (edit only if your repo layout differs)
# ------------------------------------------------------------
filepath1   <- file.path(results_dir, "map1.png")
filepath2   <- file.path(results_dir, "map2.png")
filepath_us <- file.path(results_dir, "map_us_pool.png")

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
  geom_sf(data = multnomah, fill = "gold", alpha = 0.7, color = "black", size = 0.5) +
  
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
  geom_sf(data = multnomah, fill = "gold", alpha = 0.6, color = "black", size = 0.6) +
  
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
    data = st_centroid(metro_reg), aes(label = "METRO"),
    color = "#9E0168",
    size = 3, fontface = "bold"
  ) +
  theme_void() +
  coord_sf(expand = FALSE)

ggsave(filepath2, map2, width = 10, height = 8, dpi = 300)
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
      "Multnomah" = "#C1121F"   # deep red
    ),
    name = NULL
  ) +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.text = element_text(colour = "white"))


ggsave(filepath_us, map_us_pool, width = 12, height = 7, dpi = 300)

message("Saved: ", filepath1)
message("Saved: ", filepath2)
message("Saved: ", filepath_us)

