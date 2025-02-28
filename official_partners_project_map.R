# Official Partners Project Map
# This script creates a map showing official partners categorized by project

# Set CRAN mirror first
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Function to install and load a package
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    if (!require(package, character.only = TRUE)) {
      stop(paste("Package", package, "failed to install/load"))
    }
  }
}

# Load required packages
required_packages <- c(
  "tidyverse", "sf", "rnaturalearth", "rnaturalearthdata", 
  "ggspatial", "viridis", "RColorBrewer", "ggrepel", 
  "ggthemes", "scales", "ggraph", "ggnewscale", "cowplot", "grid", "shadowtext"
)

# Install and load each package
for (pkg in required_packages) {
  install_and_load(pkg)
}

# Set fonts
title_font <- ""
body_font <- ""

# Load the data
partners_data <- read.delim("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned.csv", 
                           sep = "\t", stringsAsFactors = FALSE)

# Print column names for debugging
cat("Column names in CSV:", paste(colnames(partners_data), collapse=", "), "\n")

# Filter only official partners
official_partners <- partners_data %>%
  filter(Official.Partners == 1)

# Define project columns and their colors
project_cols <- c("CHAMNHA", "HEAT", "ENBEL", "GHAP", "HAPI", "BioHEAT", "HIGH_Horizons")

# Rename HEAT to HE²AT with superscript for display
project_display_names <- c(
  "CHAMNHA" = "CHAMNHA",
  "HEAT" = "HE²AT",  # Using the superscript 2
  "ENBEL" = "ENBEL",
  "GHAP" = "GHAP",
  "HAPI" = "HAPI",
  "BioHEAT" = "BioHEAT",
  "HIGH_Horizons" = "HIGH_Horizons"
)

project_colors <- c(
  "CHAMNHA" = "#FF9E44",       # Orange
  "HEAT" = "#FFCC00",          # Yellow
  "ENBEL" = "#4DAF4A",         # Green
  "GHAP" = "#377EB8",          # Blue
  "HAPI" = "#984EA3",          # Purple
  "BioHEAT" = "#E41A1C",       # Red
  "HIGH_Horizons" = "#00CCCC"  # Teal
)

# Transform data for plotting - create a row for each institution-project combination
partners_long <- official_partners %>%
  pivot_longer(
    cols = all_of(project_cols),
    names_to = "Project",
    values_to = "Involved"
  ) %>%
  filter(Involved == 1)  # Keep only where institution is involved in project

# Count projects per institution for dot sizing
partners_project_count <- official_partners %>%
  rowwise() %>%
  mutate(
    project_count = sum(c_across(all_of(project_cols))),
    size_factor = sqrt(project_count) * 1.2  # Square root scaling for better visual representation
  ) %>%
  ungroup()

# Print data summary
cat("Number of official partners:", nrow(official_partners), "\n")
cat("Number of project involvements:", nrow(partners_long), "\n")

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define HEAT contributing countries - based on the HEAT column = 1
heat_countries <- official_partners %>% 
  filter(HEAT == 1) %>% 
  pull(Country) %>% 
  unique()

cat("HEAT contributing countries:", paste(heat_countries, collapse=", "), "\n")

# Add fill color to world data
world <- world %>%
  mutate(fill_color = ifelse(name %in% heat_countries, "#FFFAC8", "white"))

# Create graticules for scientific look
custom_graticule <- st_graticule(
  lat = seq(-90, 90, by = 15),
  lon = seq(-180, 180, by = 15),
  crs = st_crs(4326)
)

# Define regions for maps
# Main map centered on Africa and Europe
africa_europe <- world %>%
  st_crop(c(xmin = -25, xmax = 50, ymin = -40, ymax = 70))

# Europe detailed map
europe <- world %>%
  st_crop(c(xmin = -20, xmax = 40, ymin = 35, ymax = 70))

# Define African and European partners for labeling
# For Africa, only include major partners (involved in 2+ projects)
african_partners <- official_partners %>%
  filter(lat < 35, lat > -40, lon > -25, lon < 50) %>%  # African bounds
  group_by(Institution) %>% 
  summarise(
    lon = first(lon),
    lat = first(lat),
    project_count = sum(across(all_of(project_cols))),
    Country = first(Country),
    .groups = "drop"
  ) %>%
  filter(project_count >= 2)  # Only include partners involved in 2+ projects

# For Europe, include all partners
european_partners <- official_partners %>% 
  filter(lat > 35 & lat < 70 & lon > -20 & lon < 40) %>%  # European bounds
  group_by(Institution) %>% 
  summarise(
    lon = first(lon),
    lat = first(lat),
    project_count = sum(across(all_of(project_cols))),
    Country = first(Country),
    .groups = "drop"
  )

#-------------------------
# CREATE MAIN MAP (AFRICA + EUROPE)
#-------------------------
main_map <- ggplot() +
  # Add graticules
  geom_sf(data = custom_graticule,
          color = alpha("gray80", 0.5),
          size = 0.2) +
  
  # Add world base with HEAT countries highlighted
  geom_sf(data = africa_europe,
          aes(fill = fill_color),
          color = "gray40",
          size = 0.2) +
  
  # Set fill colors manually for HEAT countries only
  scale_fill_identity(
    name = "Country Type",
    labels = c("HE²AT Center Contributing Partner"),
    guide = guide_legend(
      override.aes = list(
        fill = c("#FFFAC8"),
        color = "gray40",
        size = 0.5
      )
    )
  ) +
  
  # Add new scale for points
  new_scale_fill() +
  
  # Add partner points with size based on number of projects
  geom_point(
    data = partners_project_count,
    aes(x = lon, y = lat, size = size_factor),
    color = "gray30",
    alpha = 0.5
  ) +
  
  # Hide the size legend
  scale_size_identity() +
  
  # Add partner points colored by project
  geom_point(data = partners_long,
             aes(x = lon, y = lat, color = Project, group = Institution),
             size = 3, alpha = 0.8) +
  
  # Set project colors with updated HE²AT label
  scale_color_manual(
    values = project_colors,
    labels = project_display_names
  ) +
  
  # Add African partner labels with white outline for better visibility
  # Only for major partners (2+ projects)
  geom_shadowtext(
    data = african_partners,
    aes(x = lon, y = lat, label = Institution),
    size = 2.8,
    fontface = "bold",
    bg.color = "white",
    bg.r = 0.2,
    color = "black"
  ) +
  
  # Add scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    style = "bar",
    text_cex = 0.7,
    bar_cols = c("black", "white")
  ) +
  
  # Add north arrow
  annotation_north_arrow(
    location = "bl", 
    which_north = "true",
    pad_x = unit(0.2, "in"), 
    pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering(
      fill = c("white", "black"),
      line_col = "black",
      text_col = "black"
    ),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  
  # Add title and captions
  labs(
    title = "Official Partners by Project",
    subtitle = "Africa and Europe Overview",
    caption = paste0("Data source: partners_cleaned.csv\nCreated: ", format(Sys.Date(), "%d %B %Y")),
    x = "Longitude",
    y = "Latitude",
    color = "Project"
  ) +
  
  # Set theme elements
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, color = "gray30", hjust = 1),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.major = element_line(color = "gray85", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "aliceblue"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  
  # Set map projection and limits with fixed aspect ratio
  coord_sf(
    xlim = c(-25, 50),
    ylim = c(-40, 70),
    expand = FALSE,
    datum = NA  # Prevents map distortion
  )

#-------------------------
# CREATE DETAILED EUROPE MAP
#-------------------------
europe_map <- ggplot() +
  # Add graticules
  geom_sf(data = custom_graticule,
          color = alpha("gray80", 0.5),
          size = 0.2) +
  
  # Add Europe base
  geom_sf(data = europe,
          aes(fill = fill_color),
          color = "gray40",
          size = 0.2) +
  
  # Set fill colors manually
  scale_fill_identity() +
  
  # Add new scale for points
  new_scale_fill() +
  
  # Add partner points with size based on number of projects
  geom_point(
    data = partners_project_count %>% 
      filter(lat > 35 & lat < 70 & lon > -20 & lon < 40),  # European bounds
    aes(x = lon, y = lat, size = size_factor),
    color = "gray30",
    alpha = 0.5
  ) +
  
  # Hide the size legend
  scale_size_identity() +
  
  # Filter for European partners only
  # Add partner points colored by project
  geom_point(
    data = partners_long %>% 
      filter(lat > 35 & lat < 70 & lon > -20 & lon < 40),  # European bounds
    aes(x = lon, y = lat, color = Project, group = Institution),
    size = 4, alpha = 0.8
  ) +
  
  # Set project colors with updated HE²AT label
  scale_color_manual(
    values = project_colors,
    labels = project_display_names
  ) +
  
  # Add partner labels for ALL European institutions with white outline
  geom_shadowtext(
    data = european_partners,
    aes(x = lon, y = lat, label = Institution),
    size = 3.0,
    fontface = "bold",
    bg.color = "white",
    bg.r = 0.15,
    color = "black",
    check_overlap = TRUE
  ) +
  
  # Add scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    style = "bar",
    text_cex = 0.7,
    bar_cols = c("black", "white")
  ) +
  
  # Add title and captions
  labs(
    title = "European Official Partners by Project",
    subtitle = "Detailed View",
    caption = paste0("Data source: partners_cleaned.csv\nCreated: ", format(Sys.Date(), "%d %B %Y")),
    x = "Longitude",
    y = "Latitude",
    color = "Project"
  ) +
  
  # Set theme elements
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0),
    plot.caption = element_text(size = 8, color = "gray30", hjust = 1),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.major = element_line(color = "gray85", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "aliceblue"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  
  # Set map projection and limits with fixed aspect ratio
  coord_sf(
    xlim = c(-20, 40),
    ylim = c(35, 70),
    expand = FALSE,
    datum = NA  # Prevents map distortion
  )

#-------------------------
# SAVE MAPS AS SEPARATE PDFs WITH OPTIMIZED DIMENSIONS
#-------------------------

# Save the Africa/Europe map as PDF with optimized dimensions
ggsave(
  "official_partners_africa_europe_map.pdf",
  plot = main_map,
  width = 11,
  height = 8,
  device = cairo_pdf
)

# Save the Europe detailed map as PDF with optimized dimensions
ggsave(
  "official_partners_europe_detail_map.pdf",
  plot = europe_map,
  width = 10,
  height = 8,
  device = cairo_pdf
)

print("Maps created successfully as separate PDFs with enhanced styling!")
