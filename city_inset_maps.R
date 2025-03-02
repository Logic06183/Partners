# City Inset Maps for Johannesburg and Cape Town
# This script creates detailed standalone inset maps for urban areas
# with enhanced underlying map details

# Load required libraries
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("sf")) install.packages("sf")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("ggspatial")) install.packages("ggspatial")
if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")
if (!require("osmdata")) install.packages("osmdata")

library(tidyverse)
library(sf)
library(ggrepel)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(osmdata)

# Set the file path
file_path <- "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned_with_short_names.csv"

# Read the data
partners <- read.csv(file_path, stringsAsFactors = FALSE)

# Define project categories (excluding individual people)
project_cats <- c("CHAMNHA", "HEAT", "ENBEL", "GHAP", 
                 "HAPI", "BioHEAT", "HIGH_Horizons", "Funder", 
                 "Data_Providers", "Government Partners", "Policy_Stakeholders",
                 "Pilot_Projects")

# Count project categories for each partner
partners$project_count <- rowSums(partners[, project_cats], na.rm = TRUE)

# Identify primary project for each partner
partners$primary_project <- colnames(partners[, project_cats])[max.col(partners[, project_cats], ties.method = "first")]

# Replace NA with "Other" for partners without a primary project
partners$primary_project[is.na(partners$primary_project)] <- "Other"

# Filter for Southern Africa partners
southern_africa_countries <- c(
  "South Africa", "Namibia", "Botswana", "Zimbabwe", 
  "Mozambique", "Lesotho", "Eswatini", "Zambia", "Malawi", "Angola"
)

sa_partners <- partners %>%
  filter(Country %in% southern_africa_countries) %>%
  # Ensure we have numeric coordinates
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  ) %>%
  # Remove any rows with NA coordinates
  filter(!is.na(lat) & !is.na(lon))

# Rename some of the project categories to make them more readable
sa_partners$display_category <- sa_partners$primary_project
sa_partners$display_category <- gsub("_", " ", sa_partners$display_category)
sa_partners$display_category <- gsub("HIGH Horizons", "HIGH", sa_partners$display_category)
sa_partners$display_category <- gsub("Government Partners", "Government", sa_partners$display_category)
sa_partners$display_category <- gsub("Policy Stakeholders", "Policy", sa_partners$display_category)
sa_partners$display_category <- gsub("Data Providers", "Data Provider", sa_partners$display_category)
sa_partners$display_category[sa_partners$display_category == "Official Partners"] <- "Research Partner"

# Create a custom scientific color palette based on Nature/Science journal aesthetics
scientific_palette <- c(
  "#4878d0", # Blue
  "#ee854a", # Orange
  "#6acc64", # Green
  "#d65f5f", # Red
  "#956cb4", # Purple
  "#8c613c", # Brown
  "#dc7ec0", # Pink
  "#797979"  # Gray
)

# Define bounding boxes for inset maps of dense urban areas
johannesburg_bbox <- c(
  xmin = 27.90, 
  xmax = 28.15, 
  ymin = -26.30, 
  ymax = -26.05
)

cape_town_bbox <- c(
  xmin = 18.35, 
  xmax = 18.60, 
  ymin = -34.00, 
  ymax = -33.75
)

# Filter partners in Johannesburg area
joburg_partners <- sa_partners %>%
  filter(
    lon >= johannesburg_bbox["xmin"],
    lon <= johannesburg_bbox["xmax"],
    lat >= johannesburg_bbox["ymin"],
    lat <= johannesburg_bbox["ymax"]
  )

# Filter partners in Cape Town area
cape_town_partners <- sa_partners %>%
  filter(
    lon >= cape_town_bbox["xmin"],
    lon <= cape_town_bbox["xmax"],
    lat >= cape_town_bbox["ymin"],
    lat <= cape_town_bbox["ymax"]
  )

# Print summary of partners in the inset areas
cat("\nPartners in Johannesburg area:", nrow(joburg_partners), "\n")
cat("Partners in Cape Town area:", nrow(cape_town_partners), "\n\n")

# Get detailed OSM data for Johannesburg
joburg_osm <- opq(bbox = c(
    johannesburg_bbox["xmin"], 
    johannesburg_bbox["ymin"], 
    johannesburg_bbox["xmax"], 
    johannesburg_bbox["ymax"]
  )) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "trunk", "primary", "secondary")) %>%
  osmdata_sf()

# Get detailed OSM data for Cape Town
cape_town_osm <- opq(bbox = c(
    cape_town_bbox["xmin"], 
    cape_town_bbox["ymin"], 
    cape_town_bbox["xmax"], 
    cape_town_bbox["ymax"]
  )) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "trunk", "primary", "secondary")) %>%
  osmdata_sf()

# Get water bodies for Johannesburg
joburg_water <- opq(bbox = c(
    johannesburg_bbox["xmin"], 
    johannesburg_bbox["ymin"], 
    johannesburg_bbox["xmax"], 
    johannesburg_bbox["ymax"]
  )) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

# Get water bodies for Cape Town
cape_town_water <- opq(bbox = c(
    cape_town_bbox["xmin"], 
    cape_town_bbox["ymin"], 
    cape_town_bbox["xmax"], 
    cape_town_bbox["ymax"]
  )) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

# Get land use for Johannesburg
joburg_landuse <- opq(bbox = c(
    johannesburg_bbox["xmin"], 
    johannesburg_bbox["ymin"], 
    johannesburg_bbox["xmax"], 
    johannesburg_bbox["ymax"]
  )) %>%
  add_osm_feature(key = "landuse", 
                  value = c("residential", "commercial", "industrial", "retail")) %>%
  osmdata_sf()

# Get land use for Cape Town
cape_town_landuse <- opq(bbox = c(
    cape_town_bbox["xmin"], 
    cape_town_bbox["ymin"], 
    cape_town_bbox["xmax"], 
    cape_town_bbox["ymax"]
  )) %>%
  add_osm_feature(key = "landuse", 
                  value = c("residential", "commercial", "industrial", "retail")) %>%
  osmdata_sf()

# Set a publication-quality theme for inset maps
inset_theme <- theme_minimal() +
  theme(
    text = element_text(family = "serif", color = "#333333"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 15), color = "#555555"),
    axis.title = element_blank(),
    axis.text = element_text(size = 8),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.5, "cm"),
    panel.grid.major = element_line(color = "#e0e0e0", size = 0.2),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f8f9fa", color = NA),
    plot.background = element_rect(fill = "white", color = "#333333", size = 0.5),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

# Create the Johannesburg inset map with detailed underlying map
johannesburg_inset <- ggplot() +
  # Add land use polygons
  geom_sf(data = joburg_landuse$osm_polygons, aes(fill = osm_id), alpha = 0.2, color = NA) +
  scale_fill_manual(values = rep(c("#e6e6e6", "#f0f0f0", "#d9d9d9", "#e6e6e6"), 1000)) +
  # Add water bodies
  geom_sf(data = joburg_water$osm_polygons, fill = "#cde4f5", color = "#a8cce5", size = 0.2) +
  # Add roads with different weights
  geom_sf(data = joburg_osm$osm_lines, aes(color = highway), size = 0.8) +
  scale_color_manual(values = c(
    "motorway" = "#e74c3c",
    "trunk" = "#e67e22",
    "primary" = "#f1c40f",
    "secondary" = "#95a5a6"
  )) +
  # Add points for partner locations
  geom_point(
    data = joburg_partners,
    aes(
      x = lon, 
      y = lat, 
      color = display_category,
      size = project_count
    ),
    alpha = 0.9,
    stroke = 0.8,
    shape = 21,
    fill = "white"
  ) +
  # Use the same color palette as the main map
  scale_color_manual(values = scientific_palette, name = "Project Category") +
  scale_size_continuous(
    name = "Number of Projects", 
    range = c(4, 8),
    breaks = c(1, 2, 3, 5)
  ) +
  # Set proper coordination limits for Johannesburg
  coord_sf(
    xlim = c(johannesburg_bbox["xmin"], johannesburg_bbox["xmax"]),
    ylim = c(johannesburg_bbox["ymin"], johannesburg_bbox["ymax"]),
    expand = FALSE
  ) +
  # Add labels for ALL institutions in this area
  geom_text_repel(
    data = joburg_partners,
    aes(
      x = lon, 
      y = lat, 
      label = Institution
    ),
    size = 3,
    fontface = "plain",
    family = "serif",
    color = "#333333",
    bg.color = "white",
    bg.r = 0.15,
    segment.size = 0.3,
    segment.color = "#666666",
    min.segment.length = 0.1,
    point.padding = 0.3,
    box.padding = 0.5,
    force = 2,
    max.overlaps = 30,
    seed = 42,
    direction = "both"
  ) +
  # Add a scale bar for scientific accuracy
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    style = "ticks",
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm")
  ) +
  # Add north arrow for scientific accuracy
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm"),
    style = north_arrow_minimal(
      line_width = 0.8,
      line_col = "#333333",
      fill = "#333333",
      text_col = "#333333"
    )
  ) +
  # Add a title
  labs(
    title = "Johannesburg Research Partners",
    subtitle = paste0("Detailed view of ", nrow(joburg_partners), " research partners")
  ) +
  # Apply the inset theme
  inset_theme +
  # Hide the highway legend
  guides(fill = "none", color = guide_legend(override.aes = list(size = 4)))

# Create the Cape Town inset map with detailed underlying map
cape_town_inset <- ggplot() +
  # Add land use polygons
  geom_sf(data = cape_town_landuse$osm_polygons, aes(fill = osm_id), alpha = 0.2, color = NA) +
  scale_fill_manual(values = rep(c("#e6e6e6", "#f0f0f0", "#d9d9d9", "#e6e6e6"), 1000)) +
  # Add water bodies
  geom_sf(data = cape_town_water$osm_polygons, fill = "#cde4f5", color = "#a8cce5", size = 0.2) +
  # Add roads with different weights
  geom_sf(data = cape_town_osm$osm_lines, aes(color = highway), size = 0.8) +
  scale_color_manual(values = c(
    "motorway" = "#e74c3c",
    "trunk" = "#e67e22",
    "primary" = "#f1c40f",
    "secondary" = "#95a5a6"
  )) +
  # Add points for partner locations
  geom_point(
    data = cape_town_partners,
    aes(
      x = lon, 
      y = lat, 
      color = display_category,
      size = project_count
    ),
    alpha = 0.9,
    stroke = 0.8,
    shape = 21,
    fill = "white"
  ) +
  # Use the same color palette as the main map
  scale_color_manual(values = scientific_palette, name = "Project Category") +
  scale_size_continuous(
    name = "Number of Projects", 
    range = c(4, 8),
    breaks = c(1, 2, 3, 5)
  ) +
  # Set proper coordination limits for Cape Town
  coord_sf(
    xlim = c(cape_town_bbox["xmin"], cape_town_bbox["xmax"]),
    ylim = c(cape_town_bbox["ymin"], cape_town_bbox["ymax"]),
    expand = FALSE
  ) +
  # Add labels for ALL institutions in this area
  geom_text_repel(
    data = cape_town_partners,
    aes(
      x = lon, 
      y = lat, 
      label = Institution
    ),
    size = 3,
    fontface = "plain",
    family = "serif",
    color = "#333333",
    bg.color = "white",
    bg.r = 0.15,
    segment.size = 0.3,
    segment.color = "#666666",
    min.segment.length = 0.1,
    point.padding = 0.3,
    box.padding = 0.5,
    force = 2,
    max.overlaps = 30,
    seed = 42,
    direction = "both"
  ) +
  # Add a scale bar for scientific accuracy
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    style = "ticks",
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm")
  ) +
  # Add north arrow for scientific accuracy
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm"),
    style = north_arrow_minimal(
      line_width = 0.8,
      line_col = "#333333",
      fill = "#333333",
      text_col = "#333333"
    )
  ) +
  # Add a title
  labs(
    title = "Cape Town Research Partners",
    subtitle = paste0("Detailed view of ", nrow(cape_town_partners), " research partners")
  ) +
  # Apply the inset theme
  inset_theme +
  # Hide the highway legend
  guides(fill = "none", color = guide_legend(override.aes = list(size = 4)))

# Save the Johannesburg inset map as a standalone file
ggsave(
  "johannesburg_inset_map.png", 
  johannesburg_inset, 
  width = 8, 
  height = 7, 
  dpi = 600,
  bg = "white"
)

# Save the Cape Town inset map as a standalone file
ggsave(
  "cape_town_inset_map.png", 
  cape_town_inset, 
  width = 8, 
  height = 7, 
  dpi = 600,
  bg = "white"
)

# Save as PDF for vector graphics (better for publication)
ggsave(
  "johannesburg_inset_map.pdf", 
  johannesburg_inset, 
  width = 8, 
  height = 7, 
  device = cairo_pdf
)

ggsave(
  "cape_town_inset_map.pdf", 
  cape_town_inset, 
  width = 8, 
  height = 7, 
  device = cairo_pdf
)

cat("Detailed standalone inset maps created successfully!\n")
cat("- johannesburg_inset_map.png: High-resolution PNG map of Johannesburg\n")
cat("- cape_town_inset_map.png: High-resolution PNG map of Cape Town\n")
cat("- johannesburg_inset_map.pdf: Vector PDF map of Johannesburg\n")
cat("- cape_town_inset_map.pdf: Vector PDF map of Cape Town\n")
