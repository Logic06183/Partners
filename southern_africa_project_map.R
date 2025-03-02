# Southern Africa Project Map - Publication Quality
# This script creates a detailed, publication-quality map focused on Southern Africa
# showing partners categorized by project type

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Load required libraries
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("maps")) install.packages("maps")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("sf")) install.packages("sf")
if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")
if (!require("viridis")) install.packages("viridis")
if (!require("ggspatial")) install.packages("ggspatial")
if (!require("cowplot")) install.packages("cowplot")
if (!require("grid")) install.packages("grid")

library(tidyverse)
library(RColorBrewer)
library(maps)
library(ggrepel)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(ggspatial)
library(cowplot)
library(grid)

# Set the file path
file_path <- "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned_with_short_names.csv"

# Read the CSV file
partners <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)

# Print basic information about the dataset
cat("Number of partners:", nrow(partners), "\n")
cat("Number of countries:", length(unique(partners$Country)), "\n\n")

# Define project categories (excluding individual people and filtering out "Official Partners")
project_cats <- c("CHAMNHA", "HEAT", "ENBEL", "GHAP", 
                 "HAPI", "BioHEAT", "HIGH_Horizons", "Funder", 
                 "Data_Providers", "Government Partners", "Policy_Stakeholders",
                 "Pilot_Projects")

# Count project categories for each partner
partners$project_count <- rowSums(partners[, project_cats], na.rm = TRUE)

# Determine primary project category for each partner
get_primary_project <- function(row) {
  for (col in project_cats) {
    if (!is.na(row[col]) && row[col] == 1) {
      return(col)
    }
  }
  return("Other")
}

# Apply the function to each row
partners$primary_project <- apply(partners[, project_cats], 1, get_primary_project)

# Filter out the specific individuals as requested
partners <- partners %>%
  filter(!(primary_project %in% c("Gueladio_Cisse", "Matthew_Chersich")))

# Print project categories and counts
project_counts <- table(partners$primary_project)
cat("Project categories and counts:\n")
print(project_counts)

# Focus on southern African countries
southern_africa_countries <- c("South Africa", "Namibia", "Botswana", "Zimbabwe", 
                              "Mozambique", "Eswatini", "Lesotho", 
                              "Zambia", "Malawi", "Angola")

# Filter partners for Southern Africa
sa_partners <- partners %>%
  filter(Country %in% southern_africa_countries)

# Get world map data with higher resolution for high-quality publication
world <- ne_countries(scale = "medium", returnclass = "sf")

# Get the bounding box for Southern Africa
sa_bbox <- c(
  xmin = 10, # Western longitude
  xmax = 41, # Eastern longitude
  ymin = -35, # Southern latitude
  ymax = -8   # Northern latitude
)

# Improve styling for publication quality
# Use a sophisticated color palette inspired by scientific journals
map_background <- "#f8f9fa"
border_color <- "#d3d3d3"
water_color <- "#e6f2ff"
land_color <- "#f5f5f5"

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

# Rename some of the project categories to make them more readable and scientifically appropriate
sa_partners$display_category <- sa_partners$primary_project
sa_partners$display_category <- gsub("_", " ", sa_partners$display_category)
sa_partners$display_category <- gsub("HIGH Horizons", "HIGH", sa_partners$display_category)
sa_partners$display_category <- gsub("Government Partners", "Government", sa_partners$display_category)
sa_partners$display_category <- gsub("Policy Stakeholders", "Policy", sa_partners$display_category)
sa_partners$display_category <- gsub("Data Providers", "Data Provider", sa_partners$display_category)
# Remove "Official Partners" from display categories
sa_partners$display_category[sa_partners$display_category == "Official Partners"] <- "Research Partner"

# Create strategic city clusters by grouping closely located points
# This prevents label overlap by only labeling key cities
sa_partners <- sa_partners %>%
  # Add importance score based on project count and uniqueness
  mutate(importance = project_count * 2)

# Create a smarter labeling strategy - using fewer labels
# Group nearby cities and only label the most important ones
city_clusters <- sa_partners %>%
  group_by(Country) %>%
  # Get the top 2-3 cities per country based on importance
  slice_max(order_by = importance, n = 2, with_ties = FALSE) %>%
  ungroup()

# Set a publication-quality theme inspired by scientific journals like Nature and Science
publication_theme <- theme_minimal() +
  theme(
    text = element_text(family = "serif", color = "#333333"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 25), color = "#555555"),
    plot.caption = element_text(size = 9, hjust = 1, margin = margin(t = 20), color = "#555555", face = "italic"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = map_background, color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.7, "cm"),
    legend.box = "vertical",
    legend.margin = margin(t = 10, r = 10, b = 10, l = 10),
    legend.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 25, r = 25, b = 25, l = 25)
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

# Create the base map with enhanced scientific styling
main_map <- ggplot() +
  # Add country borders with improved styling
  geom_sf(data = world, fill = land_color, color = border_color, size = 0.3) +
  # Add a subtle graticule (grid lines) for scientific precision
  geom_sf(data = st_graticule(lat = seq(-40, 0, 10), lon = seq(10, 40, 10)), 
          color = "#e0e0e0", size = 0.1, alpha = 0.5) +
  # Add points for partner locations with sizing based on project count
  geom_point(
    data = sa_partners,
    aes(
      x = lon, 
      y = lat, 
      color = display_category,
      size = project_count
    ),
    alpha = 0.85,
    stroke = 0.5,
    shape = 21,
    fill = "white"
  ) +
  # Use a scientific color palette and set proper labels
  scale_color_manual(values = scientific_palette, name = "Project Category") +
  scale_size_continuous(
    name = "Number of Projects", 
    range = c(3, 8),
    breaks = c(1, 2, 3, 5)
  ) +
  # Set proper coordination limits for Southern Africa
  coord_sf(
    xlim = c(sa_bbox["xmin"], sa_bbox["xmax"]), 
    ylim = c(sa_bbox["ymin"], sa_bbox["ymax"]),
    expand = FALSE
  ) +
  # Add country labels with improved styling
  geom_text(
    data = world %>% 
      filter(name %in% southern_africa_countries) %>%
      st_centroid() %>%
      st_coordinates() %>%
      as.data.frame() %>%
      mutate(Country = world$name[world$name %in% southern_africa_countries][1:nrow(.)]) %>%
      rename(lon = X, lat = Y),
    aes(x = lon, y = lat, label = Country),
    fontface = "italic",
    color = "#888888",
    size = 4,
    alpha = 0.6
  ) +
  # Add labels for major institutions using full names with improved styling
  geom_text_repel(
    data = city_clusters,
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
    segment.color = "#888888",
    min.segment.length = 0.2,
    point.padding = 0.5,
    box.padding = 0.8,
    force = 3,
    max.overlaps = 20,
    seed = 42,
    direction = "both"
  ) +
  # Add a scale bar for scientific accuracy
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    style = "ticks",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  # Add north arrow for scientific accuracy
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_minimal(
      line_width = 1,
      line_col = "#333333",
      fill = "#333333",
      text_col = "#333333"
    )
  ) +
  # Add a title and labels with improved scientific wording
  labs(
    title = "Research Partner Distribution in Southern Africa",
    subtitle = paste0(
      "Spatial distribution of ", nrow(sa_partners), 
      " research partners across ", length(unique(sa_partners$Country)), 
      " countries by project category"
    ),
    caption = "Data source: Partners Database (2025) | Map projection: WGS84"
  ) +
  # Apply the publication-quality theme
  publication_theme

# Add rectangles to show the inset map areas on the main map
main_map_with_insets <- main_map +
  # Add rectangle for Johannesburg inset
  geom_rect(
    xmin = johannesburg_bbox["xmin"],
    ymin = johannesburg_bbox["ymin"],
    xmax = johannesburg_bbox["xmax"],
    ymax = johannesburg_bbox["ymax"],
    fill = NA, 
    color = "#333333",
    size = 0.7,
    linetype = "dashed"
  ) +
  # Add rectangle for Cape Town inset
  geom_rect(
    xmin = cape_town_bbox["xmin"],
    ymin = cape_town_bbox["ymin"],
    xmax = cape_town_bbox["xmax"],
    ymax = cape_town_bbox["ymax"],
    fill = NA, 
    color = "#333333",
    size = 0.7,
    linetype = "dashed"
  ) +
  # Add labels for the inset areas
  annotate(
    "text", 
    x = johannesburg_bbox["xmax"] + 0.5, 
    y = johannesburg_bbox["ymax"], 
    label = "Johannesburg\nArea", 
    hjust = 0, 
    size = 3, 
    fontface = "bold",
    family = "serif"
  ) +
  annotate(
    "text", 
    x = cape_town_bbox["xmax"] + 0.5, 
    y = cape_town_bbox["ymax"], 
    label = "Cape Town\nArea", 
    hjust = 0, 
    size = 3, 
    fontface = "bold",
    family = "serif"
  )

# Create the Johannesburg inset map
# Filter partners in Johannesburg area for more detailed labeling
joburg_partners <- sa_partners %>%
  filter(
    lon >= johannesburg_bbox["xmin"],
    lon <= johannesburg_bbox["xmax"],
    lat >= johannesburg_bbox["ymin"],
    lat <= johannesburg_bbox["ymax"]
  )

johannesburg_inset <- ggplot() +
  # Add country borders
  geom_sf(data = world, fill = land_color, color = border_color, size = 0.2) +
  # Add points for partner locations
  geom_point(
    data = joburg_partners,
    aes(
      x = lon, 
      y = lat, 
      color = display_category,
      size = project_count
    ),
    alpha = 0.85,
    stroke = 0.5,
    shape = 21,
    fill = "white"
  ) +
  # Use the same color palette as the main map
  scale_color_manual(values = scientific_palette, name = "Project Category") +
  scale_size_continuous(
    name = "Number of Projects", 
    range = c(3, 6),
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
    size = 2.5,
    fontface = "plain",
    family = "serif",
    color = "#333333",
    bg.color = "white",
    bg.r = 0.15,
    segment.size = 0.2,
    segment.color = "#888888",
    min.segment.length = 0.1,
    point.padding = 0.3,
    box.padding = 0.5,
    force = 2,
    max.overlaps = 30,
    seed = 42,
    direction = "both"
  ) +
  # Add a title
  labs(title = "Johannesburg Area") +
  # Apply a simplified theme
  theme_minimal() +
  theme(
    text = element_text(family = "serif", color = "#333333"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(size = 6),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = "#333333", size = 0.5),
    plot.margin = margin(5, 5, 5, 5)
  )

# Create the Cape Town inset map
# Filter partners in Cape Town area for more detailed labeling
cape_town_partners <- sa_partners %>%
  filter(
    lon >= cape_town_bbox["xmin"],
    lon <= cape_town_bbox["xmax"],
    lat >= cape_town_bbox["ymin"],
    lat <= cape_town_bbox["ymax"]
  )

cape_town_inset <- ggplot() +
  # Add country borders
  geom_sf(data = world, fill = land_color, color = border_color, size = 0.2) +
  # Add points for partner locations
  geom_point(
    data = cape_town_partners,
    aes(
      x = lon, 
      y = lat, 
      color = display_category,
      size = project_count
    ),
    alpha = 0.85,
    stroke = 0.5,
    shape = 21,
    fill = "white"
  ) +
  # Use the same color palette as the main map
  scale_color_manual(values = scientific_palette, name = "Project Category") +
  scale_size_continuous(
    name = "Number of Projects", 
    range = c(3, 6),
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
    size = 2.5,
    fontface = "plain",
    family = "serif",
    color = "#333333",
    bg.color = "white",
    bg.r = 0.15,
    segment.size = 0.2,
    segment.color = "#888888",
    min.segment.length = 0.1,
    point.padding = 0.3,
    box.padding = 0.5,
    force = 2,
    max.overlaps = 30,
    seed = 42,
    direction = "both"
  ) +
  # Add a title
  labs(title = "Cape Town Area") +
  # Apply a simplified theme
  theme_minimal() +
  theme(
    text = element_text(family = "serif", color = "#333333"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(size = 6),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = "#333333", size = 0.5),
    plot.margin = margin(5, 5, 5, 5)
  )

# Combine the main map with insets using cowplot
final_map <- ggdraw(main_map_with_insets) +
  # Add Johannesburg inset
  draw_plot(
    johannesburg_inset,
    x = 0.22,  # Position from left
    y = 0.15,  # Position from bottom
    width = 0.25,  # Width of inset
    height = 0.25   # Height of inset
  ) +
  # Add Cape Town inset
  draw_plot(
    cape_town_inset,
    x = 0.22,  # Position from left
    y = 0.45,  # Position from bottom
    width = 0.25,  # Width of inset
    height = 0.25   # Height of inset
  )

# Print summary of partners in the inset areas
cat("\nPartners in Johannesburg area:", nrow(joburg_partners), "\n")
cat("Partners in Cape Town area:", nrow(cape_town_partners), "\n\n")

# Save the high-quality map with insets for publication
ggsave(
  "southern_africa_project_map_with_insets.png", 
  final_map, 
  width = 14, 
  height = 10, 
  dpi = 600, # High resolution for publication
  bg = "white"
)

# Save as PDF for vector graphics (better for publication)
ggsave(
  "southern_africa_project_map_with_insets.pdf", 
  final_map, 
  width = 14, 
  height = 10, 
  device = cairo_pdf
)

cat("Publication-quality maps with insets created successfully!\n")
cat("- southern_africa_project_map_with_insets.png: High-resolution PNG map\n")
cat("- southern_africa_project_map_with_insets.pdf: Vector PDF map for publication\n")
