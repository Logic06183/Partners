# Partner Geographic Map
# This script creates a geographic map showing all partners as dots

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Load required libraries
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("leaflet")) install.packages("leaflet")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("htmlwidgets")) install.packages("htmlwidgets")
if (!require("maps")) install.packages("maps")

library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)
if (require("maps")) {
  library(maps)
}

# Set the file path
file_path <- "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned_with_short_names.csv"

# Read the CSV file
partners <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)

# Print basic information about the dataset
cat("Number of partners:", nrow(partners), "\n")
cat("Number of countries:", length(unique(partners$Country)), "\n\n")

# Define partner type columns
partner_type_cols <- c("Official Partners", "CHAMNHA", "HEAT", "ENBEL", "GHAP", 
                       "HAPI", "BioHEAT", "HIGH_Horizons", "Funder", "Partners", 
                       "Data_Providers", "Government Partners", "Policy_Stakeholders",
                       "Gueladio_Cisse", "Matthew_Chersich", "Pilot_Projects")

# We need to limit the number of colors to 9 for Set1 palette
# Let's use the top 8 most common partner types
partner_counts <- sapply(partner_type_cols, function(col) sum(partners[[col]], na.rm = TRUE))
partner_counts_df <- data.frame(
  Type = partner_type_cols,
  Count = partner_counts
)
partner_counts_df <- partner_counts_df[order(-partner_counts_df$Count), ]
top_types <- head(partner_counts_df$Type, 8)
top_types <- c(top_types, "Other")

# Create a color palette for partner types
pal <- colorFactor(
  palette = "Set1",
  domain = top_types
)

# Add a primary type column to partners
# This assigns each partner to one primary type based on priority of the top types
get_primary_type <- function(row) {
  for (col in top_types) {
    if (col != "Other" && row[col] == 1) {
      return(col)
    }
  }
  return("Other")
}

# Apply the function to each row
partners$primary_type <- apply(partners[, partner_type_cols], 1, get_primary_type)

# Count number of partner types for each partner
partners$type_count <- rowSums(partners[, partner_type_cols])

# Create a basic map
basic_map <- leaflet(partners) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon, 
    lat = ~lat,
    popup = ~paste("<b>", Institution, "</b><br>",
                  "City: ", City, "<br>",
                  "Country: ", Country, "<br>",
                  "Short Name: ", Short_Name),
    radius = 5,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.8
  ) %>%
  addLegend(
    position = "bottomright",
    colors = "blue",
    labels = "All Partners",
    opacity = 0.8
  )

# Save the basic map
saveWidget(basic_map, "partner_geographic_map_basic.html", selfcontained = TRUE)

# Create a more advanced map with different colors for different partner types
# Use the primary_type for coloring
advanced_map <- leaflet(partners) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~lon, 
    lat = ~lat,
    popup = ~paste("<b>", Institution, "</b><br>",
                  "City: ", City, "<br>",
                  "Country: ", Country, "<br>",
                  "Type: ", primary_type, "<br>",
                  "Number of types: ", type_count),
    radius = ~pmin(8, 3 + type_count/2),  # Size based on number of types
    color = ~pal(primary_type),
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.7
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~primary_type,
    title = "Partner Types",
    opacity = 0.7
  )

# Save the advanced map
saveWidget(advanced_map, "partner_geographic_map_advanced.html", selfcontained = TRUE)

# Create a map focusing on Africa
africa_map <- leaflet(partners) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(
    lng = ~lon, 
    lat = ~lat,
    popup = ~paste("<b>", Institution, "</b><br>",
                  "City: ", City, "<br>",
                  "Country: ", Country, "<br>",
                  "Type: ", primary_type, "<br>",
                  "Number of types: ", type_count),
    radius = ~pmin(8, 3 + type_count/2),
    color = ~pal(primary_type),
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.7
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~primary_type,
    title = "Partner Types",
    opacity = 0.7
  ) %>%
  setView(lng = 20, lat = 0, zoom = 3)  # Center on Africa

# Save the Africa map
saveWidget(africa_map, "partner_geographic_map_africa.html", selfcontained = TRUE)

# Create a simple static map without using the maps package
# This will just show the points on a blank background
static_map_simple <- ggplot(partners, aes(x = lon, y = lat, color = primary_type, size = type_count)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set1", name = "Partner Type") +
  scale_size_continuous(name = "Number of Types", range = c(1, 5)) +
  theme_minimal() +
  labs(title = "Geographic Distribution of Partners",
       subtitle = paste("Total:", nrow(partners), "partners in", length(unique(partners$Country)), "countries"),
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom",
        legend.box = "vertical")

# Save the simple static map
ggsave("partner_geographic_map_simple.png", static_map_simple, width = 12, height = 8, dpi = 300)

# Try to create a more detailed static map if the maps package is available
if (require("maps")) {
  # Create a static version for easier sharing
  # First, let's create a static map using ggplot2
  world <- map_data("world")
  
  # Create a static map
  static_map <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), 
                 fill = "lightgray", color = "white", size = 0.1) +
    geom_point(data = partners, aes(x = lon, y = lat, color = primary_type, size = type_count),
               alpha = 0.7) +
    scale_color_brewer(palette = "Set1", name = "Partner Type") +
    scale_size_continuous(name = "Number of Types", range = c(1, 5)) +
    theme_minimal() +
    labs(title = "Geographic Distribution of Partners",
         subtitle = paste("Total:", nrow(partners), "partners in", length(unique(partners$Country)), "countries"),
         x = "", y = "") +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  # Save the static map
  ggsave("partner_geographic_map_static.png", static_map, width = 12, height = 8, dpi = 300)
  
  # Create a static map focusing on Africa
  africa_static_map <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), 
                 fill = "lightgray", color = "white", size = 0.1) +
    geom_point(data = partners, aes(x = lon, y = lat, color = primary_type, size = type_count),
               alpha = 0.7) +
    scale_color_brewer(palette = "Set1", name = "Partner Type") +
    scale_size_continuous(name = "Number of Types", range = c(2, 6)) +
    theme_minimal() +
    labs(title = "Geographic Distribution of Partners in Africa",
         subtitle = paste("Total:", nrow(partners), "partners in", length(unique(partners$Country)), "countries"),
         x = "", y = "") +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    coord_cartesian(xlim = c(-20, 60), ylim = c(-40, 40))  # Focus on Africa
  
  # Save the Africa static map
  ggsave("partner_geographic_map_africa_static.png", africa_static_map, width = 12, height = 8, dpi = 300)
}

# Print a completion message
cat("Geographic maps created successfully!\n")
cat("- partner_geographic_map_basic.html: Basic interactive map\n")
cat("- partner_geographic_map_advanced.html: Advanced interactive map with colors by type\n")
cat("- partner_geographic_map_africa.html: Interactive map focused on Africa\n")
cat("- partner_geographic_map_simple.png: Simple static map\n")
if (require("maps")) {
  cat("- partner_geographic_map_static.png: Static map with world boundaries\n")
  cat("- partner_geographic_map_africa_static.png: Static map focused on Africa\n")
}
