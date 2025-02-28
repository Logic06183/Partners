# Wits Planetary Health Partnership Map
# This script creates a map showing research partnerships in Africa and Europe

# Load required packages
library(ggplot2)
library(dplyr)
library(sf)
library(ggrepel)
library(scales)      # For alpha function
# library(rnaturalearth)  # Not using this due to dependency issues

# Define HEAT contributing countries
heat_countries <- c(
  "South Africa", 
  "Zimbabwe", 
  "Nigeria", 
  "Ghana", 
  "Cameroon", 
  "Benin", 
  "Burkina Faso", 
  "Côte d'Ivoire",
  "Mauritania", 
  "Ethiopia", 
  "Kenya", 
  "Uganda", 
  "Tanzania", 
  "Malawi", 
  "Mozambique", 
  "Zambia", 
  "Rwanda", 
  "Botswana",
  "Dem. Rep. Congo",
  "Sierra Leone", 
  "Lesotho", 
  "Senegal",
  "Namibia"
)

# Read the real data (tab-delimited)
partners_data <- read.delim("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned.csv", 
                           sep = "\t", stringsAsFactors = FALSE)

# Calculate total projects for each institution
df <- partners_data %>%
  mutate(
    total_projects = CHAMNHA + HEAT + ENBEL + GHAP + HAPI + BioHEAT + HIGH_Horizons,
    primary_project = case_when(
      Funder == 1 ~ "Funding Agency",
      CHAMNHA == 1 ~ "CHAMNHA",
      HEAT == 1 ~ "HE²AT",
      ENBEL == 1 ~ "ENBEL",
      GHAP == 1 ~ "GHAP",
      HAPI == 1 ~ "HAPI",
      BioHEAT == 1 ~ "BioHEAT",
      HIGH_Horizons == 1 ~ "HIGH Horizons",
      TRUE ~ "Multiple Projects"
    ),
    # Label only major partners
    is_major_partner = total_projects >= 2 |  # Show institutions with 2+ projects
      Funder == 1 |                          # Show all funders
      HEAT == 1                              # Show all HE²AT partners
  )

# Define project colors - matching the image
project_colors <- c(
  'Funding Agency' = '#666666',   # Gray
  'CHAMNHA' = '#3CAEA3',          # Teal
  'HE²AT' = '#F6511D',            # Orange
  'ENBEL' = '#FFB400',            # Yellow
  'GHAP' = '#7A5195',             # Purple
  'HAPI' = '#00A6ED',             # Blue
  'BioHEAT' = '#ED254E',          # Red
  'HIGH Horizons' = '#73D2DE',    # Light blue
  'Multiple Projects' = '#444444' # Dark gray
)

# Convert to sf object for partners
partners_sf <- df %>%
  filter(!is.na(lon) & !is.na(lat)) %>%  # Remove rows with missing coordinates
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Try to get world country data from a local file if it exists
world_sf_file <- "world_sf.rds"
if (file.exists(world_sf_file)) {
  world <- readRDS(world_sf_file)
  # Add HEAT country information
  world <- world %>%
    mutate(
      is_heat_country = name %in% heat_countries,
      fill_color = ifelse(is_heat_country, "#FFFAC8", "gray80")  # Yellow for HEAT countries
    )
  cat("Loaded world country data from local file\n")
} else {
  # Create a simple world map with basic continent outlines
  cat("Creating simple world map...\n")
  
  # Create a simple world outline
  world_coords <- list(
    rbind(
      c(-180, -90),
      c(180, -90),
      c(180, 90),
      c(-180, 90),
      c(-180, -90)
    )
  )
  
  world <- st_polygon(world_coords) %>%
    st_sfc(crs = 4326) %>%
    st_sf() %>%
    mutate(
      name = "World",
      continent = "World",
      is_heat_country = FALSE,
      fill_color = "gray80"
    )
  
  # Create a simple Africa outline
  africa_coords <- list(
    rbind(
      c(-20, -40),
      c(55, -40),
      c(55, 40),
      c(-20, 40),
      c(-20, -40)
    )
  )
  
  africa <- st_polygon(africa_coords) %>%
    st_sfc(crs = 4326) %>%
    st_sf() %>%
    mutate(
      name = "Africa",
      continent = "Africa",
      is_heat_country = TRUE,
      fill_color = "#FFFAC8"
    )
  
  # Create a simple Europe outline
  europe_coords <- list(
    rbind(
      c(-12, 34),
      c(42, 34),
      c(42, 67),
      c(-12, 67),
      c(-12, 34)
    )
  )
  
  europe <- st_polygon(europe_coords) %>%
    st_sfc(crs = 4326) %>%
    st_sf() %>%
    mutate(
      name = "Europe",
      continent = "Europe",
      is_heat_country = FALSE,
      fill_color = "gray80"
    )
  
  # Combine them
  world <- rbind(africa, europe)
}

# Filter for African countries
africa <- world %>%
  filter(continent == "Africa" | name == "Africa")

# Filter partners for Africa
africa_partners <- partners_sf %>% 
  filter(Country %in% c(heat_countries, 
                       "Egypt", "Algeria", "Tunisia", "Morocco", "Libya",
                       "Chad", "Mali", "Niger", "Sudan", "South Sudan",
                       "Eritrea", "Djibouti", "Somalia"))

# Create the Africa map
africa_map <- ggplot() +
  # Base map
  geom_sf(data = africa,
          aes(fill = fill_color),
          color = "gray40",
          linewidth = 0.3) +
  
  # Set the fill manually
  scale_fill_identity() +
  
  # Add partner points
  geom_sf(data = africa_partners,
          aes(size = total_projects,
              color = primary_project),
          alpha = 0.9,
          stroke = 1.2,
          shape = 21) +  # Filled circles with border
  
  # Add labels for major partners
  geom_text_repel(
    data = africa_partners %>% filter(is_major_partner),
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    fontface = "bold",
    box.padding = 0.6,
    point.padding = 0.4,
    force = 10,
    max.overlaps = 20,
    color = "black",
    min.segment.length = 0.2,
    segment.color = "gray50",
    segment.size = 0.2
  ) +
  
  # Customize colors for points
  scale_color_manual(
    values = project_colors,
    name = "Research Project",
    breaks = names(project_colors)
  ) +
  
  # Customize size scale
  scale_size_continuous(
    name = "Number of Projects",
    range = c(3, 10),
    breaks = c(1, 2, 3, 4, 5, 6, 7)
  ) +
  
  # Theme customization
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.box = "vertical",
    legend.box.background = element_rect(color = "gray80", fill = "white", linewidth = 0.3),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = "gray80", linewidth = 0.3),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Set map extent for Africa - adjusted based on memory
  coord_sf(
    xlim = c(-25, 50),  # Shifted left as per memory
    ylim = c(-40, 40),
    expand = FALSE
  )

# Filter European countries
europe <- world %>%
  filter(continent == "Europe" | name == "Europe")

# Filter partners for Europe
europe_partners <- partners_sf %>% 
  filter(Country %in% c(
    "United Kingdom", "France", "Germany", "Italy", "Spain", 
    "Netherlands", "Belgium", "Switzerland", "Sweden", "Norway", 
    "Finland", "Denmark", "Austria", "Ireland", "Portugal",
    "Greece", "Poland", "Czech Republic", "Hungary", "Romania"
  ))

# Create Europe inset map
europe_map <- ggplot() +
  # Base map
  geom_sf(data = europe,
          aes(fill = fill_color),
          color = "gray40",
          linewidth = 0.3) +
  
  # Set the fill manually
  scale_fill_identity() +
  
  # Add orange overlay for Europe - as seen in the image
  annotate("rect", 
           xmin = -12, xmax = 42, 
           ymin = 34, ymax = 67, 
           fill = alpha("#F6511D", 0.3),  # Orange with transparency
           color = NA) +
  
  # Add partner points
  geom_sf(data = europe_partners,
          aes(size = total_projects,
              color = primary_project),
          alpha = 0.9,
          stroke = 1.2,
          shape = 21) +
  
  # Add labels for European partners
  geom_text_repel(
    data = europe_partners,
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5,
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.3,
    force = 8,
    max.overlaps = 20,
    color = "black",
    min.segment.length = 0.2,
    segment.color = "gray50",
    segment.size = 0.2
  ) +
  
  # Customize colors for points
  scale_color_manual(
    values = project_colors,
    guide = "none"  # No legend for the inset
  ) +
  
  # Customize size scale
  scale_size_continuous(
    guide = "none"  # No legend for the inset
  ) +
  
  # Theme customization
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(1, 1, 1, 1)
  ) +
  
  # Set Europe extent
  coord_sf(
    xlim = c(-12, 42), 
    ylim = c(34, 67), 
    expand = FALSE
  )

# Create separate plot for the legend
legend_plot <- ggplot() +
  geom_point(data = data.frame(x = 1, y = 1:7, size = 1:7, type = "HEAT"), 
             aes(x = x, y = y, size = size, color = type)) +
  scale_size_continuous(
    name = "Number of Projects",
    range = c(3, 10),
    breaks = 1:7
  ) +
  scale_color_manual(
    name = "Research Project",
    values = c("HEAT" = project_colors["HE²AT"]),
    breaks = "HEAT",
    labels = "HE²AT"
  ) +
  theme_void() +
  theme(legend.position = "right")

# Create a simple grid with both maps
# Instead of using cowplot's draw_plot, save the maps separately
# Save Africa map with adjusted title
africa_map_titled <- africa_map +
  ggtitle("Wits Planetary Health's Research Partners",
         subtitle = "Research Partnerships in Africa and Europe") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 0.5,
                               margin = margin(t = 10))
  ) +
  labs(caption = paste0("Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | February 2025"))

# Save separate maps
ggsave("Wits_Planetary_Health_Africa_map.png", 
       africa_map_titled,
       width = 12, height = 8, dpi = 300,
       bg = "white")

# Save the Europe map
europe_map <- europe_map +
  ggtitle("European Partners") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

ggsave("Wits_Planetary_Health_Europe_map.png", 
       europe_map,
       width = 8, height = 8, dpi = 300,
       bg = "white")

# Print message
cat("\nMaps saved as:\n")
cat("1. Wits_Planetary_Health_Africa_map.png (Africa with title)\n")
cat("2. Wits_Planetary_Health_Europe_map.png (Europe only)\n")

# Print summary statistics
cat("\nSummary of Global Project Distribution:\n")
cat("====================================\n")
print(table(df$primary_project))

# Add a note explaining that a combined map requires cowplot or gridExtra
cat("\nNote: To create a combined map with insets, please install one of these packages:\n")
cat("1. gridExtra (install.packages('gridExtra'))\n")
cat("2. cowplot (install.packages('cowplot'))\n")
