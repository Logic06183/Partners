# Wits Planetary Health Partnership Map
# This script creates a map showing research partnerships in Africa and Europe

# Load required packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(viridis)
library(RColorBrewer)

# Read the data using the absolute path
partners_data <- read.csv("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned_with_short_names.csv", 
                         stringsAsFactors = FALSE)

# Print column names to verify structure
cat("Column names in CSV:", paste(names(partners_data), collapse = ", "), "\n")

# Check if Short_Name already exists
if(!"Short_Name" %in% names(partners_data)) {
  # Add shortened institution names if not already present
  partners_data <- partners_data %>%
    mutate(
      Short_Name = case_when(
        grepl("University", Institution) ~ gsub("University of |University|Universität", "Univ.", Institution),
        grepl("Institute", Institution) ~ gsub("Institute of |Institute", "Inst.", Institution),
        TRUE ~ Institution
      ),
      # Further shorten names if they're still too long
      Short_Name = ifelse(nchar(Short_Name) > 25, paste0(substr(Short_Name, 1, 22), "..."), Short_Name)
    )
}

# Filter for official partners only
partners_data <- partners_data %>%
  filter(Official.Partners == 1)

# Calculate total projects if not already present
if(!"total_projects" %in% names(partners_data)) {
  partners_data <- partners_data %>%
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
      )
    )
}

# Print summary
cat("Number of official partners:", nrow(partners_data), "\n")
cat("Number of project involvements:", sum(partners_data$total_projects, na.rm=TRUE), "\n")

# Define HEAT countries - updated list adding Malawi and using ISO code for DRC
heat_countries <- c(
  # West Africa
  "Senegal", "Mauritania", "Burkina Faso", "Côte d'Ivoire", "Ghana", 
  "Benin", "Nigeria", "Cameroon",
  
  # Central Africa
  "Central African Republic", "South Sudan", "Ethiopia", 
  "Democratic Republic of the Congo", "DRC", "Congo, Dem. Rep.", "COD",  # Added ISO code for DRC
  
  # East Africa
  "Uganda", "Rwanda", "Burundi", "Kenya", "Tanzania", 
  
  # Southern Africa
  "Namibia", "Botswana", "Zimbabwe", "South Africa", "Zambia", "Mozambique", "Malawi"  # Added Malawi
)

cat("HEAT contributing countries:", paste(heat_countries, collapse = ", "), "\n")

# Convert to sf object - explicitly keep all columns
partners_sf <- partners_data %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Print to verify Short_Name column exists in partners_sf
cat("Checking if Short_Name exists in partners_sf: ", 
    ifelse("Short_Name" %in% names(partners_sf), "Yes, it exists!", "No, it's missing!"), "\n")

# Create separate datasets for Africa and Europe partners
africa_partners <- partners_sf %>% 
  filter(Country %in% c(
    # African countries only
    "Burundi", "Cameroon", "Côte d'Ivoire", "Mauritania", "Nigeria", "Rwanda", 
    "Senegal", "South Africa", "Togo", "Uganda", "Zimbabwe",
    "Egypt", "Algeria", "Tunisia", "Morocco", "Libya", "Chad", "Mali", 
    "Niger", "Sudan", "South Sudan", "Eritrea", "Djibouti", "Somalia", 
    "Kenya", "Tanzania", "Ethiopia", "Botswana", "Namibia", "Zambia", 
    "Malawi", "Mozambique", "Angola", "DRC", "Congo", "Gabon",
    "Equatorial Guinea", "Central African Republic", "Ghana",
    "Benin", "Burkina Faso", "Gambia", "Guinea", "Guinea-Bissau",
    "Liberia", "Sierra Leone", "Lesotho", "Eswatini", "Madagascar"
  )) 

# Double-check to make sure no US partners are included
africa_partners <- africa_partners %>%
  filter(Country != "United States")

# Print to verify Short_Name column exists in africa_partners
cat("Checking if Short_Name exists in africa_partners: ", 
    ifelse("Short_Name" %in% names(africa_partners), "Yes, it exists!", "No, it's missing!"), "\n")

europe_partners <- partners_sf %>% 
  filter(Country %in% c(
    "United Kingdom", "France", "Germany", "Italy", "Spain", 
    "Netherlands", "Belgium", "Switzerland", "Sweden", "Norway", 
    "Finland", "Denmark", "Austria", "Ireland", "Portugal",
    "Greece", "Poland", "Czech Republic", "Hungary", "Romania",
    "Estonia", "Latvia", "Lithuania", "Slovakia", "Slovenia",
    "Croatia", "Bulgaria", "Cyprus", "Malta", "Luxembourg"
  ))

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create a version of the world map with HEAT countries highlighted
# Use a direct approach with a simple ifelse
world$is_heat_country <- world$name %in% heat_countries | world$iso_a3 %in% c("COD")

# Print to check if any HEAT countries were matched
cat("Number of HEAT countries matched in world map: ", 
    sum(world$is_heat_country), "\n")

# Define project colors
project_colors <- c(
  "Funding Agency" = "gray50",
  "CHAMNHA" = "#66C2A5",
  "HE²AT" = "#FC8D62",
  "ENBEL" = "#FFD92F",
  "GHAP" = "#8DA0CB",
  "HAPI" = "#A6D854",
  "BioHEAT" = "#E78AC3",
  "HIGH Horizons" = "#8DD3C7",
  "Multiple Projects" = "#BEBADA"
)

# Common theme settings to remove axis labels for all maps
remove_axes_theme <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank()
)

# 1. OVERVIEW MAP (Africa-Europe without labels)
overview_map <- ggplot() +
  # Base map with country outlines - highlight HEAT countries in yellow
  geom_sf(data = world,
          aes(fill = is_heat_country),
          color = "gray70", 
          linewidth = 0.2) +
  
  # Set fill colors for the country types
  scale_fill_manual(
    values = c("TRUE" = "#FFFAC8", "FALSE" = "gray95"),
    name = "Country Type",
    labels = c("TRUE" = "HE²AT Contributing Country"),
    guide = guide_legend(order = 1),  # Make this legend appear first
    na.value = "gray95",
    breaks = c("TRUE")  # Only include TRUE in the legend
  ) +
  
  # Add partner points
  geom_sf(data = partners_sf,
          aes(size = total_projects, color = primary_project),
          alpha = 0.9) +
  
  # Customize colors for points
  scale_color_manual(
    values = project_colors,
    name = "Research Project",
    guide = guide_legend(order = 2)  # Make this legend appear second
  ) +
  
  # Customize size scale
  scale_size_continuous(
    name = "Number of Projects",
    range = c(2, 8),
    breaks = c(1, 2, 3, 4, 5, 6, 7),
    guide = guide_legend(order = 3)  # Make this legend appear third
  ) +
  
  # Theme customization
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  remove_axes_theme +  # Apply the common theme to remove axes
  
  # Set map extent to show Africa and Europe
  coord_sf(
    xlim = c(-20, 50), 
    ylim = c(-40, 70), 
    expand = FALSE
  ) +
  
  # Add title
  ggtitle("Wits Planetary Health's Research Partners",
         subtitle = "Research Partnerships in Africa and Europe") +
  labs(caption = "Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | February 2025")

# 2. AFRICA DETAILED MAP (with labels)
africa_map <- ggplot() +
  # Base map with country outlines - highlight HEAT countries in yellow
  geom_sf(data = world,
          aes(fill = is_heat_country),
          color = "gray70", 
          linewidth = 0.2) +
  
  # Set fill colors for the country types
  scale_fill_manual(
    values = c("TRUE" = "#FFFAC8", "FALSE" = "gray95"),
    name = "Country Type",
    labels = c("TRUE" = "HE²AT Contributing Country"),
    guide = guide_legend(order = 1),  # Make this legend appear first
    na.value = "gray95",
    breaks = c("TRUE")  # Only include TRUE in the legend
  ) +
  
  # Add partner points
  geom_sf(data = africa_partners,  # Using filtered dataset without US partners
          aes(size = total_projects, color = primary_project),
          alpha = 0.9) +
  
  # Add labels for all African partners
  geom_text_repel(
    data = africa_partners,  # Using filtered dataset without US partners
    aes(label = Short_Name, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.0,
    fontface = "bold",
    box.padding = 0.6,
    point.padding = 0.4,
    force = 10,
    max.overlaps = 30,
    bg.colour = "white",
    bg.r = 0.15
  ) +
  
  # Customize colors for points
  scale_color_manual(
    values = project_colors,
    name = "Research Project",
    guide = guide_legend(order = 2)  # Make this legend appear second
  ) +
  
  # Customize size scale
  scale_size_continuous(
    name = "Number of Projects",
    range = c(2, 8),
    breaks = c(1, 2, 3, 4, 5, 6, 7),
    guide = guide_legend(order = 3)  # Make this legend appear third
  ) +
  
  # Theme customization
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  remove_axes_theme +  # Apply the common theme to remove axes
  
  # Set map extent to show Africa
  coord_sf(
    xlim = c(-20, 50), 
    ylim = c(-40, 40), 
    expand = FALSE
  ) +
  
  # Add title
  ggtitle("Wits Planetary Health's African Research Partners",
         subtitle = "Detailed View of African Partnerships") +
  labs(caption = "Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | February 2025")

# 3. EUROPE DETAILED MAP (with labels)
europe_map <- ggplot() +
  # Base map with country outlines
  geom_sf(data = world, fill = "gray95", color = "gray70", linewidth = 0.2) +
  
  # Add partner points
  geom_sf(data = europe_partners,
          aes(size = total_projects, color = primary_project),
          alpha = 0.9) +
  
  # Add labels for all European partners
  geom_text_repel(
    data = europe_partners,
    aes(label = Short_Name, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.0,
    fontface = "bold",
    box.padding = 0.6,
    point.padding = 0.4,
    force = 10,
    max.overlaps = 30,
    bg.colour = "white",
    bg.r = 0.15
  ) +
  
  # Customize colors for points
  scale_color_manual(
    values = project_colors,
    name = "Research Project",
    guide = guide_legend(order = 2)  # Make this legend appear second
  ) +
  
  # Customize size scale
  scale_size_continuous(
    name = "Number of Projects",
    range = c(2, 8),
    breaks = c(1, 2, 3, 4, 5, 6, 7),
    guide = guide_legend(order = 3)  # Make this legend appear third
  ) +
  
  # Theme customization
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  remove_axes_theme +  # Apply the common theme to remove axes
  
  # Set map extent to show Europe
  coord_sf(
    xlim = c(-10, 40), 
    ylim = c(35, 70), 
    expand = FALSE
  ) +
  
  # Add title
  ggtitle("Wits Planetary Health's European Research Partners",
         subtitle = "Detailed View of European Partnerships") +
  labs(caption = "Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | February 2025")

# Save all three maps as PDFs
ggsave("Wits_Planetary_Health_Overview_Map.pdf", 
       overview_map,
       width = 10, height = 8, dpi = 300,
       bg = "white")

ggsave("Wits_Planetary_Health_Africa_Detailed_Map.pdf", 
       africa_map,
       width = 10, height = 8, dpi = 300,
       bg = "white")

ggsave("Wits_Planetary_Health_Europe_Detailed_Map.pdf", 
       europe_map,
       width = 10, height = 8, dpi = 300,
       bg = "white")

# Also save as PNGs for quick reference
ggsave("Wits_Planetary_Health_Overview_Map.png", 
       overview_map,
       width = 10, height = 8, dpi = 300,
       bg = "white")

ggsave("Wits_Planetary_Health_Africa_Detailed_Map.png", 
       africa_map,
       width = 10, height = 8, dpi = 300,
       bg = "white")

ggsave("Wits_Planetary_Health_Europe_Detailed_Map.png", 
       europe_map,
       width = 10, height = 8, dpi = 300,
       bg = "white")

cat("\nMaps saved as:\n")
cat("1. Wits_Planetary_Health_Overview_Map.pdf/png (Africa-Europe overview without labels)\n")
cat("2. Wits_Planetary_Health_Africa_Detailed_Map.pdf/png (Africa with all labels)\n")
cat("3. Wits_Planetary_Health_Europe_Detailed_Map.pdf/png (Europe with all labels)\n")

# Print summary of project distribution
cat("\nSummary of Global Project Distribution:\n")
cat("====================================\n")
print(table(partners_data$primary_project))

# Add a note about the yellow highlighting
cat("\nNote: HE²AT partner countries are highlighted in yellow on the maps.\n")

