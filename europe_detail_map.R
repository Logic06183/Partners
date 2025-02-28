# Europe Detailed Partners Map
# This script creates a detailed map showing institutional partners in Europe

# Load required packages
library(ggplot2)
library(dplyr)
library(sf)
library(ggrepel)
library(scales)

# Read the tab-delimited partner data - IMPORTANT: using tab delimiter
partners_data <- read.delim("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned.csv", 
                           sep = "\t", stringsAsFactors = FALSE)

# Print column names to debug
cat("Column names in CSV:", paste(colnames(partners_data), collapse=", "), "\n")

# Fix column names if needed 
if("Official.Partners" %in% colnames(partners_data)) {
  # Already correct format
  cat("Using 'Official.Partners' column\n")
} else if("Official Partners" %in% colnames(partners_data)) {
  cat("Converting 'Official Partners' to 'Official.Partners'\n")
  colnames(partners_data)[colnames(partners_data) == "Official Partners"] <- "Official.Partners"
} else {
  # If neither exists, create a default column (all institutions are partners)
  cat("WARNING: No 'Official Partners' column found, assuming all are partners\n")
  partners_data$Official.Partners <- 1
}

# List of European countries
european_countries <- c(
  "United Kingdom", "Germany", "France", "Italy", "Spain", "Netherlands", 
  "Belgium", "Portugal", "Sweden", "Denmark", "Norway", "Finland", 
  "Switzerland", "Austria", "Poland", "Czech Republic", "Ireland", 
  "Luxembourg", "Greece", "Hungary", "Romania", "Bulgaria", "Slovakia", 
  "Slovenia", "Croatia", "Estonia", "Latvia", "Lithuania", "Cyprus", "Malta"
)

# Filter for European partners - filter by country, ensuring required columns exist
europe_df <- partners_data %>%
  filter(Country %in% european_countries)

# Print a warning if no European partners are found
if(nrow(europe_df) == 0) {
  cat("WARNING: No European partners found in the data. Check the 'Country' column for European countries.\n")
  cat("European countries searched for:", paste(european_countries, collapse=", "), "\n")
  cat("Countries in data:", paste(unique(partners_data$Country), collapse=", "), "\n")
}

# Replace NA with 0 for project columns to avoid errors
project_columns <- c("CHAMNHA", "HEAT", "ENBEL", "GHAP", "HAPI", "BioHEAT", "HIGH_Horizons", "Funder")
for(col in project_columns) {
  if(col %in% colnames(europe_df)) {
    europe_df[[col]][is.na(europe_df[[col]])] <- 0
  } else {
    cat("WARNING: Column", col, "not found in data, creating with zeros\n")
    europe_df[[col]] <- 0
  }
}

# Calculate total projects for each institution
europe_df <- europe_df %>%
  mutate(
    # Sum all project columns, handling missing ones safely
    total_projects = rowSums(select(., all_of(intersect(project_columns, colnames(.)))), na.rm = TRUE),
    
    # Determine primary project
    primary_project = case_when(
      Funder == 1 ~ "Funding Agency",
      CHAMNHA == 1 ~ "CHAMNHA",
      HEAT == 1 ~ "ENBEL",  # Note: Using ENBEL color for HEAT per example image
      ENBEL == 1 ~ "ENBEL",
      GHAP == 1 ~ "GHAP",
      HIGH_Horizons == 1 ~ "HIGH Horizons",
      BioHEAT == 1 ~ "BioHEAT",
      TRUE ~ "Multiple Projects"
    ),
    
    # Label all European partners
    is_major_partner = TRUE
  )

# Define project colors - matching the example image
project_colors <- c(
  'Funding Agency' = 'grey50',
  'CHAMNHA' = '#00BFC4',      # Teal color from image
  'ENBEL' = '#F8766D',        # Yellow/orange from image
  'GHAP' = '#7CAE00',         # Green from image
  'BioHEAT' = '#F564E3',      # Pink from image
  'HIGH Horizons' = '#00B0F6', # Blue from image
  'Multiple Projects' = 'grey30'
)

# Print summary of European partners by project
cat("\nEuropean partners by project type:\n")
print(table(europe_df$primary_project))

# Convert to sf object for partners
europe_partners_sf <- europe_df %>%
  filter(!is.na(lon) & !is.na(lat)) %>%  # Remove rows with missing coordinates
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Print a message about missing coordinates
missing_euro_coords <- europe_df %>% filter(is.na(lon) | is.na(lat))
if(nrow(missing_euro_coords) > 0) {
  cat("\nWARNING: The following European partners are missing coordinates and will not be shown on the map:\n")
  print(missing_euro_coords$Institution)
}

# Create Europe outline
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
    fill_color = "#FFC19E"  # Light orange
  )

# Create the Europe detailed map
europe_map <- ggplot() +
  # Base map with orange overlay
  geom_sf(data = europe,
          fill = "#FFC19E",  # Light orange matching the example image
          color = "gray70",
          linewidth = 0.1) +
  
  # Add gridlines to match example image
  geom_hline(yintercept = seq(35, 65, by = 5), color = "gray80", linewidth = 0.2) +
  geom_vline(xintercept = seq(-10, 40, by = 10), color = "gray80", linewidth = 0.2) +
  
  # Add partner points
  geom_sf(data = europe_partners_sf,
          aes(size = total_projects,
              color = primary_project),
          alpha = 0.8) +
  
  # Add labels for all European partners
  geom_text_repel(
    data = europe_partners_sf,
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    fontface = "bold",
    box.padding = 0.8,
    point.padding = 0.5,
    force = 10,
    max.overlaps = 30,
    color = "black",
    min.segment.length = 0.1,
    segment.color = "gray50",
    segment.size = 0.2
  ) +
  
  # Add latitude labels
  annotate("text", x = -12, y = seq(35, 65, by = 5), 
           label = paste0(seq(35, 65, by = 5), "°N"), 
           size = 2.5, hjust = 0, color = "gray30") +
  
  # Add longitude labels
  annotate("text", y = 34, x = seq(-10, 40, by = 10), 
           label = c("10°W", "0°", "10°E", "20°E", "30°E", "40°E"), 
           size = 2.5, vjust = 1, color = "gray30") +
  
  # Customize colors to match the example image
  scale_color_manual(
    values = project_colors,
    name = "Research Project",
    breaks = names(project_colors)
  ) +
  
  # Customize size scale
  scale_size_continuous(
    name = "Number of Projects",
    range = c(3, 7),
    breaks = c(1, 2, 3),
    limits = c(1, 3)
  ) +
  
  # Set the legend title to match example
  guides(
    color = guide_legend(title = "primary_project", override.aes = list(size = 4)),
    size = guide_legend(title = "Number of Projects")
  ) +
  
  # Theme customization to match example image
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "right",
    legend.box.spacing = unit(0.2, "cm"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = NA, color = NA)
  ) +
  
  # Set map extent - focused on Europe
  coord_sf(
    xlim = c(-12, 40),
    ylim = c(35, 65),
    expand = FALSE
  ) +
  
  # Add title and caption
  ggtitle("European Research Partners",
         subtitle = "Detailed View of Wits Planetary Health's European Partnerships") +
  labs(caption = paste0("Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | February 2025"))

# Save the map
ggsave("Wits_Planetary_Health_Europe_Detailed.png", 
       europe_map,
       width = 10, height = 8, dpi = 300,
       bg = "white")

# Print summary statistics
cat("\nMap saved as: Wits_Planetary_Health_Europe_Detailed.png\n")
cat("\nTotal number of European partners:", nrow(europe_df))
