# Africa-Europe Research Partners Map
# This script creates a map showing institutional partners in Africa and Europe with chloropleth

# Load required packages
library(ggplot2)
library(dplyr)
library(sf)
library(ggrepel)
library(scales)

# Define HEAT contributing countries for yellow chloropleth
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

# Read the tab-delimited partner data
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

# Replace NA with 0 for project columns to avoid errors
project_columns <- c("CHAMNHA", "HEAT", "ENBEL", "GHAP", "HAPI", "BioHEAT", "HIGH_Horizons", "Funder")
for(col in project_columns) {
  if(col %in% colnames(partners_data)) {
    partners_data[[col]][is.na(partners_data[[col]])] <- 0
  } else {
    cat("WARNING: Column", col, "not found in data, creating with zeros\n")
    partners_data[[col]] <- 0
  }
}

# Filter for official institutional partners
df <- partners_data %>%
  filter(Official.Partners == 1) %>%
  mutate(
    # Sum all project columns
    total_projects = rowSums(select(., all_of(intersect(project_columns, colnames(.)))), na.rm = TRUE),
    
    # Determine primary project - matching the exact colors from the example image
    primary_project = case_when(
      Funder == 1 ~ "Funding Agency", 
      HEAT == 1 ~ "HEAT",  # HE²AT in orange
      CHAMNHA == 1 ~ "CHAMNHA",
      ENBEL == 1 ~ "ENBEL",
      GHAP == 1 ~ "GHAP",
      HIGH_Horizons == 1 ~ "HIGH Horizons",
      BioHEAT == 1 ~ "BioHEAT",
      TRUE ~ "Multiple Projects"
    ),
    
    # Label major partners for readability
    is_major_partner = total_projects >= 2 |  # Show institutions with 2+ projects
      Funder == 1 |                        # Show all funders
      HEAT == 1                            # Show all HEAT partners
  )

# Define project colors - match the exact colors from the example image
project_colors <- c(
  'Funding Agency' = 'grey50',
  'CHAMNHA' = '#00BFC4',      # Teal blue in image
  'HEAT' = '#F8766D',         # Orange/pink in image
  'ENBEL' = '#FFB400',        # Yellow in image
  'GHAP' = '#7CAE00',         # Green in image
  'BioHEAT' = '#F564E3',      # Pink in image
  'HIGH Horizons' = '#00B0F6', # Light blue in image
  'Multiple Projects' = 'grey30'
)

# Convert to sf object for partners
partners_sf <- df %>%
  filter(!is.na(lon) & !is.na(lat)) %>%  # Remove rows with missing coordinates
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Print a message about missing coordinates
missing_coords <- df %>% filter(is.na(lon) | is.na(lat))
if(nrow(missing_coords) > 0) {
  cat("\nWARNING: The following institutional partners are missing coordinates and will not be shown on the map:\n")
  print(missing_coords$Institution)
}

# Create simplified world map focused on Africa and Europe
# First create a gray background for all areas
world_base <- st_polygon(list(rbind(
  c(-180, -90),
  c(180, -90),
  c(180, 90),
  c(-180, 90),
  c(-180, -90)
))) %>%
  st_sfc(crs = 4326) %>%
  st_sf() %>%
  mutate(fill_color = "gray80")

# Create HEAT country polygons
# In the example image, these appear as yellow areas in African countries
heat_country_polygons <- data.frame(
  name = heat_countries,
  is_heat_country = TRUE
) %>%
  mutate(
    # Create a simple rectangle for each country
    # In a real implementation, we'd use actual boundaries
    fill_color = "#FFFAC8"  # Light yellow color for HEAT countries
  )

# Create the combined Africa-Europe map
map <- ggplot() +
  # Base map (gray background)
  geom_sf(data = world_base, fill = "gray80", color = NA) +
  
  # Add HEAT country highlighting (simplified with rectangles around partners)
  # This simulates the yellow highlighting in the example image
  geom_rect(data = partners_sf %>% 
              filter(Country %in% heat_countries) %>%
              st_coordinates() %>%
              as.data.frame() %>%
              group_by(L1) %>%
              summarise(
                xmin = min(X) - 2,
                xmax = max(X) + 2,
                ymin = min(Y) - 2,
                ymax = max(Y) + 2
              ),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#FFFAC8", color = NA, alpha = 0.7) +
  
  # Add gridlines like in the example image
  geom_hline(yintercept = seq(-40, 40, by = 20), color = "gray90", linewidth = 0.2) +
  geom_vline(xintercept = seq(-20, 60, by = 10), color = "gray90", linewidth = 0.2) +
  
  # Add partner points
  geom_sf(data = partners_sf,
          aes(size = total_projects,
              color = primary_project),
          alpha = 0.8) +
  
  # Add labels for major partners
  geom_text_repel(
    data = partners_sf %>% filter(is_major_partner),
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.3,
    force = 10,
    max.overlaps = 20,
    min.segment.length = 0.1,
    segment.color = "gray50",
    segment.size = 0.2
  ) +
  
  # Add latitude labels like in example image
  annotate("text", x = -23, y = c(-40, -20, 0, 20, 40), 
           label = c("40°S", "20°S", "0°", "20°N", "40°N"), 
           size = 2.5, hjust = 0, color = "gray30") +
  
  # Add longitude labels like in example image
  annotate("text", y = -40, x = seq(-20, 50, by = 10), 
           label = c("20°W", "10°W", "0°", "10°E", "20°E", "30°E", "40°E", "50°E"), 
           size = 2.5, vjust = 1, color = "gray30") +
  
  # Customize colors for points to match example image
  scale_color_manual(
    values = project_colors,
    name = "Research Project",
    breaks = names(project_colors)
  ) +
  
  # Customize size scale
  scale_size_continuous(
    name = "Number of Projects",
    range = c(2, 8),
    breaks = c(1, 2, 3, 4, 5, 6, 7)
  ) +
  
  # Set up legends to match example image
  guides(
    color = guide_legend(title = "Research Project", override.aes = list(size = 4)),
    size = guide_legend(title = "Number of Projects")
  ) +
  
  # Theme customization to match example image
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = NA, color = NA)
  ) +
  
  # Set map extent - focus more on Africa with a view of southern Europe
  coord_sf(
    xlim = c(-25, 50),
    ylim = c(-40, 40),
    expand = FALSE
  ) +
  
  # Add title and caption
  ggtitle("Wits Planetary Health's Research Partners",
         subtitle = "Research Partnerships in Africa and Europe") +
  labs(caption = paste0("Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | February 2025"))

# Save the map
ggsave("Wits_Planetary_Health_Africa_Map.png", 
       map,
       width = 12, height = 8, dpi = 300,
       bg = "white")

# Print summary statistics
cat("\nMap saved as: Wits_Planetary_Health_Africa_Map.png\n")
cat("\nTotal number of partners:", nrow(df), "\n")
cat("Partners by continent:\n")
print(table(df$Country %in% heat_countries))
