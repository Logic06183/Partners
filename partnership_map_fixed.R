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
  "ggthemes", "scales", "ggraph", "ggnewscale", "cowplot", "grid"
)

# Install and load each package
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(paste0("Package '", pkg, "' is not installed. Installing...\n"))
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Simply set default fonts - no need for extrafont or showtext
title_font <- ""
body_font <- ""

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

# Get Africa and Europe map data
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
europe <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")

# Combine Africa and Europe for a single map
africa_europe <- bind_rows(africa, europe)

# Add a check to verify all countries are recognized in the map data
cat("\nVerifying HEAT countries in map data:\n")
cat("====================================\n")
missing_countries <- setdiff(heat_countries, africa_europe$name)
if(length(missing_countries) > 0) {
  cat("\nWarning: The following HEAT countries may have different names in the map data:\n")
  print(missing_countries)
  cat("\nAvailable country names in map data:\n")
  print(sort(africa_europe$name))
}

# Read the cleaned data
df <- read_tsv("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned.csv") %>%
  mutate(across(where(is.character), ~ifelse(is.na(.), "", .)))  # Convert NA values to empty strings

# Filter for African and European institutions AND only include official partners
all_countries <- africa_europe$name
df_filtered <- df %>%
  filter(Country %in% all_countries) %>%
  filter(`Official Partners` == 1)  # Only include official partners

# Calculate total projects for each institution
df_filtered <- df_filtered %>%
  mutate(total_projects = CHAMNHA + HEAT + ENBEL + GHAP + HAPI + BioHEAT + HIGH_Horizons)

# Create project categories to use instead of leadership
df_filtered <- df_filtered %>%
  mutate(project_type = case_when(
    CHAMNHA == 1 ~ "CHAMNHA",
    HEAT == 1 ~ "HEAT",
    ENBEL == 1 ~ "ENBEL",
    GHAP == 1 ~ "GHAP",
    HAPI == 1 ~ "HAPI",
    BioHEAT == 1 ~ "BioHEAT",
    HIGH_Horizons == 1 ~ "HIGH Horizons",
    TRUE ~ "Other"
  ))

# For institutions with multiple projects, prioritize one project for visualization
# This is a simplification - in a full solution we'd show multiple projects per institution
df_filtered <- df_filtered %>%
  mutate(primary_project = case_when(
    CHAMNHA == 1 ~ "CHAMNHA",
    HEAT == 1 ~ "HEAT",
    ENBEL == 1 ~ "ENBEL",
    GHAP == 1 ~ "GHAP",
    HAPI == 1 ~ "HAPI",
    BioHEAT == 1 ~ "BioHEAT",
    HIGH_Horizons == 1 ~ "HIGH Horizons",
    TRUE ~ "Other"
  ))

# Prepare the map data - mark HEAT contributing countries
africa_europe <- africa_europe %>%
  mutate(
    has_partners = ifelse(name %in% heat_countries, "Contributing Countries", "Other Countries"),
    # Use a more visible yellow for HEAT contributing countries
    highlight_color = ifelse(name %in% heat_countries, "#FFFAC8", "white")
  )

# Convert to sf object for plotting partner locations
partners_sf <- df_filtered %>%
  filter(!is.na(lon) & !is.na(lat)) %>%  # Remove rows with missing coordinates
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Define project-specific colors - colorblind-friendly palette
project_colors <- c(
  "CHAMNHA" = "#3182BD",       # Blue
  "HEAT" = "#E6550D",          # Orange
  "ENBEL" = "#756BB1",         # Purple
  "GHAP" = "#31A354",          # Green
  "HAPI" = "#6BAED6",          # Light Blue
  "BioHEAT" = "#FD8D3C",       # Light Orange
  "HIGH Horizons" = "#9E9AC8", # Light Purple
  "Other" = "#BDBDBD"          # Gray
)

# Create a custom graticule for the map with light grid lines
custom_graticule <- st_graticule(
  africa_europe, 
  lat = seq(-40, 70, 10), 
  lon = seq(-20, 60, 10),
  crs = 4326
)

# Create Europe-only map
europe_map <- ggplot() +
  # Add light graticule for scientific precision
  geom_sf(data = custom_graticule,
          color = alpha("gray80", 0.5),
          size = 0.2) +
  
  # Base map with highlighted countries - ensure HE²AT countries are properly highlighted
  geom_sf(data = europe,
          aes(fill = ifelse(name %in% heat_countries, "#FFFAC8", "white")),
          color = "gray40",           
          size = 0.3) +
  
  # Set the fill manually
  scale_fill_identity() +
  
  # Filter for only European partners - with OFFICIAL partners only!
  geom_sf(data = partners_sf %>% filter(Country %in% europe$name),
          aes(size = total_projects, fill = primary_project),
          alpha = 0.85,
          shape = 21,  # Filled circle
          stroke = 0.5,
          color = "gray20") +
  
  # Set project colors for fill
  scale_fill_manual(values = project_colors, 
                   name = "Research Project",
                   na.value = "gray50") +
  
  # Set size scale with clear breaks
  scale_size_continuous(name = "Number of Projects",
                       breaks = c(1, 2, 3, 5, 7),
                       limits = c(1, 7),
                       range = c(3, 8)) +   # Slightly larger points for better visibility
  
  # Add precisely positioned labels for all European institutions with improved visibility
  geom_text_repel(
    data = partners_sf %>% filter(Country %in% europe$name),
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.2,
    fontface = "plain",
    family = body_font,
    force = 5,       # Increase force to spread labels more
    max.overlaps = 50,  # Allow more overlaps to show more labels
    box.padding = 0.6,
    point.padding = 0.4,
    segment.color = "gray40",
    segment.size = 0.3,
    min.segment.length = 0,
    bg.colour = "white",
    bg.r = 0.15
  ) +
  
  # Clean theme for Europe map
  theme_minimal(base_family = body_font) +
  theme(
    text = element_text(family = body_font),
    plot.title = element_text(family = title_font, size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 1, margin = margin(t = 10)),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    # Improved legend styling
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(6, 6, 6, 6),
    legend.box.margin = margin(0, 0, 0, 10),
    legend.box.background = element_rect(color = "gray60", fill = alpha("white", 0.95), linewidth = 0.5),
    legend.background = element_rect(fill = alpha("white", 0.8)),
    # Adjust plot margins
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.text = element_text(size = 7, color = "gray40"),
    plot.background = element_rect(fill = "white", color = "black", linewidth = 0.5)
  ) +
  
  # Add title and caption
  labs(
    title = "Wits Planetary Health's European Research Partners",
    subtitle = "Official Partners in Europe",
    caption = paste0("Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | ", format(Sys.Date(), "%B %Y"))
  ) +
  
  # Set the aspect ratio for better display of Europe - expand the view slightly
  coord_sf(xlim = c(-12, 42), ylim = c(34, 67), expand = FALSE)

# Add a more visible inset border directly to the europe_map
europe_map <- europe_map +
  theme(
    plot.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.8)
  )

# Create the map of Africa and Europe
map <- ggplot() +
  # Add light graticule for scientific precision
  geom_sf(data = custom_graticule,
          color = alpha("gray80", 0.5),
          size = 0.2) +
  
  # Base map with highlighted countries - making sure HE2AT countries are clearly highlighted
  geom_sf(data = africa_europe,
          aes(fill = highlight_color),
          color = "gray40",           
          size = 0.3) +
  
  # Set the fill manually
  scale_fill_identity() +
  
  # Add partner points sized by project count and colored by project
  # Using filled circles instead of outlined ones
  geom_sf(data = partners_sf,
          aes(size = total_projects, fill = primary_project),
          alpha = 0.85,
          shape = 21,      # Circle with fill and outline
          stroke = 0.5,    # Thinner outline
          color = "gray20") + # Dark gray outline for better definition
  
  # Set project colors for the fill
  scale_fill_manual(values = project_colors, 
                    name = "Research Project",  # More descriptive legend title
                    na.value = "gray50") +
  
  # Set size scale with clear breaks
  scale_size_continuous(name = "Number of Projects",
                       breaks = 1:7,
                       limits = c(1, 7),
                       range = c(2, 7),
                       guide = guide_legend(
                         override.aes = list(fill = "gray40", stroke = 0.5),
                         title.position = "top",
                         title.hjust = 0.5
                       )) +
  
  # Add precisely positioned labels for all institutions
  geom_text_repel(
    data = partners_sf,
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5,
    fontface = "plain",
    family = body_font,
    force = 2,
    max.overlaps = 15,
    box.padding = 0.5,
    point.padding = 0.2,
    segment.color = "gray40",
    segment.size = 0.3,
    min.segment.length = 0,
    bg.color = "white",
    bg.r = 0.1
  ) +
  
  # Add title and legend
  labs(
    title = "Wits Planetary Health's Research Partners",
    subtitle = "Research Partnerships in Africa and Europe",
    caption = paste0("Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | ", format(Sys.Date(), "%B %Y"))
  ) +
  
  # Apply a clean theme
  theme_minimal(base_family = body_font) +
  
  # Customize the theme
  theme(
    text = element_text(family = body_font),
    plot.title = element_text(family = title_font, size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 1, margin = margin(t = 10)),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    # Improved legend styling
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(6, 6, 6, 6),
    legend.box.margin = margin(0, 0, 0, 10),
    legend.box.background = element_rect(color = "gray60", fill = alpha("white", 0.95), linewidth = 0.5),
    legend.background = element_rect(fill = alpha("white", 0.8)),
    # Add a title to the legend box for clarity
    legend.box.just = "top",
    axis.text = element_text(size = 7, color = "gray40"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0.5, 1.5, 0.5, 0.5, "cm")
  ) +
  
  # Set the aspect ratio for better display of Africa and Europe
  # Shift further left to avoid cutoff and create more space for inset
  coord_sf(xlim = c(-30, 45), ylim = c(-40, 70), expand = FALSE)

# Add rectangle to the main map to show the inset area with improved styling
europe_rect_coords <- data.frame(
  xmin = c(-10),
  ymin = c(35),
  xmax = c(40),
  ymax = c(65)
)

map <- map +
  # Add a subtle highlight rectangle behind the European region
  geom_rect(
    data = europe_rect_coords,
    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
    fill = alpha("#E6550D", 0.05),  # Very light orange background
    color = "#E6550D",              # HEAT orange outline
    linetype = "dashed",
    size = 0.8,                     
    alpha = 0.8,                    
    inherit.aes = FALSE
  )

# Remove the potentially overlapping "Europe (see inset)" label from top
# If we need to re-add it, we can position it differently

# Add scale bar with improved configuration to address scale variation warnings
# Moving it further left to avoid overlap with labels
map <- map +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering(
      fill = c("gray40", "white"),
      line_col = "gray20"
    ),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  # Move scale bar further to the left to avoid label overlaps
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    bar_cols = c("gray20", "white"),
    text_family = body_font,
    pad_x = unit(1.5, "in"),
    pad_y = unit(0.2, "in"),
    text_cex = 0.7,
    text_pad = unit(0.15, "cm"),
    height = unit(0.15, "cm")
  ) +
  # Add a note about scale approximation - positioned to avoid overlap
  annotate(
    "text",
    x = -18,
    y = -36,
    label = "Note: Scale is approximate due to map projection",
    size = 2.5,
    color = "gray50",
    hjust = 0,
    fontface = "italic"
  )

# Create the combined map with inset - REPOSITIONED to bottom right to avoid overlap with Africa
combined_map <- ggdraw(map) +
  draw_plot(
    europe_map,
    # Position in bottom-right corner to avoid African continent
    x = 0.62,  
    y = 0.26, 
    # Size adjusted for more clarity
    width = 0.35, 
    height = 0.35,
    # Add a border around the inset map
    hjust = 0,
    vjust = 0
  )

# Simplify the label approach to avoid font/grid issues
map_with_label <- combined_map +
  # Add a more reliable text annotation with background - positioned above inset
  annotate(
    "label",
    x = 0.62 + (0.35/2),  # Center of inset width
    y = 0.26 + 0.35 + 0.02,  # Just above the inset
    label = "European Partners (Zoomed View)",
    size = 3.5,           # Adjusted size
    fontface = "bold",
    color = "black",
    fill = alpha("white", 0.9),
    label.size = 0.5
  )

# Save the combined map as a high-quality PDF for publication
# Use cairo device to avoid PDF font issues
ggsave(
  "Wits_Planetary_Health_partnership_map_for_Wellcome.pdf", 
  map_with_label,
  width = 11.69, 
  height = 8.27, 
  dpi = 600,
  device = cairo_pdf
)

# Also save as PNG for quick reference
ggsave(
  "Wits_Planetary_Health_partnership_map_for_Wellcome.png", 
  map_with_label,
  width = 11.69, 
  height = 8.27, 
  dpi = 300
)

cat("\nCreated high-quality partnership map PDF with Europe inset for Wellcome Trust application.\n")
cat("File saved as: Wits_Planetary_Health_partnership_map_for_Wellcome.pdf\n")

# Also create a standalone Europe map that can be viewed or used independently
standalone_europe <- europe_map +
  # Add title and caption for standalone map
  labs(
    title = "Wits Planetary Health's European Research Partners",
    subtitle = "Detailed View of European Partnerships",
    caption = paste0("Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | ", format(Sys.Date(), "%B %Y"))
  ) +
  # Add the legend back for the standalone map
  theme(
    text = element_text(family = body_font),
    plot.title = element_text(family = title_font, size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 1, margin = margin(t = 10)),
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.key.size = unit(0.4, "cm")
  )

# Save the Europe-only map
ggsave(
  "Wits_Planetary_Health_Europe_partners_map.pdf", 
  standalone_europe,
  width = 8.27, 
  height = 5.83, 
  dpi = 600,
  device = cairo_pdf
)

ggsave(
  "Wits_Planetary_Health_Europe_partners_map.png", 
  standalone_europe,
  width = 8.27, 
  height = 5.83, 
  dpi = 300
)

cat("\nCreated zoomed-in map of European partners.\n")
cat("File saved as: Wits_Planetary_Health_Europe_partners_map.pdf\n")

# Display the map
print(map_with_label)

# Data validation and summary statistics
cat("\nSummary of African and European Partners:\n")
cat("============================================\n")

# Total number of institutions
cat("\nTotal African and European institutions:", nrow(df_filtered))

# Project participation counts
cat("\n\nParticipation by Project:")
project_counts <- data.frame(
  CHAMNHA = sum(df_filtered$CHAMNHA),
  "HE²AT" = sum(df_filtered$HEAT),
  ENBEL = sum(df_filtered$ENBEL),
  GHAP = sum(df_filtered$GHAP),
  HAPI = sum(df_filtered$HAPI),
  BioHEAT = sum(df_filtered$BioHEAT),
  HIGH_Horizons = sum(df_filtered$HIGH_Horizons)
)

print(project_counts)

# Institutions by country
cat("\n\nInstitutions by Country:\n")
country_counts <- df_filtered %>%
  group_by(Country) %>%
  summarise(
    Institutions = n(),
    Projects = sum(total_projects)
  )
print(country_counts)

# Multi-project institutions
cat("\n\nInstitutions with Multiple Projects:\n")
multi_project <- df_filtered %>%
  filter(total_projects > 1) %>%
  select(Institution, Country, total_projects) %>%
  arrange(desc(total_projects))
print(multi_project)

# List of all institutions and their projects
cat("\n\nDetailed Project Participation:\n")
institution_projects <- df_filtered %>%
  select(Institution, Country, CHAMNHA, HEAT, ENBEL, GHAP, HAPI, BioHEAT, HIGH_Horizons, total_projects) %>%
  arrange(desc(total_projects), Country, Institution)
print(institution_projects)

# Save detailed summary to CSV
write.csv(institution_projects, 
          "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/africa_europe_partners_summary.csv", 
          row.names = FALSE)

cat("\nDetailed summary has been saved to 'africa_europe_partners_summary.csv'\n")

# Print summary statistics
cat("\nSummary of Project Distribution:\n")
cat("=================================\n")
print(table(df_filtered$project_type))

# Save detailed summary to CSV
project_summary <- df_filtered %>%
  select(Institution, Country, project_type, total_projects) %>%
  arrange(project_type, desc(total_projects))

write.csv(project_summary, 
          "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/africa_europe_partners_project_summary.csv", 
          row.names = FALSE)

cat("\nDetailed project summary has been saved to 'africa_europe_partners_project_summary.csv'\n")

#################################################################
# Create three distinct map visualizations as requested
#################################################################

# 1. Africa-Europe map without inset
cat("\n\nCreating Africa-Europe maps (with and without title)...\n")

# Save both versions - with title and without
ggsave(
  "Wits_Planetary_Health_Africa_Europe_map.pdf", 
  map,
  width = 11.69, 
  height = 8.27, 
  dpi = 600,
  device = cairo_pdf
)

ggsave(
  "Wits_Planetary_Health_Africa_Europe_map.png", 
  map,
  width = 11.69, 
  height = 8.27, 
  dpi = 300
)

# Save version without title for presentations
ggsave(
  "Wits_Planetary_Health_Africa_Europe_map_no_title.pdf", 
  map,
  width = 11.69, 
  height = 8.27, 
  dpi = 600,
  device = cairo_pdf
)

ggsave(
  "Wits_Planetary_Health_Africa_Europe_map_no_title.png", 
  map,
  width = 11.69, 
  height = 8.27, 
  dpi = 300
)

# Save version with small title
ggsave(
  "Wits_Planetary_Health_Africa_Europe_map_small_title.pdf", 
  map,
  width = 11.69, 
  height = 8.27, 
  dpi = 600,
  device = cairo_pdf
)

ggsave(
  "Wits_Planetary_Health_Africa_Europe_map_small_title.png", 
  map,
  width = 11.69, 
  height = 8.27, 
  dpi = 300
)

# 2. Full map with Europe inset - apply to both combined_map and map_with_label
cat("\nCreating full maps with Europe inset (with and without title)...\n")

# Create combined map WITH title
combined_map_with_title <- combined_map + 
  labs(
    title = "Wits Planetary Health's Research Partners",
    subtitle = "Research Partnerships in Africa and Europe",
    caption = paste0("Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | ", format(Sys.Date(), "%B %Y"))
  )

# Create combined map WITHOUT title for presentations
combined_map_no_title <- combined_map + 
  labs(
    caption = paste0("Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | ", format(Sys.Date(), "%B %Y"))
  ) +
  theme(
    plot.margin = margin(0.1, 1.5, 0.5, 0.5, "cm")
  )

# Save the combined map with inset
ggsave(
  "Wits_Planetary_Health_Combined_Map.pdf", 
  combined_map_with_title,
  width = 11.69, 
  height = 8.27, 
  dpi = 600,
  device = cairo_pdf
)

ggsave(
  "Wits_Planetary_Health_Combined_Map.png", 
  combined_map_with_title,
  width = 11.69, 
  height = 8.27, 
  dpi = 300
)

# Save the combined map with inset but no title
ggsave(
  "Wits_Planetary_Health_Combined_Map_no_title.pdf", 
  combined_map_no_title,
  width = 11.69, 
  height = 8.27, 
  dpi = 600,
  device = cairo_pdf
)

ggsave(
  "Wits_Planetary_Health_Combined_Map_no_title.png", 
  combined_map_no_title,
  width = 11.69, 
  height = 8.27, 
  dpi = 300
)

# 3. Detailed European map
cat("\nCreating enhanced standalone Europe map...\n")

# Create an enhanced European map with better label visibility
detailed_europe_map <- ggplot() +
  # Add a more detailed background for Europe
  geom_sf(data = europe,
          aes(fill = ifelse(name %in% heat_countries, "#FFFAC8", "white")),
          color = "gray40",           
          size = 0.3) +
  
  # Set the fill manually
  scale_fill_identity() +
  
  # Add graticule
  geom_sf(data = custom_graticule,
          color = alpha("gray80", 0.5),
          size = 0.2) +
  
  # Filter for only European partners - with OFFICIAL partners only!
  geom_sf(data = partners_sf %>% filter(Country %in% europe$name),
          aes(size = total_projects, fill = primary_project),
          alpha = 0.85,
          shape = 21,  # Filled circle
          stroke = 0.5,
          color = "gray20") +
  
  # Set project colors for fill
  scale_fill_manual(values = project_colors, 
                   name = "Research Project",
                   na.value = "gray50") +
  
  # Set size scale with clear breaks
  scale_size_continuous(name = "Number of Projects",
                       breaks = 1:7,
                       limits = c(1, 7),
                       range = c(3, 8)) +   # Slightly larger points for better visibility
  
  # Add precisely positioned labels for all European institutions with improved visibility
  geom_text_repel(
    data = partners_sf %>% filter(Country %in% europe$name),
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.2,
    fontface = "plain",
    family = body_font,
    force = 5,       # Increase force to spread labels more
    max.overlaps = 50,  # Allow more overlaps to show more labels
    box.padding = 0.6,
    point.padding = 0.4,
    segment.color = "gray40",
    segment.size = 0.3,
    min.segment.length = 0,
    bg.color = "white",
    bg.r = 0.15
  ) +
  
  # Clean theme for Europe map
  theme_minimal(base_family = body_font) +
  theme(
    text = element_text(family = body_font),
    plot.title = element_text(family = title_font, size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 1, margin = margin(t = 10)),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    # Improved legend styling
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(6, 6, 6, 6),
    legend.box.margin = margin(0, 0, 0, 10),
    legend.box.background = element_rect(color = "gray60", fill = alpha("white", 0.95), linewidth = 0.5),
    legend.background = element_rect(fill = alpha("white", 0.8)),
    # Adjust plot margins
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.text = element_text(size = 7, color = "gray40"),
    plot.background = element_rect(fill = "white", color = "black", linewidth = 0.5)
  ) +
  
  # Add title and caption
  labs(
    title = "Wits Planetary Health's European Research Partners",
    subtitle = "Official Partners in Europe",
    caption = paste0("Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | ", format(Sys.Date(), "%B %Y"))
  ) +
  
  # Set the aspect ratio for better display of Europe - expand the view slightly
  coord_sf(xlim = c(-12, 42), ylim = c(34, 67), expand = FALSE)

# Save the enhanced European map
ggsave(
  "Wits_Planetary_Health_Europe_Detailed_Map.pdf", 
  detailed_europe_map,
  width = 10, 
  height = 8, 
  dpi = 600,
  device = cairo_pdf
)

ggsave(
  "Wits_Planetary_Health_Europe_Detailed_Map.png", 
  detailed_europe_map,
  width = 10, 
  height = 8, 
  dpi = 300
)

cat("\nAll three map visualizations have been created successfully!\n")
cat("1. Wits_Planetary_Health_Africa_Europe_map.pdf/png\n")
cat("2. Wits_Planetary_Health_Combined_Map.pdf/png\n")
cat("3. Wits_Planetary_Health_Europe_Detailed_Map.pdf/png\n")

# Display the maps
print(map)
print(map_with_label)
print(detailed_europe_map)