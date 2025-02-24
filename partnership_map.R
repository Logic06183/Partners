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

# Install and load required packages individually
required_packages <- c(
  "tidyverse",    # for data manipulation
  "sf",           # for spatial data
  "rnaturalearth",# for country boundaries
  "rnaturalearthdata",
  "ggspatial",    # for north arrow
  "viridis",      # for color palettes
  "showtext",     # for custom fonts
  "RColorBrewer", # for color palettes
  "ggrepel",      # for text label positioning
  "ggthemes",     # for scientific themes
  "scales",       # for better scale formatting
  "ggraph"        # for network visualization
)

# Install and load each package
for (pkg in required_packages) {
  install_and_load(pkg)
}

# Load Google Fonts
font_add_google("Montserrat", "montserrat")
font_add_google("Open Sans", "opensans")
showtext_auto()

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

# Add a check to verify all countries are recognized in the map data
cat("\nVerifying HEAT countries in map data:\n")
cat("====================================\n")
missing_countries <- setdiff(heat_countries, africa$name)
if(length(missing_countries) > 0) {
  cat("\nWarning: The following HEAT countries may have different names in the map data:\n")
  print(missing_countries)
  cat("\nAvailable country names in map data:\n")
  print(sort(africa$name))
}

# Get Africa map data
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Read the cleaned data
df <- read_csv("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned.csv")

# Filter for African institutions
african_countries <- africa$name
df_africa <- df %>%
  filter(Country %in% african_countries)

# Calculate total projects for each institution
df_africa <- df_africa %>%
  mutate(total_projects = CHAMNHA + HEAT + ENBEL + GHAP + HAPI + BioHEAT + HIGH_Horizons)

# Create leadership categories based on the columns
df_africa <- df_africa %>%
  mutate(leadership = case_when(
    Gueladio_Cisse == 1 & Matthew_Chersich == 1 ~ "Joint Projects",
    Gueladio_Cisse == 1 ~ "Gueladio Cisse",
    Matthew_Chersich == 1 ~ "Matthew Chersich",
    Pilot_Projects == 1 ~ "Pilot Projects",
    TRUE ~ "Other"
  ))

# Convert to sf object
partners_sf <- df_africa %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Define updated color scheme for leadership
leadership_colors <- c(
  'Gueladio Cisse' = '#2C85B2',      # More muted blue
  'Matthew Chersich' = '#B23A48',     # More muted red
  'Joint Projects' = '#7E4F88'        # More muted purple
)

# Create a vector of countries with partners
partner_countries <- unique(df$Country)

# Add a column to the Africa map data to indicate HEAT countries
africa <- africa %>%
  mutate(has_partners = name %in% heat_countries)

# Create the map
map <- ggplot() +
  # Base Africa map
  geom_sf(data = africa,
          aes(fill = has_partners),
          color = "gray40",           # Darker boundary lines
          size = 0.3) +
  
  # Add graticules (latitude/longitude lines)
  geom_sf(data = st_graticule(africa, lat = seq(-40, 40, 10), lon = seq(-20, 50, 10)),
          color = alpha("gray60", 0.3),
          size = 0.1) +
  
  # Add partner points
  geom_sf(data = partners_sf,
          aes(size = total_projects,
              fill = leadership,
              color = leadership),
          alpha = 0.85,
          stroke = 0.5,
          shape = 21) +
  
  # Add labels for all institutions
  geom_text_repel(
    data = partners_sf,  # Label all institutions
    aes(label = Institution,
        geometry = geometry),
    stat = "sf_coordinates",
    size = 2.8,
    fontface = "bold",
    box.padding = 0.8,
    point.padding = 0.5,
    force = 10,
    max.overlaps = 50,  # Increase to show all labels
    color = "black",
    bg.color = "white",
    bg.r = 0.15,
    min.segment.length = 0.3,
    segment.color = "gray50",
    segment.size = 0.2,
    nudge_x = 1,  # More horizontal spacing
    nudge_y = 1,  # More vertical spacing
    direction = "both"  # Allow both horizontal and vertical separation
  ) +
  
  # Customize colors for points
  scale_fill_manual(
    values = c(
      leadership_colors,
      'Pilot Projects' = '#9B59B6'
    ),
    name = "Project Leadership",
    breaks = c('Gueladio Cisse', 'Matthew Chersich', 'Joint Projects')
  ) +
  
  scale_color_manual(
    values = c(
      leadership_colors,
      'Pilot Projects' = '#9B59B6'
    ),
    name = "Project Leadership",
    breaks = c('Gueladio Cisse', 'Matthew Chersich', 'Joint Projects')
  ) +
  
  # Customize colors for choropleth
  scale_fill_manual(
    values = c(
      "TRUE" = alpha("#FFD700", 0.4),
      "FALSE" = "white"
    ),
    name = expression(paste("HE"^2, "AT Center")),
    labels = "Contributing Countries",
    breaks = c("TRUE")
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
    text = element_text(family = "opensans"),
    plot.title = element_text(family = "montserrat", size = 16, face = "bold", hjust = 0, margin = margin(b = 20)),
    plot.subtitle = element_text(family = "montserrat", size = 12, hjust = 0, margin = margin(b = 20)),
    legend.position = "right",
    legend.title = element_text(family = "montserrat", face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.box = "vertical",
    legend.box.background = element_rect(color = "gray80", fill = "white", size = 0.3),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = "gray80", size = 0.3),
    panel.background = element_rect(fill = alpha("#F0F8FF", 0.2), color = NA),
    legend.key.size = unit(1.2, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.box.spacing = unit(0.4, "cm"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Add scale bar
  annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Add north arrow
  annotation_north_arrow(location = "bl", which_north = "true",
                        pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
                        style = north_arrow_fancy_orienteering) +
  
  # Set map extent to focus on Africa
  coord_sf(
    xlim = c(-20, 50),
    ylim = c(-40, 40),
    expand = FALSE
  ) +
  
  # Update labels
  labs(
    title = "African Health Research Partnership Network",
    subtitle = expression(paste("Project Leadership and Collaborations (HE"^2, "AT Center)")),
    caption = paste0("Data: Research Collaboration Network\n",
                     format(Sys.Date(), "%B %Y"))
  ) +
  
  # Update the map boundaries for HEAT countries
  geom_sf(data = africa %>% filter(has_partners),
          fill = NA,
          color = "#FFD700",
          size = 0.8)

# Display the map
print(map)

# Save the map
ggsave("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/african_partnership_map_leadership.pdf", 
       width = 16, height = 16, dpi = 300,
       device = cairo_pdf)

ggsave("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/african_partnership_map_leadership.png", 
       width = 16, height = 16, dpi = 300,
       bg = "white")

# Data validation and summary statistics
cat("\nSummary of African Partners:\n")
cat("============================\n")

# Total number of institutions
cat("\nTotal African institutions:", nrow(df_africa))

# Project participation counts
cat("\n\nParticipation by Project:")
project_counts <- data.frame(
  CHAMNHA = sum(df_africa$CHAMNHA),
  "HE²AT" = sum(df_africa$HEAT),
  ENBEL = sum(df_africa$ENBEL),
  GHAP = sum(df_africa$GHAP),
  HAPI = sum(df_africa$HAPI),
  BioHEAT = sum(df_africa$BioHEAT),
  HIGH_Horizons = sum(df_africa$HIGH_Horizons)
)

print(project_counts)

# Institutions by country
cat("\n\nInstitutions by Country:\n")
country_counts <- df_africa %>%
  group_by(Country) %>%
  summarise(
    Institutions = n(),
    Total_Project_Participations = sum(total_projects)
  ) %>%
  arrange(desc(Total_Project_Participations))
print(country_counts)

# Multi-project institutions
cat("\n\nInstitutions with Multiple Projects:\n")
multi_project <- df_africa %>%
  filter(total_projects > 1) %>%
  select(Institution, Country, total_projects) %>%
  arrange(desc(total_projects))
print(multi_project)

# List of all institutions and their projects
cat("\n\nDetailed Project Participation:\n")
institution_projects <- df_africa %>%
  select(Institution, Country, CHAMNHA, HEAT, ENBEL, GHAP, HAPI, BioHEAT, HIGH_Horizons, total_projects) %>%
  arrange(desc(total_projects), Country, Institution)
print(institution_projects)

# Save detailed summary to CSV
write.csv(institution_projects, 
          "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/african_partners_summary.csv", 
          row.names = FALSE)

cat("\nDetailed summary has been saved to 'african_partners_summary.csv'\n")

# Print summary statistics
cat("\nSummary of Leadership Distribution:\n")
cat("================================\n")
print(table(df_africa$leadership))

# Save detailed summary to CSV
leadership_summary <- df_africa %>%
  select(Institution, Country, leadership, total_projects) %>%
  arrange(leadership, desc(total_projects))

write.csv(leadership_summary, 
          "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/african_partners_leadership_summary.csv", 
          row.names = FALSE)

cat("\nDetailed leadership summary has been saved to 'african_partners_leadership_summary.csv'\n")

# Create the network plot
ggraph(graph_layout, layout = 'fr') +
  geom_edge_link(aes(width = weight), 
                 alpha = 0.2, 
                 color = "gray50") +
  geom_node_point(aes(color = leadership),
                  size = 10) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 size = 3) +
  scale_edge_width(range = c(0.5, 2)) +
  scale_color_manual(values = c(
    'Gueladio Cisse' = '#2C85B2',    # Blue
    'Matthew Chersich' = '#B23A48',   # Red
    'Joint Projects' = '#7E4F88',     # Purple
    'Pilot Projects' = '#7E4F88',     # Same purple for pilot projects
    'Other' = '#7E4F88'              # Same purple for other
  )) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  ) +
  labs(color = "Project Leadership",
       edge_width = "Partnership Strength") 