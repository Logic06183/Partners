# Required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # for data manipulation
  sf,           # for spatial data
  rnaturalearth,# for country boundaries
  rnaturalearthdata,
  ggspatial,    # for north arrow
  viridis,      # for color palettes
  showtext,     # for custom fonts
  RColorBrewer, # for color palettes
  ggrepel,      # for text label positioning
  ggthemes,     # for scientific themes
  scales        # for better scale formatting
)

# Load Google Fonts
font_add_google("Montserrat", "montserrat")
font_add_google("Open Sans", "opensans")
showtext_auto()

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Read the cleaned data
df <- read_csv("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned.csv")

# Calculate total projects for each institution
df <- df %>%
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

# Convert to sf object
partners_sf <- df %>%
  filter(!is.na(lon) & !is.na(lat)) %>%  # Remove rows with missing coordinates
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Define project colors
project_colors <- c(
  'Funding Agency' = '#2C3E50',  # Dark gray for funders
  'CHAMNHA' = '#2C85B2',      # Muted blue
  'HE²AT' = '#B23A48',        # Muted red
  'ENBEL' = '#3D8C40',        # Muted green
  'GHAP' = '#7E4F88',         # Muted purple
  'HAPI' = '#D68910',         # Muted orange
  'BioHEAT' = '#21618C',      # Dark blue
  'HIGH Horizons' = '#884EA0', # Dark purple
  'Multiple Projects' = '#566573' # Lighter gray
)

# Create the world map
world_map <- ggplot() +
  # Base world map
  geom_sf(data = world,
          fill = "white",
          color = "gray40",
          size = 0.3) +
  
  # Add graticules
  geom_sf(data = st_graticule(world, lat = seq(-80, 80, 20), lon = seq(-180, 180, 20)),
          color = alpha("gray60", 0.3),
          size = 0.1) +
  
  # Add partner points
  geom_sf(data = partners_sf,
          aes(size = total_projects,
              color = primary_project,
              fill = primary_project),
          alpha = 0.85,
          stroke = 1.2,
          shape = 21) +  # Filled circles with border
  
  # Customize colors for points
  scale_color_manual(
    values = project_colors,
    name = "Project Participation",
    breaks = names(project_colors)
  ) +
  
  # Add fill colors matching the outline
  scale_fill_manual(
    values = lapply(project_colors, function(x) alpha(x, 0.7)),
    guide = "none"
  ) +
  
  # Customize size scale
  scale_size_continuous(
    name = "Number of Projects",
    range = c(4, 12),  # Increase overall point size
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
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Add scale bar
  annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Add north arrow
  annotation_north_arrow(location = "bl", which_north = "true",
                        pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
                        style = north_arrow_fancy_orienteering) +
  
  # Set map extent
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 80),
    expand = FALSE
  ) +
  
  # Update labels
  labs(
    title = "Global Health Research Partnership Network",
    subtitle = "Project Distribution and Collaborations",
    caption = paste0("Data: Research Collaboration Network\n",
                     format(Sys.Date(), "%B %Y"))
  )

# Display the map
print(world_map)

# Save the map
ggsave("world_partnership_map.pdf", 
       width = 20, height = 10, dpi = 300,
       device = cairo_pdf)  # Better PDF rendering

ggsave("world_partnership_map.png", 
       width = 20, height = 10, dpi = 300,
       bg = "white")  # Ensure white background

# Print summary statistics
cat("\nSummary of Global Project Distribution:\n")
cat("====================================\n")
print(table(df$primary_project))

# Save detailed summary
project_summary <- df %>%
  select(Institution, Country, primary_project, total_projects) %>%
  arrange(primary_project, desc(total_projects))

write.csv(project_summary, 
          "global_partners_summary.csv", 
          row.names = FALSE) 