# Final simplified map script with embedded data
library(ggplot2)
library(dplyr)
library(sf)
library(ggrepel)
library(scales)  # For alpha function

# Create a simple dummy dataset for testing
partners_data <- data.frame(
  Institution = c(
    "University of Witwatersrand", "London School of Hygiene", "University of Washington", 
    "Karolinska Institute", "University of Ghana", "University of Nairobi",
    "University of Cape Town", "University of Pretoria", "University of Zimbabwe",
    "University of Malawi", "University of Zambia", "University of Botswana",
    "University of Nigeria", "University of Ibadan", "University of Dar es Salaam",
    "University of Khartoum", "University of Addis Ababa", "University of Makerere",
    "Oxford University", "Cambridge University", "Imperial College London",
    "University of Edinburgh", "University of Manchester", "University College London"
  ),
  Country = c(
    "South Africa", "United Kingdom", "United States", "Sweden", "Ghana", "Kenya",
    "South Africa", "South Africa", "Zimbabwe", "Malawi", "Zambia", "Botswana",
    "Nigeria", "Nigeria", "Tanzania", "Sudan", "Ethiopia", "Uganda",
    "United Kingdom", "United Kingdom", "United Kingdom", "United Kingdom", "United Kingdom", "United Kingdom"
  ),
  lat = c(
    -26.1917, 51.5207, 47.6553, 59.3293, 5.6037, -1.2921,
    -33.9575, -25.7461, -17.7835, -15.7861, -15.3875, -24.6282,
    6.5244, 7.3775, -6.8235, 15.5007, 9.0054, 0.3476,
    51.7548, 52.2053, 51.4991, 55.9533, 53.4668, 51.5246
  ),
  lon = c(
    28.0323, -0.1303, -122.3035, 18.0686, -0.1870, 36.8219,
    18.4615, 28.2308, 31.0519, 35.0059, 28.3228, 25.9231,
    3.3792, 3.9470, 39.2695, 32.5599, 38.7612, 32.5825,
    -1.2544, 0.1218, -0.1791, -3.1883, -2.2339, -0.1339
  ),
  HEAT = c(
    1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0
  ),
  CHAMNHA = c(
    1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0
  ),
  ENBEL = c(
    0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1
  ),
  GHAP = c(
    1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ),
  HAPI = c(
    0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ),
  BioHEAT = c(
    1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ),
  HIGH_Horizons = c(
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0
  ),
  Funder = c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  )
)

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
    # Expand criteria for labeling
    is_major_partner = total_projects >= 2 |  # Show institutions with 2+ projects
      Funder == 1 |                          # Show all funders
      Institution %in% c(                     # Show specific key institutions
        "London School of Hygiene",
        "Karolinska Institute",
        "University of Washington",
        "University of Witwatersrand",
        "University of Cape Town",
        "University of Pretoria"
      )
  )

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

# Convert to sf object for partners
partners_sf <- df %>%
  filter(!is.na(lon) & !is.na(lat)) %>%  # Remove rows with missing coordinates
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Create a simple world map outline
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
  st_sf(name = "World")

# Create the map
map <- ggplot() +
  # Base map
  geom_sf(data = world, fill = "white", color = "gray40", linewidth = 0.3) +
  
  # Add partner points
  geom_sf(data = partners_sf,
          aes(size = total_projects,
              color = primary_project,
              fill = primary_project),
          alpha = 0.85,
          stroke = 1.2,
          shape = 21) +  # Filled circles with border
  
  # Add labels for major partners
  geom_text_repel(
    data = partners_sf %>% filter(is_major_partner),
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    fontface = "bold",
    box.padding = 0.6,
    point.padding = 0.4,
    force = 10,
    max.overlaps = 50,
    color = "black",
    min.segment.length = 0.2,
    segment.color = "gray50",
    segment.size = 0.2,
    nudge_x = 1,
    nudge_y = 1
  ) +
  
  # Customize colors for points
  scale_color_manual(
    values = project_colors,
    name = "Project Participation",
    breaks = names(project_colors)
  ) +
  
  # Add fill colors matching the outline
  scale_fill_manual(
    values = lapply(project_colors, function(x) adjustcolor(x, alpha.f = 0.7)),
    guide = "none"
  ) +
  
  # Customize size scale
  scale_size_continuous(
    name = "Number of Projects",
    range = c(4, 12),
    breaks = c(1, 2, 3, 4, 5, 6, 7)
  ) +
  
  # Theme customization
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 20)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.box = "vertical",
    legend.box.background = element_rect(color = "gray80", fill = "white", linewidth = 0.3),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = "gray80", linewidth = 0.3),
    panel.background = element_rect(fill = alpha("#F0F8FF", 0.2), color = NA),
    legend.key.size = unit(1.2, "cm"),
    plot.margin = margin(20, 40, 20, 20)  # Increased right margin for inset map
  ) +
  
  # Set map extent - shifted left to create more space for Europe inset
  coord_sf(
    xlim = c(-25, 50),
    ylim = c(-60, 80),
    expand = FALSE
  ) +
  
  # Update labels - removed subtitle for more space
  labs(
    title = "Global Health Research Partnership Network",
    caption = paste0("Data: Research Collaboration Network | ",
                     format(Sys.Date(), "%B %Y"))
  )

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
  # Add partner points
  geom_sf(data = europe_partners,
          aes(size = total_projects,
              color = primary_project,
              fill = primary_project),
          alpha = 0.85,
          stroke = 1.0,
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
    max.overlaps = 30,
    color = "black",
    min.segment.length = 0.2,
    segment.color = "gray50",
    segment.size = 0.2
  ) +
  
  # Customize colors for points
  scale_color_manual(
    values = project_colors,
    guide = "none"
  ) +
  
  # Add fill colors matching the outline
  scale_fill_manual(
    values = lapply(project_colors, function(x) adjustcolor(x, alpha.f = 0.7)),
    guide = "none"
  ) +
  
  # Customize size scale
  scale_size_continuous(
    guide = "none"
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

# Display the maps separately
print(map)
print(europe_map)

# Save the maps
ggsave("world_partnership_map_final.png", 
       map,
       width = 20, height = 10, dpi = 300,
       bg = "white")  # Ensure white background

ggsave("europe_inset_map_final.png", 
       europe_map,
       width = 10, height = 8, dpi = 300,
       bg = "white")  # Ensure white background

# Print summary statistics
cat("\nSummary of Global Project Distribution:\n")
cat("====================================\n")
print(table(df$primary_project))
