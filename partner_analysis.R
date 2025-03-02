# Partner Analysis Script
# This script analyzes and visualizes partners by different types
# from the partners_cleaned_with_short_names.csv file

# Load required libraries
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggplot2")) install.packages("tidyverse")
if (!require("leaflet")) install.packages("leaflet")
if (!require("DT")) install.packages("DT")
if (!require("plotly")) install.packages("plotly")
if (!require("RColorBrewer")) install.packages("RColorBrewer")

library(tidyverse)
library(ggplot2)
library(leaflet)
library(DT)
library(plotly)
library(RColorBrewer)

# Set the file path
file_path <- "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned_with_short_names.csv"

# Read the CSV file
partners <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)

# Print basic information about the dataset
cat("Dataset Overview:\n")
cat("Number of partners:", nrow(partners), "\n")
cat("Number of countries:", length(unique(partners$Country)), "\n\n")

# Define partner type columns
partner_type_cols <- c("Official Partners", "CHAMNHA", "HEAT", "ENBEL", "GHAP", 
                       "HAPI", "BioHEAT", "HIGH_Horizons", "Funder", "Partners", 
                       "Data_Providers", "Government Partners", "Policy_Stakeholders",
                       "Gueladio_Cisse", "Matthew_Chersich", "Pilot_Projects")

# Function to count partners by type
count_by_type <- function(data, type_col) {
  sum(data[[type_col]], na.rm = TRUE)
}

# Count partners by each type
partner_counts <- sapply(partner_type_cols, function(col) count_by_type(partners, col))
partner_counts_df <- data.frame(
  Type = partner_type_cols,
  Count = partner_counts
)

# Sort by count in descending order
partner_counts_df <- partner_counts_df[order(-partner_counts_df$Count), ]

# Create a bar plot of partner types
ggplot(partner_counts_df, aes(x = reorder(Type, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Number of Partners by Type",
       x = "Partner Type",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Save the plot
ggsave("partner_types_barplot.png", width = 10, height = 8)

# Create a table of partners by country
country_summary <- partners %>%
  group_by(Country) %>%
  summarize(
    Total = n(),
    `Official Partners` = sum(`Official Partners`, na.rm = TRUE),
    CHAMNHA = sum(CHAMNHA, na.rm = TRUE),
    HEAT = sum(HEAT, na.rm = TRUE),
    ENBEL = sum(ENBEL, na.rm = TRUE),
    GHAP = sum(GHAP, na.rm = TRUE),
    HAPI = sum(HAPI, na.rm = TRUE),
    BioHEAT = sum(BioHEAT, na.rm = TRUE),
    HIGH_Horizons = sum(HIGH_Horizons, na.rm = TRUE),
    Funder = sum(Funder, na.rm = TRUE),
    Partners = sum(Partners, na.rm = TRUE),
    Data_Providers = sum(Data_Providers, na.rm = TRUE),
    `Government Partners` = sum(`Government Partners`, na.rm = TRUE),
    Policy_Stakeholders = sum(Policy_Stakeholders, na.rm = TRUE)
  ) %>%
  arrange(desc(Total))

# Print the country summary
print(country_summary)

# Create an interactive map of all partners
partner_map <- leaflet(partners) %>%
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
    labels = "Partners",
    opacity = 0.8
  )

# Create a function to generate a map for a specific partner type
create_type_map <- function(data, type_col) {
  filtered_data <- data[data[[type_col]] == 1, ]
  
  if(nrow(filtered_data) > 0) {
    map <- leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon, 
        lat = ~lat,
        popup = ~paste("<b>", Institution, "</b><br>",
                      "City: ", City, "<br>",
                      "Country: ", Country, "<br>",
                      "Short Name: ", Short_Name),
        radius = 5,
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.8
      ) %>%
      addLegend(
        position = "bottomright",
        colors = "red",
        labels = type_col,
        opacity = 0.8
      )
    
    return(map)
  } else {
    return(NULL)
  }
}

# Create a map for Official Partners
official_partners_map <- create_type_map(partners, "Official Partners")

# Create a network visualization of partners
# Convert the data to a format suitable for network visualization
partner_matrix <- partners %>%
  select(Institution, all_of(partner_type_cols)) %>%
  as.matrix()

# Create a correlation matrix between partner types
partner_type_cor <- cor(partners[, partner_type_cols])

# Create a heatmap of the correlation matrix
heatmap_plot <- heatmap(
  partner_type_cor,
  col = colorRampPalette(brewer.pal(9, "Blues"))(100),
  main = "Correlation between Partner Types",
  margins = c(10, 10)
)

# Create a summary table of all partners with their types
partner_summary <- partners %>%
  select(Institution, Country, Short_Name, all_of(partner_type_cols))

# Create an interactive datatable
datatable(partner_summary, 
          options = list(
            pageLength = 15,
            autoWidth = TRUE,
            scrollX = TRUE
          ),
          caption = "All Partners with Their Types")

# Create a pie chart of partners by country
country_counts <- partners %>%
  count(Country) %>%
  arrange(desc(n))

# Take top 10 countries for better visualization
top_countries <- head(country_counts, 10)
other_countries <- data.frame(
  Country = "Other",
  n = sum(country_counts$n[11:nrow(country_counts)])
)
pie_data <- rbind(top_countries, other_countries)

# Create the pie chart
pie_chart <- plot_ly(pie_data, labels = ~Country, values = ~n, type = 'pie',
                    textinfo = 'label+percent',
                    insidetextorientation = 'radial') %>%
  layout(title = 'Partners by Country',
         showlegend = TRUE)

# Print summary statistics for each partner type
cat("\nSummary of Partner Types:\n")
for (col in partner_type_cols) {
  cat(col, ":", sum(partners[[col]], na.rm = TRUE), "partners\n")
}

# Create a function to print detailed information for each partner type
print_partner_details <- function(data, type_col) {
  filtered_data <- data[data[[type_col]] == 1, ]
  
  if(nrow(filtered_data) > 0) {
    cat("\n", type_col, "Partners (", nrow(filtered_data), "):\n", sep = "")
    for(i in 1:nrow(filtered_data)) {
      cat("  -", filtered_data$Institution[i], "(", filtered_data$Country[i], ")\n")
    }
  }
}

# Print details for each partner type
for (col in partner_type_cols) {
  print_partner_details(partners, col)
}

# Create a stacked bar chart of partner types by country
# Focus on countries with at least 5 partners for better visualization
countries_with_many_partners <- country_summary %>%
  filter(Total >= 5) %>%
  pull(Country)

# Prepare data for stacked bar chart
stacked_data <- partners %>%
  filter(Country %in% countries_with_many_partners) %>%
  select(Country, all_of(partner_type_cols)) %>%
  pivot_longer(cols = all_of(partner_type_cols), 
               names_to = "Partner_Type", 
               values_to = "Value") %>%
  filter(Value == 1) %>%
  group_by(Country, Partner_Type) %>%
  summarize(Count = n(), .groups = "drop")

# Create the stacked bar chart
ggplot(stacked_data, aes(x = Country, y = Count, fill = Partner_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Partner Types by Country",
       x = "Country",
       y = "Count",
       fill = "Partner Type") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))

# Save the plot
ggsave("partner_types_by_country.png", width = 12, height = 8)

# Print a summary message
cat("\nAnalysis complete! The script has generated various visualizations and summaries of the partners data.\n")
