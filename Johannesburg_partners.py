import pandas as pd
import folium
from folium.plugins import MarkerCluster

# Load the CSV data
df = pd.read_csv('partners_cleaned_with_short_names.csv')

# Filter for South African partners
sa_partners = df[df['Country'] == 'South Africa']

# Create a map centered on South Africa
sa_map = folium.Map(location=[-28.5, 24.5], zoom_start=6)

# Create a dictionary to store project-specific marker clusters
project_clusters = {
    'All Partners': MarkerCluster(name='All Partners').add_to(sa_map),
    'CHAMNHA': MarkerCluster(name='CHAMNHA').add_to(sa_map),
    'HEAT': MarkerCluster(name='HEAT').add_to(sa_map),
    'ENBEL': MarkerCluster(name='ENBEL').add_to(sa_map),
    'GHAP': MarkerCluster(name='GHAP').add_to(sa_map),
    'HAPI': MarkerCluster(name='HAPI').add_to(sa_map),
    'BioHEAT': MarkerCluster(name='BioHEAT').add_to(sa_map),
    'HIGH_Horizons': MarkerCluster(name='HIGH_Horizons').add_to(sa_map)
}

# Add layer control
folium.LayerControl().add_to(sa_map)

# Define color mapping for projects
project_colors = {
    'CHAMNHA': 'red',
    'HEAT': 'blue',
    'ENBEL': 'green',
    'GHAP': 'purple',
    'HAPI': 'orange',
    'BioHEAT': 'darkred',
    'HIGH_Horizons': 'darkblue'
}

# Add markers for each partner
for idx, row in sa_partners.iterrows():
    # Create popup content with institution details
    popup_content = f"""
    <b>{row['Institution']}</b><br>
    Short Name: {row['Short_Name']}<br>
    City: {row['City']}<br>
    """
    
    # Add project affiliations if any
    projects = []
    if row['CHAMNHA'] == 1: projects.append('CHAMNHA')
    if row['HEAT'] == 1: projects.append('HEAT')
    if row['ENBEL'] == 1: projects.append('ENBEL')
    if row['GHAP'] == 1: projects.append('GHAP')
    if row['HAPI'] == 1: projects.append('HAPI')
    if row['BioHEAT'] == 1: projects.append('BioHEAT')
    if row['HIGH_Horizons'] == 1: projects.append('HIGH_Horizons')
    
    if projects:
        popup_content += f"Projects: {', '.join(projects)}<br>"
    
    # Add marker to the "All Partners" cluster
    folium.Marker(
        location=[row['lat'], row['lon']],
        popup=folium.Popup(popup_content, max_width=300),
        tooltip=row['Short_Name'],
        icon=folium.Icon(icon="info-sign")
    ).add_to(project_clusters['All Partners'])
    
    # Add marker to each relevant project cluster
    for project in projects:
        folium.Marker(
            location=[row['lat'], row['lon']],
            popup=folium.Popup(popup_content, max_width=300),
            tooltip=row['Short_Name'],
            icon=folium.Icon(color=project_colors.get(project, 'blue'), icon="info-sign")
        ).add_to(project_clusters[project])

# Save the map
sa_map.save('south_africa_partners_map_by_project.html')

print("Map created successfully!")
