import pandas as pd
import folium
from folium import plugins

# Read the data
df = pd.read_csv('partners_data_with_coords.csv')

# Create a base map centered on Africa
m = folium.Map(
    location=[0, 20],  # Center on Africa
    zoom_start=3,
    tiles='cartodb positron'  # Clean, light style
)

# Define project colors
project_colors = {
    'CHAMNHA': '#0077BB',
    'HEAT': '#EE3377',
    'ENBEL': '#009988',
    'GHAP': '#CC3311',
    'HAPI': '#33BBEE',
    'BioHEAT': '#EE7733',
    'HIGH_Horizons': '#555555'
}

# Add markers for each institution
for idx, row in df.iterrows():
    # Calculate number of projects
    projects = [proj for proj in project_colors.keys() if row[proj] == 1]
    num_projects = len(projects)
    
    if num_projects > 0:
        # Determine primary project color
        color = project_colors[projects[0]]
        
        # Create popup content
        popup_content = f"""
        <div style="font-family: Arial; min-width: 200px;">
            <h4>{row['Institution']}</h4>
            <b>Location:</b> {row['City']}, {row['Country']}<br>
            <b>Projects:</b><br>
            {'<br>'.join(projects)}
        </div>
        """
        
        # Calculate marker size based on number of projects
        radius = (num_projects * 5) + 8
        
        # Different style for funders
        if row['Funder'] == 1:
            # Use custom icon for funders
            folium.CircleMarker(
                location=[row['lat'], row['lon']],
                popup=folium.Popup(popup_content, max_width=300),
                radius=radius,
                color='black',
                weight=2,
                fill=True,
                fill_color='black',
                fill_opacity=0.7,
                icon='info-sign'  # Use a different icon
            ).add_to(m)
        else:
            folium.CircleMarker(
                location=[row['lat'], row['lon']],
                popup=folium.Popup(popup_content, max_width=300),
                radius=radius,
                color='white',
                weight=1,
                fill=True,
                fill_color=color,
                fill_opacity=0.7
            ).add_to(m)

# Updated legend with CSS for custom triangle
legend_html = '''
<div style="position: fixed; 
            bottom: 50px; left: 50px; 
            border:2px solid gray; z-index:9999; 
            background-color:white;
            padding: 10px;
            font-size:14px;
            font-family: Arial;">
    <p><b>Research Programs</b></p>
'''

for project, color in project_colors.items():
    legend_html += f'''
    <p><span style="color:{color};">●</span> {project}</p>
    '''

# Add custom triangle for funder
legend_html += '''
    <p><span style="display:inline-block; width:0; height:0; 
              border-left:6px solid transparent;
              border-right:6px solid transparent;
              border-bottom:12px solid black;
              margin-right:5px;"></span> Funding Organization</p>
    </div>
'''

m.get_root().html.add_child(folium.Element(legend_html))

# Add a title
title_html = '''
<div style="position: fixed; 
            top: 20px; left: 50%; 
            transform: translateX(-50%);
            z-index:9999; 
            background-color:white;
            padding: 10px;
            border:2px solid gray;
            font-size:18px;
            font-family: Arial;">
    <h3 style="margin:0;">Global Health Research Partnership Network</h3>
    <p style="margin:5px 0 0 0; font-size:14px; font-style:italic;">
        Size indicates number of project participations
    </p>
</div>
'''

m.get_root().html.add_child(folium.Element(title_html))

# Save the map
m.save('interactive_partnership_map.html') 