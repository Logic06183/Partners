import pandas as pd
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
from cartopy.io.shapereader import Reader
import numpy as np

# Read the CSV file with coordinates
df = pd.read_csv('partners_data_with_coords.csv')

# Remove rows with missing coordinates
df = df.dropna(subset=['lon', 'lat'])

# Create figure with a geographic projection
plt.figure(figsize=(20, 10))
ax = plt.axes(projection=ccrs.Robinson())

# Add map features
ax.add_feature(cfeature.LAND, facecolor='lightgray', alpha=0.5)
ax.add_feature(cfeature.OCEAN, facecolor='lightblue', alpha=0.5)
ax.add_feature(cfeature.BORDERS, linestyle=':', alpha=0.5)
ax.add_feature(cfeature.COASTLINE)

# Calculate node sizes based on number of projects
df['total_projects'] = df[['CHAMNHA', 'HEAT', 'HIGH', 'ENBEL', 'GHAP', 'HAPI', 'BioHEAT', 'HIGH_Horizons']].sum(axis=1)

# Create scatter plot for institutions
for idx, row in df.iterrows():
    if row['Funder'] == 1:
        color = 'red'
        marker = '^'  # triangle for funders
    else:
        color = 'blue'
        marker = 'o'  # circle for research institutions
    
    size = row['total_projects'] * 50 + 100  # Base size + project multiplier
    
    # Plot point
    plt.plot(row['lon'], row['lat'],
             marker=marker,
             markersize=np.sqrt(size),
             color=color,
             alpha=0.6,
             transform=ccrs.PlateCarree())

# Add legend
legend_elements = [
    plt.Line2D([0], [0], marker='o', color='w', markerfacecolor='blue',
               markersize=10, label='Research Institution'),
    plt.Line2D([0], [0], marker='^', color='w', markerfacecolor='red',
               markersize=10, label='Funding Organization'),
    plt.Line2D([0], [0], marker='o', color='w', markerfacecolor='blue',
               markersize=15, label='5+ Projects'),
    plt.Line2D([0], [0], marker='o', color='w', markerfacecolor='blue',
               markersize=10, label='2-4 Projects'),
    plt.Line2D([0], [0], marker='o', color='w', markerfacecolor='blue',
               markersize=5, label='1 Project')
]
ax.legend(handles=legend_elements, loc='lower left', bbox_to_anchor=(0.1, 0.1))

# Add title
plt.title('Global Distribution of Research Partners\nSize indicates number of projects', 
          pad=20, fontsize=16)

# Save the map
plt.savefig('geographic_partners_map.png', dpi=300, bbox_inches='tight')
plt.close() 