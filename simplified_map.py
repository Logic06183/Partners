import pandas as pd
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import numpy as np
from matplotlib.patches import Patch, Rectangle
from matplotlib.lines import Line2D

# Set up publication-quality settings
plt.rcParams.update({
    'figure.figsize': (16, 10),
    'figure.dpi': 300,
    'font.family': 'sans-serif',
    'font.sans-serif': ['Arial', 'Helvetica'],
    'font.size': 11
})

# Read data
df = pd.read_csv('partners_data_with_coords.csv')

# Create figure
fig = plt.figure(figsize=(16, 10))
ax = plt.axes(projection=ccrs.Robinson(central_longitude=0))
ax.set_extent([-130, 50, -45, 65], crs=ccrs.PlateCarree())

# Enhanced map features
ax.add_feature(cfeature.LAND, facecolor='#f9f9f9', alpha=1.0)
ax.add_feature(cfeature.OCEAN, facecolor='#f0f8ff', alpha=1.0)
ax.add_feature(cfeature.COASTLINE, linewidth=0.6, color='#404040')
ax.add_feature(cfeature.BORDERS, linestyle=':', color='#808080', alpha=0.3)

# Add refined graticules
gl = ax.gridlines(draw_labels=False, 
                  linewidth=0.2, 
                  color='gray', 
                  alpha=0.2, 
                  linestyle=':')

# Scientific color palette (colorblind-friendly)
project_colors = {
    'CHAMNHA': '#0077BB',  # Blue
    'HEAT': '#EE3377',     # Magenta
    'ENBEL': '#009988',    # Teal
    'GHAP': '#CC3311',     # Red
    'HAPI': '#33BBEE',     # Cyan
    'BioHEAT': '#EE7733',  # Orange
    'HIGH_Horizons': '#555555'  # Gray
}

# Plot institutions with refined markers - no network lines
for _, row in df.iterrows():
    projects = [proj for proj in project_colors.keys() if row[proj] == 1]
    if projects:
        # Reduce base size for smaller dots
        size = len(projects) * 25 + 30  # Reduced scaling
        if row['Funder'] == 1:
            marker = '^'
            color = '#000000'
            size = size * 1.2
        else:
            marker = 'o'
            # Use color of most recent/primary project
            color = project_colors[projects[0]]
        
        ax.plot(row['lon'], row['lat'],
                marker=marker,
                markersize=np.sqrt(size),
                color=color,
                markeredgecolor='white',
                markeredgewidth=0.8,
                alpha=0.85,
                transform=ccrs.PlateCarree(),
                zorder=5)

# Create legend
legend_elements = []
for project, color in project_colors.items():
    legend_elements.append(Line2D([0], [0], 
                                marker='o', 
                                color='w',
                                markerfacecolor=color,
                                markeredgecolor='white',
                                markersize=8,
                                label=project))
legend_elements.append(Line2D([0], [0], 
                            marker='^', 
                            color='w',
                            markerfacecolor='#000000',
                            markeredgecolor='white',
                            markersize=8,
                            label='Funding Organization'))

# Add legend
leg = ax.legend(handles=legend_elements,
                loc='lower left',
                bbox_to_anchor=(0.02, 0.02),
                title='Research Programs',
                frameon=True,
                facecolor='white',
                edgecolor='#d0d0d0',
                fontsize=9,
                title_fontsize=10,
                framealpha=0.95,
                borderpad=1,
                labelspacing=1.2)

# Add size legend
def add_size_legend(ax):
    sizes = [1, 3, 5]
    y_base = 0.15
    x_base = 0.85
    for i, num in enumerate(sizes):
        size = np.sqrt(num * 25 + 30)  # Match new scaling
        circle = plt.Circle((x_base, y_base + i*0.05), size/400,
                          fc='gray', ec='white', alpha=0.6, transform=ax.transAxes)
        ax.add_artist(circle)
        ax.text(x_base + 0.04, y_base + i*0.05, f'{num} projects',
                transform=ax.transAxes, va='center', fontsize=8)

add_size_legend(ax)

# Add title
plt.suptitle('Global Health Research Partnership Network',
             fontsize=16,
             fontweight='bold',
             y=0.95)
plt.title('Node size proportional to number of project participations',
         fontsize=10,
         style='italic',
         pad=20)

# Add source
plt.figtext(0.98, 0.02,
            'Source: Wits Planetary Health Research, 2024\n'
            'Visualization represents research partnerships across multiple programs',
            ha='right',
            fontsize=8,
            style='italic')

# Save
plt.savefig('simplified_network_map.pdf',
            dpi=400,
            bbox_inches='tight',
            facecolor='white',
            edgecolor='none')

plt.savefig('simplified_network_map.png',
            dpi=400,
            bbox_inches='tight',
            facecolor='white',
            edgecolor='none')

plt.close() 