import pandas as pd
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import numpy as np
from matplotlib.patches import Patch
from matplotlib.lines import Line2D

# Alternative style setting
plt.style.use('default')
plt.rcParams.update({
    'figure.facecolor': 'white',
    'axes.facecolor': 'white',
    'axes.grid': True,
    'grid.color': 'gray',
    'grid.alpha': 0.2,
    'font.family': 'sans-serif',
    'font.sans-serif': ['Arial'],
    'font.size': 10
})

# Read data
df = pd.read_csv('partners_data_with_coords.csv')

# Create figure with specific size for publication (in inches)
fig = plt.figure(figsize=(15, 10), dpi=300)
ax = plt.axes(projection=ccrs.Robinson())

# Add sophisticated map features
ax.add_feature(cfeature.LAND, facecolor='#f5f5f5', alpha=1.0)
ax.add_feature(cfeature.OCEAN, facecolor='#e6f3ff', alpha=1.0)
ax.add_feature(cfeature.COASTLINE, linewidth=0.5, color='#787878')
ax.add_feature(cfeature.BORDERS, linestyle=':', color='#787878', alpha=0.5)

# Add gridlines
gl = ax.gridlines(draw_labels=False, linewidth=0.2, color='gray', alpha=0.2, linestyle='--')

# Define project colors using a professional color palette
project_colors = {
    'CHAMNHA': '#2c7bb6',
    'HEAT': '#d7191c',
    'ENBEL': '#1a9641',
    'GHAP': '#756bb1',
    'HAPI': '#fd8d3c',
    'BioHEAT': '#31a354',
    'HIGH_Horizons': '#636363'
}

# Create network connections between collaborating institutions
for _, source in df[df['Funder'] == 0].iterrows():
    projects = [col for col in project_colors.keys() if source[col] == 1]
    for project in projects:
        partners = df[(df[project] == 1) & (df['Funder'] == 0)]
        for _, target in partners.iterrows():
            if source['Institution'] != target['Institution']:
                ax.plot([source['lon'], target['lon']], 
                       [source['lat'], target['lat']],
                       color=project_colors[project],
                       alpha=0.1,
                       linewidth=0.3,
                       transform=ccrs.Geodetic())

# Plot institutions
for _, row in df.iterrows():
    projects = [proj for proj in project_colors.keys() if row[proj] == 1]
    if projects:
        size = len(projects) * 50 + 50  # Base size plus project multiplier
        if row['Funder'] == 1:
            marker = '^'
            color = '#000000'
            size = size * 1.2
        else:
            marker = 'o'
            color = project_colors[projects[0]]
        
        ax.plot(row['lon'], row['lat'],
                marker=marker,
                markersize=np.sqrt(size),
                color=color,
                markeredgecolor='white',
                markeredgewidth=0.5,
                alpha=0.7,
                transform=ccrs.PlateCarree(),
                zorder=5)

# Create legend
legend_elements = []
for project, color in project_colors.items():
    legend_elements.append(Line2D([0], [0], marker='o', color='w',
                                markerfacecolor=color,
                                markeredgecolor='white',
                                markersize=8,
                                label=project))
legend_elements.append(Line2D([0], [0], marker='^', color='w',
                            markerfacecolor='#000000',
                            markeredgecolor='white',
                            markersize=8,
                            label='Funding Organization'))

# Add legend with professional styling
leg = ax.legend(handles=legend_elements,
                loc='lower left',
                bbox_to_anchor=(0.02, 0.02),
                title='Research Programs',
                frameon=True,
                facecolor='white',
                edgecolor='none',
                fontsize=8)
leg.get_title().set_fontsize(9)
leg.get_title().set_fontweight('bold')

# Add title and subtitle
plt.suptitle('Global Health Research Partnership Network',
             fontsize=16,
             fontweight='bold',
             y=0.95)
plt.title('Size indicates number of project participations\nLines show research collaborations',
         fontsize=10,
         pad=20)

# Add source note
plt.figtext(0.98, 0.02,
            'Source: Wits Planetary Health Research, 2024\n'
            'Visualization: Craig Parker',
            ha='right',
            fontsize=8,
            style='italic')

# Save with high resolution
plt.savefig('publication_quality_map.png',
            dpi=300,
            bbox_inches='tight',
            facecolor='white',
            edgecolor='none')
plt.close() 