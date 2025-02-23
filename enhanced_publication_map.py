import pandas as pd
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import numpy as np
from matplotlib.patches import Patch
from matplotlib.lines import Line2D

# Set up high-quality figure settings
plt.rcParams.update({
    'figure.figsize': (20, 12),  # Wider aspect ratio
    'figure.dpi': 300,
    'figure.facecolor': 'white',
    'figure.edgecolor': 'none',
    'axes.facecolor': 'white',
    'axes.grid': False,  # Remove grid for cleaner look
    'font.family': 'sans-serif',
    'font.sans-serif': ['Arial', 'Helvetica'],
    'font.size': 12,
    'savefig.dpi': 300,
    'savefig.format': 'pdf',
    'savefig.bbox': 'tight',
    'pdf.fonttype': 42,  # Ensure fonts are embedded properly
    'ps.fonttype': 42
})

# Read data
df = pd.read_csv('partners_data_with_coords.csv')

# Create figure
fig = plt.figure(figsize=(20, 12))
ax = plt.axes(projection=ccrs.Robinson(central_longitude=0))

# Enhance map features
ax.set_global()  # Show entire globe
ax.add_feature(cfeature.LAND, facecolor='#f8f8f8', alpha=1.0)
ax.add_feature(cfeature.OCEAN, facecolor='#f0f8ff', alpha=1.0)
ax.add_feature(cfeature.COASTLINE, linewidth=0.8, color='#404040')
ax.add_feature(cfeature.BORDERS, linestyle=':', color='#606060', alpha=0.4)

# Add subtle graticules
gl = ax.gridlines(draw_labels=False, linewidth=0.3, color='gray', alpha=0.2, linestyle=':')

# Updated professional color palette
project_colors = {
    'CHAMNHA': '#1f77b4',  # Blue
    'HEAT': '#d62728',     # Red
    'ENBEL': '#2ca02c',    # Green
    'GHAP': '#9467bd',     # Purple
    'HAPI': '#ff7f0e',     # Orange
    'BioHEAT': '#17becf',  # Cyan
    'HIGH_Horizons': '#7f7f7f'  # Gray
}

# Draw collaboration lines with curved paths
for _, source in df[df['Funder'] == 0].iterrows():
    projects = [col for col in project_colors.keys() if source[col] == 1]
    for project in projects:
        partners = df[(df[project] == 1) & (df['Funder'] == 0)]
        for _, target in partners.iterrows():
            if source['Institution'] != target['Institution']:
                ax.plot([source['lon'], target['lon']], 
                       [source['lat'], target['lat']],
                       color=project_colors[project],
                       alpha=0.15,  # Increased transparency
                       linewidth=0.4,
                       transform=ccrs.Geodetic(),
                       zorder=1)

# Plot institutions with enhanced markers
for _, row in df.iterrows():
    projects = [proj for proj in project_colors.keys() if row[proj] == 1]
    if projects:
        size = len(projects) * 40 + 60  # Adjusted size scaling
        if row['Funder'] == 1:
            marker = '^'
            color = '#000000'
            size = size * 1.3
        else:
            marker = 'o'
            color = project_colors[projects[0]]
        
        ax.plot(row['lon'], row['lat'],
                marker=marker,
                markersize=np.sqrt(size),
                color=color,
                markeredgecolor='white',
                markeredgewidth=1,
                alpha=0.8,
                transform=ccrs.PlateCarree(),
                zorder=5)

# Enhanced legend
legend_elements = []
for project, color in project_colors.items():
    legend_elements.append(Line2D([0], [0], marker='o', color='w',
                                markerfacecolor=color,
                                markeredgecolor='white',
                                markersize=10,
                                label=project))
legend_elements.append(Line2D([0], [0], marker='^', color='w',
                            markerfacecolor='#000000',
                            markeredgecolor='white',
                            markersize=10,
                            label='Funding Organization'))

# Add legend with improved styling
leg = ax.legend(handles=legend_elements,
                loc='lower left',
                bbox_to_anchor=(0.02, 0.02),
                title='Research Programs',
                frameon=True,
                facecolor='white',
                edgecolor='#e0e0e0',
                fontsize=10)
leg.get_title().set_fontsize(11)
leg.get_title().set_fontweight('bold')

# Add refined title and subtitle
plt.suptitle('Global Health Research Partnership Network',
             fontsize=20,
             fontweight='bold',
             y=0.95)
plt.title('Size indicates number of project participations • Lines show research collaborations',
         fontsize=12,
         pad=20,
         style='italic')

# Add source note with more detail
plt.figtext(0.98, 0.02,
            'Source: Wits Planetary Health Research, 2024\n'
            'Data represents active research partnerships and funding relationships',
            ha='right',
            fontsize=9,
            style='italic')

# Save as high-quality PDF
plt.savefig('global_research_network.pdf',
            dpi=300,
            bbox_inches='tight',
            facecolor='white',
            edgecolor='none')

# Also save as PNG for easy viewing
plt.savefig('global_research_network.png',
            dpi=300,
            bbox_inches='tight',
            facecolor='white',
            edgecolor='none')

plt.close() 