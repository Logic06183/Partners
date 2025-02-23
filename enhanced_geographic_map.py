import pandas as pd
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import numpy as np
from adjustText import adjust_text  # For better label placement

# Read the CSV file with coordinates
df = pd.read_csv('partners_data_with_coords.csv')

# Add debug print to check UW data before plotting
print("Checking University of Washington data before plotting:")
print(df[df['Institution'] == 'University of Washington'])

# Create figure with a geographic projection
fig = plt.figure(figsize=(24, 16))
ax = plt.axes(projection=ccrs.Robinson())

# Add detailed map features with improved colors
ax.add_feature(cfeature.LAND, facecolor='#FAFAFA', alpha=0.8)
ax.add_feature(cfeature.OCEAN, facecolor='#E6F3F8', alpha=0.8)
ax.add_feature(cfeature.BORDERS, linestyle='-', alpha=0.3, linewidth=0.3)
ax.add_feature(cfeature.COASTLINE, linewidth=0.5)

# Set map extent to focus on relevant areas
ax.set_extent([-120, 50, -40, 60], crs=ccrs.PlateCarree())

# Add gridlines with labels
gl = ax.gridlines(draw_labels=True, linewidth=0.2, color='gray', alpha=0.3, linestyle='--')
gl.top_labels = False
gl.right_labels = False

# Project colors with better contrast
project_colors = {
    'CHAMNHA': '#1f77b4',  # blue
    'HEAT': '#ff7f0e',     # orange
    'HIGH': '#2ca02c',     # green
    'ENBEL': '#d62728',    # red
    'GHAP': '#9467bd',     # purple
    'HAPI': '#8c564b',     # brown
    'BioHEAT': '#e377c2',  # pink
    'HIGH_Horizons': '#7f7f7f'  # gray
}

# Store all text annotations for later adjustment
texts = []

# Create scatter plot for institutions
for idx, row in df.iterrows():
    # Calculate marker size based on total projects
    total_projects = row[list(project_colors.keys())].sum()
    base_size = 3  # Increased base size for better visibility
    size = total_projects * base_size + base_size
    
    # Create pie chart for project participation
    projects = [proj for proj in project_colors.keys() if row[proj] == 1]
    if projects:
        # Create pie chart with white edges for better separation
        fig_pie = plt.figure(figsize=(size, size))
        ax_pie = fig_pie.add_subplot(111)
        
        # Plot pie chart with white edges
        colors = [project_colors[proj] for proj in projects]
        wedges, _ = ax_pie.pie([1] * len(projects), colors=colors,
                              wedgeprops=dict(edgecolor='white', linewidth=1))
        ax_pie.axis('equal')
        
        # Convert pie chart to image
        fig_pie.canvas.draw()
        pie_rgba = np.array(fig_pie.canvas.buffer_rgba())
        plt.close(fig_pie)
        
        # Plot the institution marker with adjusted size
        extent_size = 2 * (1 + total_projects * 0.2)  # Scale with number of projects
        ax.imshow(pie_rgba, 
                 extent=[row['lon']-extent_size, row['lon']+extent_size, 
                        row['lat']-extent_size, row['lat']+extent_size],
                 transform=ccrs.PlateCarree(),
                 zorder=3)
    
    # Add labels for all institutions
    if total_projects > 0:
        institution_name = row['Institution'].split(',')[0]  # Take first part of institution name
        # Create annotation and store it
        text = plt.annotate(
            f"{institution_name}\n({total_projects} projects)",
            xy=(row['lon'], row['lat']),
            xytext=(10, 10), textcoords='offset points',
            fontsize=7,
            bbox=dict(facecolor='white', edgecolor='none', alpha=0.7, pad=1),
            transform=ccrs.PlateCarree(),
            zorder=4
        )
        texts.append(text)

# Adjust text positions to minimize overlap
adjust_text(texts, 
           expand_points=(1.2, 1.2),
           force_points=(0.5, 0.5),
           arrowprops=dict(arrowstyle='->', color='gray', alpha=0.5))

# Create legend for projects
legend_elements = []
for project, color in project_colors.items():
    legend_elements.append(plt.Line2D([0], [0], marker='o', color='w',
                                    markerfacecolor=color, markersize=10,
                                    label=project,
                                    markeredgecolor='white'))

# Add legend with improved styling
leg = ax.legend(handles=legend_elements,
                loc='lower left',
                bbox_to_anchor=(0.02, 0.02),
                title='Research Projects',
                frameon=True,
                facecolor='white',
                edgecolor='gray',
                fontsize=10,
                ncol=2)  # Two columns for better layout
leg.get_title().set_fontsize(12)
leg.get_title().set_fontweight('bold')

# Add title and subtitle
plt.suptitle('Global Research Partnership Network', 
             fontsize=20, 
             fontweight='bold', 
             y=0.95)
plt.title('Circle segments show project participation\n', 
         fontsize=14, 
         pad=20)

# Add data source note
plt.figtext(0.98, 0.02, 
            'Data source: Research Collaboration Network 2025\n'
            'Projects: CHAMNHA, HEAT, HIGH, ENBEL, GHAP, HAPI, BioHEAT, HIGH_Horizons',
            ha='right', 
            fontsize=8, 
            style='italic')

# Save the map with high resolution
plt.savefig('enhanced_geographic_partners_map.png', 
            dpi=300, 
            bbox_inches='tight',
            facecolor='white',
            edgecolor='none')
plt.close() 