import pandas as pd
import matplotlib.pyplot as plt
import networkx as nx
import numpy as np

# Read the CSV file
df = pd.read_csv('partners_data.csv')

# Create a graph
G = nx.Graph()

# Add nodes for each institution
for _, row in df.iterrows():
    # Calculate total projects for node size
    total_projects = sum([row[proj] for proj in ['CHAMNHA', 'HEAT', 'HIGH', 'ENBEL', 'GHAP', 'HAPI', 'BioHEAT', 'HIGH_Horizons']])
    
    # Add node with attributes
    G.add_node(row['Institution'], 
               country=row['Country'],
               projects=total_projects,
               is_funder=row['Funder'])

# Add edges between institutions that share projects
projects = ['CHAMNHA', 'HEAT', 'HIGH', 'ENBEL', 'GHAP', 'HAPI', 'BioHEAT', 'HIGH_Horizons']
for proj in projects:
    institutions = df[df[proj] == 1]['Institution'].tolist()
    for i in range(len(institutions)):
        for j in range(i+1, len(institutions)):
            if G.has_edge(institutions[i], institutions[j]):
                G[institutions[i]][institutions[j]]['weight'] += 1
            else:
                G.add_edge(institutions[i], institutions[j], weight=1)

# Set up the plot
plt.figure(figsize=(20, 20))

# Create layout
pos = nx.spring_layout(G, k=1, iterations=50)

# Draw nodes
node_sizes = [G.nodes[node]['projects'] * 300 for node in G.nodes()]
node_colors = ['red' if G.nodes[node]['is_funder'] else 'lightblue' for node in G.nodes()]

nx.draw_networkx_nodes(G, pos, 
                      node_size=node_sizes,
                      node_color=node_colors,
                      alpha=0.7)

# Draw edges with varying thickness based on weight
edge_weights = [G[u][v]['weight'] for u, v in G.edges()]
nx.draw_networkx_edges(G, pos, 
                      width=[w/2 for w in edge_weights],
                      alpha=0.4)

# Add labels
nx.draw_networkx_labels(G, pos, 
                       font_size=8,
                       font_weight='bold')

# Add title and legend
plt.title('Global Research Partner Network\nNode size = Number of projects, Edge thickness = Number of shared projects',
          fontsize=16, pad=20)

# Add legend
legend_elements = [plt.Line2D([0], [0], marker='o', color='w', 
                             markerfacecolor='lightblue', markersize=15, label='Research Institution'),
                  plt.Line2D([0], [0], marker='o', color='w', 
                             markerfacecolor='red', markersize=15, label='Funding Organization')]
plt.legend(handles=legend_elements, loc='upper left', fontsize=12)

# Remove axes
plt.axis('off')

# Save the plot
plt.savefig('global_partners_network.png', dpi=300, bbox_inches='tight')
plt.close()
