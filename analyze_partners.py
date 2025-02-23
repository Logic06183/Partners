import pandas as pd

# Read the CSV
df = pd.read_csv('partners_data_with_coords.csv')

# Count partners per project (excluding funders)
project_counts = {
    'CHAMNHA': len(df[(df['CHAMNHA'] == 1) & (df['Funder'] == 0)]),
    'HEAT': len(df[(df['HEAT'] == 1) & (df['Funder'] == 0)]),
    'ENBEL': len(df[(df['ENBEL'] == 1) & (df['Funder'] == 0)]),
    'GHAP': len(df[(df['GHAP'] == 1) & (df['Funder'] == 0)]),
    'HAPI': len(df[(df['HAPI'] == 1) & (df['Funder'] == 0)]),
    'BioHEAT': len(df[(df['BioHEAT'] == 1) & (df['Funder'] == 0)]),
    'HIGH_Horizons': len(df[(df['HIGH_Horizons'] == 1) & (df['Funder'] == 0)])
}

print("\nPartners per project (excluding funders):")
for project, count in project_counts.items():
    print(f"{project}: {count} partners")

print("\nDetailed breakdown per project:")
for project in project_counts.keys():
    print(f"\n{project} partners:")
    partners = df[(df[project] == 1) & (df['Funder'] == 0)]['Institution'].tolist()
    for partner in partners:
        print(f"- {partner}") 