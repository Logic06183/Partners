import pandas as pd

# Read the CSV
df = pd.read_csv('partners_data_with_coords.csv')

# Check for missing coordinates
missing_coords = df[df['lon'].isna() | df['lat'].isna()]

if len(missing_coords) > 0:
    print("\nPartners missing coordinates:")
    for _, row in missing_coords.iterrows():
        print(f"- {row['Institution']} ({row['City']}, {row['Country']})")
    print(f"\nTotal partners missing coordinates: {len(missing_coords)}")
else:
    print("\nAll partners have coordinates!")

# Print summary
print("\nSummary:")
print(f"Total partners: {len(df)}")
print(f"Partners with coordinates: {len(df) - len(missing_coords)}")
print(f"Partners missing coordinates: {len(missing_coords)}")

# Check for potentially invalid coordinates
invalid_coords = df[
    (df['lat'] < -90) | (df['lat'] > 90) |  # Invalid latitude
    (df['lon'] < -180) | (df['lon'] > 180)   # Invalid longitude
]

if len(invalid_coords) > 0:
    print("\nPartners with potentially invalid coordinates:")
    for _, row in invalid_coords.iterrows():
        print(f"- {row['Institution']}: lat={row['lat']}, lon={row['lon']}") 