import pandas as pd

# Read the original data
df_original = pd.read_csv('partners_data.csv')

# Print University of Washington's data
print("Original Data:")
uw_data = df_original[df_original['Institution'] == 'University of Washington']
print(uw_data)

# Read the coordinates data
df_coords = pd.read_csv('partners_data_with_coords.csv')

# Print University of Washington's data with coordinates
print("\nData with Coordinates:")
uw_coords = df_coords[df_coords['Institution'] == 'University of Washington']
print(uw_coords)

# Add coordinates for University of Washington if missing
if len(uw_coords) == 0:
    # Seattle coordinates
    seattle_coords = {'lon': -122.3321, 'lat': 47.6062}
    
    # Update the coordinates file
    df_coords.loc[df_coords['Institution'] == 'University of Washington', 'lon'] = seattle_coords['lon']
    df_coords.loc[df_coords['Institution'] == 'University of Washington', 'lat'] = seattle_coords['lat']
    
    # Save updated coordinates
    df_coords.to_csv('partners_data_with_coords.csv', index=False)
    print("\nAdded coordinates for University of Washington") 