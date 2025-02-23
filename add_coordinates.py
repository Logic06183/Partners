import pandas as pd
from geopy.geocoders import Nominatim
from geopy.extra.rate_limiter import RateLimiter
import time

# Read the CSV file
df = pd.read_csv('partners_data.csv')

# Initialize the geocoder
geolocator = Nominatim(user_agent="my_app")
geocode = RateLimiter(geolocator.geocode, min_delay_seconds=1)

# Create empty columns for coordinates
df['lon'] = None
df['lat'] = None

# Get coordinates for each city
for idx, row in df.iterrows():
    try:
        location = geocode(f"{row['City']}, {row['Country']}")
        if location:
            df.at[idx, 'lon'] = location.longitude
            df.at[idx, 'lat'] = location.latitude
            print(f"Found coordinates for {row['City']}, {row['Country']}")
        else:
            print(f"Could not find coordinates for {row['City']}, {row['Country']}")
        time.sleep(1)  # Be nice to the geocoding service
    except Exception as e:
        print(f"Error with {row['City']}, {row['Country']}: {e}")

# Save the updated CSV
df.to_csv('partners_data_with_coords.csv', index=False)
print("Saved updated CSV with coordinates") 