# Script to fix the CSV file format
# Read the raw data
raw_data <- read.csv("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned.csv", 
                    header = TRUE, stringsAsFactors = FALSE)

# Print the raw data structure
str(raw_data)
print(names(raw_data))

# Try to split the data if it's in a single column
if (length(names(raw_data)) == 1) {
  # Split the data by commas
  data_split <- strsplit(as.character(raw_data[[1]]), ",")
  
  # Create a new data frame with proper columns
  fixed_data <- data.frame(
    Institution = sapply(data_split, function(x) x[1]),
    City = sapply(data_split, function(x) if(length(x) > 1) x[2] else NA),
    Country = sapply(data_split, function(x) if(length(x) > 2) x[3] else NA),
    lat = sapply(data_split, function(x) {
      if(length(x) > length(x)-1) as.numeric(x[length(x)-1]) else NA
    }),
    lon = sapply(data_split, function(x) {
      if(length(x) > length(x)) as.numeric(x[length(x)]) else NA
    }),
    stringsAsFactors = FALSE
  )
  
  # Save the fixed data
  write.csv(fixed_data, "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_fixed.csv", 
            row.names = FALSE)
  
  print("Fixed data saved to partners_fixed.csv")
  print(head(fixed_data))
} else {
  print("Data already has multiple columns. No fix needed.")
}

# Create a simple dummy dataset for testing if needed
dummy_data <- data.frame(
  Institution = c("University of Witwatersrand", "London School of Hygiene", "University of Washington", 
                 "Karolinska Institute", "University of Ghana", "University of Nairobi"),
  Country = c("South Africa", "United Kingdom", "United States", "Sweden", "Ghana", "Kenya"),
  lat = c(-26.1917, 51.5207, 47.6553, 59.3293, 5.6037, -1.2921),
  lon = c(28.0323, -0.1303, -122.3035, 18.0686, -0.1870, 36.8219),
  HEAT = c(1, 1, 0, 0, 1, 1),
  CHAMNHA = c(1, 1, 1, 0, 0, 0),
  ENBEL = c(0, 1, 0, 1, 0, 0),
  GHAP = c(1, 0, 1, 0, 0, 0),
  HAPI = c(0, 0, 0, 1, 1, 0),
  BioHEAT = c(1, 0, 0, 0, 0, 1),
  HIGH_Horizons = c(0, 1, 0, 0, 0, 0),
  Funder = c(0, 0, 0, 0, 0, 0)
)

# Save the dummy data for testing
write.csv(dummy_data, "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_dummy.csv", 
          row.names = FALSE)

print("Dummy data saved to partners_dummy.csv for testing")
