# Script to fix the tab-delimited CSV file
# Read the data as tab-delimited
data <- read.delim("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned.csv", 
                   header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Print the data structure
str(data)
print(names(data))

# Save the properly formatted data
write.csv(data, "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_fixed.csv", 
          row.names = FALSE)

print("Fixed data saved to partners_fixed.csv")
print(head(data))
