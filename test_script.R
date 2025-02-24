# Simple R script to demonstrate running from terminal

# Print current time
print("Current time:")
print(Sys.time())

# Create a vector of numbers
numbers <- 1:5
print("Sum of numbers 1 to 5:")
print(sum(numbers))

# Create a simple data frame
df <- data.frame(
  name = c("Alice", "Bob", "Charlie"),
  age = c(25, 30, 35)
)
print("Sample data frame:")
print(df)

# Save a plot
png("test_plot.png")
plot(numbers, main="Simple Plot", xlab="Index", ylab="Value")
dev.off()

print("Script completed successfully!")
