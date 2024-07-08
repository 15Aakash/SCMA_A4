# Load necessary libraries
library(ggplot2)  # For plotting

# Load the dataset
icecream_df <- read.csv("C:/Users/Aakash/Desktop/SCMA/Survey.csv", header = TRUE)

# Display the dataset dimensions and column names
print(paste("Dataset dimensions:", dim(icecream_df)))
print(paste("Column names:", paste(names(icecream_df), collapse = ", ")))

# Select only numeric columns for MDS
# Assuming the first column is non-numeric and should be excluded
numeric_columns <- sapply(icecream_df, is.numeric)
ice <- icecream_df[, numeric_columns]

# Convert columns to numeric if necessary
ice <- as.data.frame(lapply(ice, as.numeric))

# Check for any missing values in the numeric data
if (any(is.na(ice))) {
  print("Missing values found in the dataset. Handling them...")
  # Handle missing values by removing rows with NA
  ice <- na.omit(ice)
}

# Ensure that there are enough rows to compute a distance matrix
if (nrow(ice) <= 1) {
  stop("Not enough data points for distance matrix computation")
}

# Compute the distance matrix
distance_matrix <- dist(ice)

# Verify that the distance matrix is correctly computed
if (length(distance_matrix) == 0) {
  stop("The distance matrix is empty. Check the data for issues.")
}

# Perform Multidimensional Scaling (MDS)
mds_result <- tryCatch({
  cmdscale(distance_matrix, k = 2)
}, error = function(e) {
  stop("Error performing MDS: ", e$message)
})

# Check if MDS result has the expected number of rows
if (nrow(mds_result) != nrow(ice)) {
  stop("The MDS result does not match the number of data points.")
}

# Create a data frame for the MDS result
mds_df <- data.frame(
  Dimension1 = mds_result[, 1],
  Dimension2 = mds_result[, 2],
  Label = rownames(ice)  # Use rownames from the original data frame
)

# Print the MDS data frame for inspection
print("MDS Data Frame:")
print(mds_df)

# Plot the MDS results using ggplot2
ggplot(mds_df, aes(x = Dimension1, y = Dimension2, label = Label)) +
  geom_point(color = 'blue', size = 3) +  # Add points with blue color
  geom_text(vjust = -0.5, hjust = 1.1, size = 5) +  # Annotate points with labels
  labs(title = "MDS Plot",
       x = "Dimension 1",
       y = "Dimension 2") +
  theme_minimal()  # Use minimal theme for better readability
