# Function to install and load packages automatically
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("cluster", "FactoMineR", "factoextra", "pheatmap")

# Install and load the packages
install_and_load(packages)

# Load the survey data from the specified CSV file
survey_df <- read.csv("C:/Users/Aakash/Desktop/SCMA/Survey.csv", header = TRUE)

# Extract the relevant columns for the cluster analysis
# Assuming columns 20 to 46 contain the data for clustering
sur_int <- survey_df[, 20:46] 

# Display the first few rows of the extracted data to understand its structure
head(sur_int)

# Perform K-means clustering
# Determine the optimal number of clusters using the gap statistic
library(factoextra)
fviz_nbclust(sur_int, kmeans, method = "gap_stat")

# Set a seed for reproducibility
set.seed(123)

# Apply K-means clustering with 4 clusters
km.res <- kmeans(sur_int, centers = 4, nstart = 25)

# Visualize the clustering result
fviz_cluster(km.res, data = sur_int, palette = "jco", ggtheme = theme_minimal())

# Perform hierarchical clustering
# Compute the distance matrix
dist_matrix <- dist(sur_int)

# Perform hierarchical clustering using Ward's method
res.hc <- hclust(dist_matrix, method = "ward.D2")

# Visualize the hierarchical clustering as a dendrogram
fviz_dend(res.hc, cex = 0.5, k = 4, palette = "jco")

# Generate a heatmap to visualize the clustering result
library(pheatmap)

# Transpose the data matrix for better visualization in the heatmap
pheatmap(t(sur_int), cutree_cols = 4, main = "Heatmap of Clustering Results")


