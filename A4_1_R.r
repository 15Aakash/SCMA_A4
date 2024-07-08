# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "ggplot2", "ggrepel", "caret", "pheatmap")

# Call the function
install_and_load(packages)

# Step 1: Read the dataset
survey_df <- read.csv("C:/Users/Aakash/Desktop/SCMA/Survey.csv", header = TRUE) 

# Step 2: Inspect the dataset
dim(survey_df) 
names(survey_df) 
head(survey_df) 
str(survey_df)

# Step 3: Check for missing values
sum(is.na(survey_df)) 

# Step 4: Select relevant columns for PCA and factor analysis
# Adjust the column indices based on your dataset
sur_int <- survey_df[, 20:46] 
str(sur_int) 
dim(sur_int) 

# Check for multicollinearity
library(caret) # For findCorrelation function
cor_matrix <- cor(sur_int, use = "complete.obs")
high_cor <- findCorrelation(cor_matrix, cutoff = 0.9)
if (length(high_cor) > 0) {
  sur_int <- sur_int[ , -high_cor]
}

# Step 5: Perform PCA and Factor Analysis
library(psych)
library(GPArotation)

# Perform PCA
pca <- principal(sur_int, nfactors = 5, n.obs = nrow(survey_df), rotate = "promax", scores = TRUE)
print(pca)

# Omega hierarchical analysis with alternative factor score estimation method
om.h <- omega(sur_int, n.obs = nrow(survey_df), sl = FALSE, fm = "minres") 
om <- omega(sur_int, n.obs = nrow(survey_df), fm = "minres") 

# Factor Analysis using psych package
factor_analysis <- fa(sur_int, nfactors = 4, rotate = "varimax")
names(factor_analysis) 
print(factor_analysis$loadings, reorder = TRUE)
fa.diagram(factor_analysis)
print(factor_analysis$communality)
print(factor_analysis$scores)

# Heatmap of Factor Loadings
library(pheatmap)
pheatmap(factor_analysis$loadings[, 1:4], 
         cluster_rows = TRUE, 
         cluster_cols = TRUE, 
         color = colorRampPalette(c("blue", "white", "red"))(50), 
         main = "Heatmap of Factor Loadings")

# Step 6: Perform PCA using FactoMineR and visualize
library(FactoMineR)
library(factoextra)

pca_fmr <- PCA(sur_int, scale.unit = TRUE) 
summary(pca_fmr) 

# Scree Plot
fviz_screeplot(pca_fmr, addlabels = TRUE, ylim = c(0, 50))

# Factor Loadings Plot
loadings <- as.data.frame(pca_fmr$var$coord)
loadings$Variables <- rownames(loadings)

# Using ggrepel to avoid overlapping text labels
library(ggrepel)
ggplot(loadings, aes(x = Dim.1, y = Dim.2, label = Variables)) +
  geom_point() +
  geom_text_repel(vjust = 1.5, hjust = 1.5) +
  labs(title = "Factor Loadings Plot", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()

# Biplot
fviz_pca_biplot(pca_fmr, repel = TRUE, 
                col.var = "blue", 
                col.ind = "black",
                label = "var", 
                addEllipses = TRUE,
                ellipse.level = 0.95) +
  labs(title = "PCA Biplot", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()
