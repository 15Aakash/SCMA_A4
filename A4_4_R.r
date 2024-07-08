# Load necessary libraries
library(readr)
library(dplyr)
library(car)
library(ggplot2)

# Load the dataset
pizza_data <- read.csv("C:/Users/Aakash/Desktop/SCMA/pizza_data.csv", header = TRUE)

# View the first few rows of the dataset
head(pizza_data)

# Encode categorical variables as factors and clean numerical data
pizza_data <- pizza_data %>%
  mutate(
    brand = as.factor(brand),
    price = as.numeric(gsub("[$,]", "", price)),
    weight = as.numeric(gsub("g", "", weight)),
    crust = as.factor(crust),
    cheese = as.factor(cheese),
    size = as.factor(size),
    toppings = as.factor(toppings),
    spicy = as.factor(spicy)
  )

# Display the structure of the dataset
str(pizza_data)

# Perform linear regression analysis
model <- lm(ranking ~ ., data = pizza_data)
summary(model)

# Extract and display the part-worth utilities (coefficients)
part_worths <- coef(model)
print(part_worths)

# Calculate the importance of each attribute
attributes <- names(pizza_data)[-which(names(pizza_data) == "ranking")]
importance <- sapply(attributes, function(attr) {
  if (is.factor(pizza_data[[attr]])) {
    levels <- levels(pizza_data[[attr]])
    part_worths_attr <- part_worths[grep(attr, names(part_worths))]
    if (length(part_worths_attr) > 1) {
      return(diff(range(part_worths_attr)))
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
})

# Remove NA values
importance <- importance[!is.na(importance)]

# Normalize and calculate percentages
importance <- importance / sum(importance) * 100

# Create a data frame for plotting
importance_df <- data.frame(Attribute = names(importance), Importance = importance)

# Plotting the relative importance of attributes
ggplot(importance_df, aes(x = reorder(Attribute, -Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Relative Importance of Pizza Attributes", x = "Attribute", y = "Importance (%)")

# Bar plots for part-worth utilities of each attribute
for (attr in attributes) {
  if (is.factor(pizza_data[[attr]])) {
    levels <- levels(pizza_data[[attr]])
    part_worths_attr <- part_worths[grep(attr, names(part_worths))]
    
    # Create a data frame for the levels and part-worths
    part_worths_df <- data.frame(Level = levels, Part_Worth = NA)
    for (level in levels) {
      coef_name <- paste(attr, level, sep = "")
      if (coef_name %in% names(part_worths)) {
        part_worths_df$Part_Worth[part_worths_df$Level == level] <- part_worths[coef_name]
      } else {
        warning(paste("No part-worth found for", coef_name))
      }
    }
    
    # Handle missing values by removing them from the plot
    part_worths_df <- na.omit(part_worths_df)
    
  }
}

# Calculate the utility score for each profile
pizza_data$utility_score <- predict(model, newdata = pizza_data)

# Find the combination with maximum utility
max_utility_profile <- pizza_data[which.max(pizza_data$utility_score), ]
print(max_utility_profile)

# Determine the levels being preferred in each attribute
preferred_levels <- sapply(attributes, function(attr) {
  if (is.factor(pizza_data[[attr]])) {
    levels <- levels(pizza_data[[attr]])
    part_worths_attr <- sapply(levels, function(level) {
      coef_name <- paste(attr, level, sep = "")
      if (coef_name %in% names(part_worths)) {
        return(part_worths[coef_name])
      } else {
        return(NA)
      }
    })
    if (length(part_worths_attr) == length(levels)) {
      levels[which.max(part_worths_attr)]
    } else {
      NA
    }
  } else {
    NA
  }
})

# Display the preferred levels for each attribute
print(preferred_levels)

# Scatter plot of utility scores vs. ranking
ggplot(pizza_data, aes(x = utility_score, y = ranking)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Utility Scores vs. Ranking", x = "Utility Score", y = "Ranking") +
  theme_minimal()

