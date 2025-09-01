# Load required libraries
library(ggplot2)

# Sample data: Generate synthetic lead exposure and outcome values
set.seed(42)
years_in_school <- runif(100, min = 1, max = 16)  # Random lead exposure values
outcome <- 40 + 2 * years_in_school + rnorm(100, mean = 2, sd = 4)  # Simulated inverse relationship

# Create a dataframe
data <- data.frame(years_in_school, outcome)

# Fit OLS regression model
model <- lm(outcome ~ years_in_school, data = data)
data$predicted_outcome <- predict(model)  # Get predicted values

# Create the plot: OLS true 
plot0 <- ggplot(data, aes(x = years_in_school, y = outcome)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +  # Scatter plot
  labs(title = "", x = "years_in_school", y = "life expectancy") +  # Labels
  theme_minimal(base_size = 14)  # Clean Theme

plot0 

plot <- ggplot(data, aes(x = years_in_school, y = outcome)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 2) +  # OLS Best Fit Line
  geom_segment(aes(xend = years_in_school, yend = predicted_outcome), color = "goldenrod", linewidth = 0.5) +  # Vertical lines
  labs(title = "", x = "years_in_school", y = "life expectancy") +  # Labels
  theme_minimal(base_size = 14)  # Clean Theme

plot 

#### Create the plot: OLS: wrong line

# Define a custom (non-best-fit) regression line
custom_intercept <- 39   # Adjusted intercept (manipulated)
custom_slope <- 3     # Adjusted slope (manipulated)
data$custom_outcome <- custom_intercept + custom_slope * data$years_in_school  # Compute values for the custom line

# Create the plot: without vertical lines

plot3 <- ggplot(data, aes(x = years_in_school, y = outcome)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +  # Scatter plot
  geom_abline(intercept = custom_intercept, slope = custom_slope, color = "blue", linewidth = 1.5) +  # Custom Line
  labs(title = "", x = "years_in_school", y = "life expectancy") + 
  theme_minimal(base_size = 14)  # Clean Theme

plot3

# Create the plot: with vertical lines
plot2 <- ggplot(data, aes(x = years_in_school, y = outcome)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +  # Scatter plot
  geom_abline(intercept = custom_intercept, slope = custom_slope, color = "blue", linewidth = 1.5) +  # Custom Line
  geom_segment(aes(xend = years_in_school, yend = custom_outcome), color = "goldenrod", linewidth = 0.5) +  # Vertical lines to custom line
  labs(title = "", x = "years_in_school", y = "life expectancy") + 
  theme_minimal(base_size = 14)  # Clean Theme

plot2


###############################################################################


library(ggplot2)
library(dplyr)

# Define grid size
n_rows <- 10
n_cols <- 10

# Create a data frame for the grid points
grid_data <- expand.grid(x = 1:n_cols, y = 1:n_rows)

# Base plot
plot0 <- ggplot(grid_data, aes(x = x, y = y)) +
  geom_point(size = 8, shape = 21, color = "black", stroke = 1) +  # Stylish dots
  coord_fixed() +  # Maintain aspect ratio
  theme_void() +  # Remove axes and grid
  theme(legend.position = "none")  # Hide legend

plot0

# Plot with 10 random points
num_selected <- 10  # Change to how many dots you want to highlight
selected_indices <- sample(nrow(grid_data), num_selected)
grid_data$selected <- "normal"
grid_data$selected[selected_indices] <- "selected"

plot1 <- ggplot(grid_data, aes(x = x, y = y)) +
  geom_point(aes(fill = selected), size = 8, shape = 21, color = "black", stroke = 1) +  # Stylish dots
  scale_fill_manual(values = c("selected" = "blue", "normal" = "white")) +  # Red for selected, dark blue for others
  coord_fixed() +  # Maintain aspect ratio
  theme_void() +  # Remove axes and grid
  theme(legend.position = "none")  # Hide legend

plot1


####################################

# Line of best fit: randomly choose 10 units

# Load required libraries
library(ggplot2)
library(dplyr)

# Generate synthetic years_in_school and test score data
set.seed(42)
years_in_school <- runif(100, min = 1, max = 16)  # Random years_in_school values
outcome <- 40 + 2 * years_in_school + rnorm(100, mean = 2, sd = 4)  # Simulated inverse relationship

# Create a dataframe
data <- data.frame(years_in_school, outcome)

# Randomly select 10 points
sample_indices <- sample(1:nrow(data), 10)  # Randomly choose 10 indices
sampled_data <- data[sample_indices, ]  # Subset of 10 points

# Fit OLS regression model to only the 10 sampled points
sample_model <- lm(outcome ~ years_in_school, data = sampled_data)
sampled_data$predicted_outcome <- predict(sample_model)  # Predicted values for sampled points

# Create the plot: Original data + Highlighted Sample
plot <- ggplot(data, aes(x = years_in_school, y = outcome)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +  # All points in black
  geom_point(data = sampled_data, aes(x = years_in_school, y = outcome), 
             color = "blue", size = 4, alpha = 0.9) +  # Highlighted sampled points
  geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 2) +  # OLS Best Fit Line
  geom_smooth(data = sampled_data, aes(x = years_in_school, y = outcome), 
              method = "lm", color = "blue", se = FALSE, linewidth = 2, linetype = "dashed") +  # OLS Best Fit Line for sampled points
  labs(title = "", x = "years_in_school", y = "life expectancy") +  # Labels
  theme_minimal(base_size = 14)  # Clean Theme

plot










################ Monte-Carlo Simulations #############################


library(ggplot2)
library(dplyr)

# Generate synthetic years_in_school and test score data
set.seed(42)
years_in_school <- runif(100, min = 1, max = 16)  # Random years_in_school values
outcome <- 40 + 2 * years_in_school + rnorm(100, mean = 2, sd = 4)  # Simulated inverse relationship

# Create a dataframe
data <- data.frame(years_in_school, outcome)

# Fit OLS regression model for the full dataset
full_model <- lm(outcome ~ years_in_school, data = data)

# Get predictions across years_in_school range for full dataset
years_in_school_seq <- seq(min(data$years_in_school), max(data$years_in_school), length.out = 100)
full_predictions <- predict(full_model, newdata = data.frame(years_in_school = years_in_school_seq))
full_regression_line <- data.frame(years_in_school = years_in_school_seq, outcome = full_predictions)

# Number of bootstrap samples
num_samples <- 1000
sample_size <- 10

# Store regression lines
regression_lines <- data.frame()

set.seed(123)  # Set seed for reproducibility
for (i in 1:num_samples) {
  sample_indices <- sample(1:nrow(data), sample_size)  # Randomly select 10 points
  sampled_data <- data[sample_indices, ]  # Subset
  
  # Fit OLS regression
  sample_model <- lm(outcome ~ years_in_school, data = sampled_data)
  
  # Get predictions across years_in_school range
  predictions <- predict(sample_model, newdata = data.frame(years_in_school = years_in_school_seq))
  
  # Store lines
  regression_lines <- rbind(regression_lines, data.frame(years_in_school = years_in_school_seq, outcome = predictions, draw = i))
}

# Plot all 1000 sample lines + full dataset line
plot <- ggplot(data, aes(x = years_in_school, y = outcome)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +  # All data points in black
  geom_line(data = regression_lines, aes(x = years_in_school, y = outcome, group = draw), 
            color = "blue", alpha = 0.05) +  # 1000 regression lines (low opacity)
  geom_line(data = full_regression_line, aes(x = years_in_school, y = outcome), 
            color = "red", linewidth = 1.5) +  # True OLS Best-Fit Line (bold red)
  labs(title = "", 
       x = "years_in_school", y = "life expectancy") +  # Labels
  theme_minimal(base_size = 14)  # Clean Theme


plot