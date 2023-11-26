# Install and load required packages
install.packages(c("ggplot2", "readxl"))
library(ggplot2)
library(readxl)
install.packages("corrplot")
library(corrplot)

# Function to recommend the city based on minimum total cost
recommendCity <- function(data) {
  data$total_cost <- data$Expenses * data$Duration
  min_cost_index <- which.min(data$total_cost)
  recommended_city <- data$City[min_cost_index]
  recommended_option <- data[min_cost_index, ]
  cat("The recommended city for the best combination of low expenses and short duration is:", recommended_city, "\n")
  print(recommended_option)
  print(travel_data)
}


# Function to create plots
createPlots <- function(travel_datadata) {
  # Bar plot for Expenses by City
  ggplot(travel_data, aes(x = City, y = Expenses, fill = City)) +
    geom_bar(stat = "identity") +
    labs(title = "Travel Expenses by City", x = "City", y = "Expenses")

  
  # Scatter plot for Expenses vs. Duration
  ggplot(travel_data, aes(x = Expenses, y = Duration, color = City)) +
    geom_point(size = 3) +
    labs(title = "Scatter Plot of Expenses vs. Duration", x = "Expenses", y = "Duration")
  

  # Box plot for Expenses by City
  ggplot(travel_data, aes(x = City, y = Expenses, fill = City)) +
    geom_boxplot() +
    labs(title = "Box Plot of Travel Expenses by City", x = "City", y = "Expenses")

  
  # Line plot for Duration trend across Cities
  ggplot(travel_data, aes(x = City, y = Duration, group = 1)) +
    geom_line(color = "blue") +
    geom_point(color = "red", size = 3) +
    labs(title = "Line Plot of Duration Trend Across Cities", x = "City", y = "Duration")

  
  # Scatter plot with Trendline for Expenses vs. Duration
  ggplot(travel_data, aes(x = Expenses, y = Duration, color = City)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "Scatter Plot with Trendline for Expenses vs. Duration", x = "Expenses", y = "Duration")

  
  # Pie chart for Distribution of Expenses across Cities
  ggplot(travel_data, aes(x = "", y = Expenses, fill = City)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = "Pie Chart of Expenses Distribution Across Cities", x = NULL, y = NULL) +
    theme_void()
}


# Histogram of Expenses
ggplot(travel_data, aes(x = Expenses)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Travel Expenses", x = "Expenses", y = "Frequency")


# Correlation Heatmap
corr_matrix <- cor(travel_data[, c("Expenses", "Duration")])
corrplot(corr_matrix, method = "color", type = "upper", addCoef.col = "black")


# Scatter Plot Matrix
pairs(travel_data[, c("Expenses", "Duration")])


# Load data from Excel file
file_path <- "/Users/manasamugdha/Downloads/your_file.xlsx"
travel_data <- read_excel(file_path)

# Display recommended city and details
recommendCity(travel_data)

# Create and display plots
createPlots(travel_data)
