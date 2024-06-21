##################################################
### PROG8431                                    ##
### Data Analysis Mathematics, Algorithms       ##
##################################################
#                                               ##
##################################################
# Written by Daniel Beckley
# ID: 8846774
#
##################################################
### Basic Set Up                                ##
##################################################
setwd("C:/Users/Owner/Desktop/AIML/DataAnalysisMath/ass4") # Set work directory
load("A4_data_political.Rdata")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")

head(A4_data, n = 5)

# Q1: Reduce Dimensionality
# 1.1.	Apply the Missing Value Filter to remove appropriate columns of data.
# Calculate the percentage of missing values per column
missing_percentage <- sapply(A4_data, function(x) sum(is.na(x)) / length(x))
missing_percentage
# Define a threshold, e.g., 20% missing values
threshold <- 0.20

# Identify columns exceeding the threshold
cols_to_remove <- names(which(missing_percentage > threshold))

# Remove these columns from the dataset
A4_data <- A4_data[, !(names(A4_data) %in% cols_to_remove)]


# 1.2.	Apply the Low Variance Filter to remove appropriate columns of data.
# Identify numeric columns only
numeric_columns <- sapply(A4_data, is.numeric)

# Calculate variance only for numeric columns
variances <- sapply(A4_data[, numeric_columns], var, na.rm = TRUE)
variances
# Define a low variance threshold
low_variance_threshold <- 0.01

# Identify low variance columns among numeric columns
low_variance_cols <- names(variances[variances < low_variance_threshold])

# Remove low variance numeric columns from the dataset
A4_data <- A4_data[, !(names(A4_data) %in% low_variance_cols)]


# 1.3 Apply the High Correlation Filter to remove appropriate columns of data.
# Select only numeric columns from the dataset
numeric_data <- A4_data[sapply(A4_data, is.numeric)]

# Calculate the correlation matrix for numeric columns only
correlation_matrix <- cor(numeric_data, use="complete.obs")
correlation_matrix

# Use corrgram to visualize the correlations
corrgram(numeric_data, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)

# Manually specify the variables to remove based on your analysis
# For example, if 'var1' and 'var3' are highly correlated, and you decide to remove 'var3'
vars_to_remove <- c("time3")  # replace 'var3' with the actual variable name you want to remove

# Remove the specified variables from the dataset
A4_data <- A4_data[, !(names(A4_data) %in% vars_to_remove)]


# Q2: Variable transform
# 1	any variables that are required to conduct the regression analysis,
#e.g. categorical variables to dummies.

A4_data <- as.data.frame(lapply(A4_data, function(x) {
  if (is.factor(x) || is.character(x)) {
    return(model.matrix(~ x - 1))  # '-1' to avoid creating an intercept column
  } else {
    return(x)
  }
}))
A4_data


numeric_non_binary_columns <- sapply(A4_data, function(x) {
  is.numeric(x) && length(unique(x)) > 2
})
numeric_non_binary_columns

plot_numeric_boxplots <- function(data, cols_per_plot=6) {
  # Identify numeric columns that are not binary
  numeric_non_binary_column_names <- names(data[, numeric_non_binary_columns])
  
  # Calculate the number of plots needed
  num_plots <- ceiling(length(numeric_non_binary_column_names) / cols_per_plot)
  
  # Loop through each plot
  for (plot_num in 1:num_plots) {
    # Calculate the range of column indices for the current batch
    col_range_start <- (plot_num - 1) * cols_per_plot + 1
    col_range_end <- min(plot_num * cols_per_plot, length(numeric_non_binary_column_names))
    
    # Select column names for the current batch
    batch_column_names <- numeric_non_binary_column_names[col_range_start:col_range_end]
    
    # Adjust layout settings for the current batch of plots
    # Determine rows and cols for mfrow based on the number of plots in the current batch
    rows <- ceiling(length(batch_column_names) / 3)
    cols <- min(3, length(batch_column_names))
    
    par(mfrow=c(rows, cols), mar=c(3, 3, 3, 3))  # Adjust margins as needed
    
    # Loop through the numeric, non-binary columns in the current batch and plot boxplots
    for (i in batch_column_names) {
      boxplot(data[[i]], main=i)
    }
    
    # Reset par settings after plotting each batch
    par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)  # Default values
  }
}


plot_numeric_boxplots(A4_data)

find_outliers_iqr_counts <- function(data) {
  # Initialize a named vector to store counts of outliers for each numeric non-binary column
  outlier_counts <- numeric()
  
  # Loop through each column in the dataset
  for (col_name in names(data)) {
    # Check if the column is numeric and non-binary
    if (is.numeric(data[[col_name]]) && length(unique(data[[col_name]])) > 2) {
      # Calculate the IQR
      Q1 <- quantile(data[[col_name]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[col_name]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      # Define the outlier thresholds
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      # Identify the indices of outliers
      outlier_indices <- which(data[[col_name]] < lower_bound | data[[col_name]] > upper_bound)
      
      # Store the count of outliers in the vector, if there are any
      if (length(outlier_indices) > 0) {
        outlier_counts[col_name] <- length(outlier_indices)
      }
    }
  }
  
  # Filter out columns with no outliers
  outlier_counts <- outlier_counts[outlier_counts > 0]
  
  return(outlier_counts)
}

# Applying the function to your dataset
outlier_counts <- find_outliers_iqr_counts(A4_data)
outlier_counts

# 3.2.	Comment on any outliers you see and deal with them appropriately.
remove_outliers_from_columns <- function(data, columns) {
  # Initialize a vector to store indices of all outliers
  all_outlier_indices <- integer()
  
  # Loop through each specified column
  for (col_name in columns) {
    # Check if the column exists and is numeric
    if (col_name %in% names(data) && is.numeric(data[[col_name]])) {
      # Calculate the IQR
      Q1 <- quantile(data[[col_name]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[col_name]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      # Define the outlier thresholds
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      # Identify indices of outliers in this column
      outlier_indices <- which(data[[col_name]] < lower_bound | data[[col_name]] > upper_bound)
      
      # Store the indices of outliers
      all_outlier_indices <- c(all_outlier_indices, outlier_indices)
    }
  }
  
  # Remove duplicates and sort the outlier indices
  all_outlier_indices <- unique(all_outlier_indices)
  
  # Remove all identified outlier rows at once
  clean_data <- data[-all_outlier_indices, ]
  
  return(clean_data)
}

# Specify the columns from which you want to remove outliers
columns_with_outliers <- c("n.child", "housing", "other", "score", "scr")

# Remove outliers from these columns
clean_A4_data <- remove_outliers_from_columns(A4_data, columns_with_outliers)




# Q4: Exploratory Analysis
# 4.1.	Correlations: Create both numeric and graphical correlations
# Compute correlation matrix
correlation_matrix <- cor(clean_A4_data[, sapply(clean_A4_data, is.numeric)], use="complete.obs")
correlation_matrix

# Visualize the correlation matrix using a heatmap
heatmap(correlation_matrix, symm = TRUE, Rowv = NA, Colv = NA, scale = "none", margins = c(5, 5))
corrgram(clean_A4_data, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)

# 4.2.	Comment on noteworthy correlations you observe. Are these surprising? Do they make
# sense?
  
  
# Q5: Simple Linear Regression
# 5.1.	Create a simple linear regression model using Pol as the dependent variable
# and score as the independent. Create a scatter plot of the two variables and overlay the regression line.
# Linear regression model: Pol as dependent variable, score as independent variable
# Model using Pol as dependent variable and score as independent variable
model1 <- lm(Pol ~ score, data=clean_A4_data)
model1

# Plotting the model
plot(clean_A4_data$score, clean_A4_data$Pol, main="Pol ~ score",
     xlab="score", ylab="Pol", pch=19, col="blue")
abline(model1, col="red")



# 5.2.	Create a simple linear regression model using Pol as the dependent variable and scr as 
# the independent. Create a scatter plot of the two variables and overlay the regression line.
# Linear regression model: Pol as dependent variable, scr as independent variable
# Model using Pol as dependent variable and scr as independent variable
model2 <- lm(Pol ~ scr, data=clean_A4_data)
model2

# Plotting the model
plot(clean_A4_data$scr, clean_A4_data$Pol, main="Pol ~ scr",
     xlab="scr", ylab="Pol", pch=19, col="blue")
abline(model2, col="red")


# 5.3.	Compare the models. Which model is superior? Why?
# Summary of the first model (Pol ~ score)
summary(model1)

# Summary of the second model (Pol ~ scr)
summary(model2)

  
# 6: Model Development
# 6.1.	Multivariate linear regression- create two models using two automatic variable selection 
# techniques discussed in class(Full(baseline), Backward).
# Full model with all predictors
full_model <- lm(Pol ~ ., data=clean_A4_data, na.action = na.omit)
full_model
summary(full_model)

# Backward elimination model
backward_model <- step(lm(Pol ~ ., data=clean_A4_data), direction="backward", details=TRUE)
backward_model

# 6.2.	For each model interpret and comment on the five main measures (Your commentary should be yours,
# not simply copied from example): 1. F-Stat 2. R-Squared value 3. Residuals 4. Significant variables 5. Variable Coefficient
# Full model summary
summary(full_model)

# Backward elimination model summary
summary(backward_model)


# 6.3.	Model evaluation: For both models evaluate the main assumptions of regression and evaluate model performance
# Example for full model, repeat for backward model
par(mfrow=c(2, 2))
plot(full_model)

plot(backward_model)




# 6.4.	Based on your preceding analysis, recommend which of the models should be used.


