##################################################
### PROG8430                                    ##
### Clustering Demonstration                    ## 
##################################################
#                                               ##
##################################################
# Written by Daniel Beckley
# ID: 8846774
#
##################################################
### Assignment 3                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("C:/Users/Owner/Desktop/AIML/DataAnalysisMath/ass3")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################


#If the library is not already downloaded, download it

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

if(!require(reshape2)){install.packages("reshape2")}
library("reshape2")

if(!require(cluster)){install.packages("cluster")}
library("cluster")

##################################################
### Read in Data                                ##
##################################################

##################################################
### Read data and do preliminary data checks    ##
##################################################

load("A3_data_expense.Rdata")

head(PROG8430_Clst_21F)

# 1. Data Transformation (1 point):

# 1.1 Standardize all the variables using either of the two normalized functions 
# demonstrated in class. Describe why you chose the method you did. (1 point)

# Function to identify and print outliers for each column
identify_outliers <- function(data) {
  for (col_name in names(data)) {
    Q1 <- quantile(data[[col_name]], 0.25)
    Q3 <- quantile(data[[col_name]], 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outliers <- data[[col_name]][data[[col_name]] < lower_bound | data[[col_name]] > upper_bound]
    
    if (length(outliers) > 0) {
      cat("Outliers in", col_name, ":\n")
      print(outliers)
      cat("\n")
    } else {
      cat("No outliers in", col_name, "\n\n")
    }
  }
}

# Apply the function to your dataset
identify_outliers(PROG8430_Clst_21F)

# Function for Min-Max normalization
min_max_normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

standardize <- function(x) {
  return ((x - mean(x)) / sd(x))
}

# Apply Min-Max normalization to the dataset
PROG8430_Clst_21F_normalized <- as.data.frame(lapply(PROG8430_Clst_21F, min_max_normalize))

# View the first few rows of the normalized data
head(PROG8430_Clst_21F_normalized)

# 2. Descriptive data analysis (1 point)
# 2.1. Create graphical summaries of the data (boxplots or histograms) and comment on any 
# observations you make. (1 point)

# Melting the data frame to use with ggplot
PROG8430_Clst_21F_long <- reshape2::melt(PROG8430_Clst_21F_normalized)

# Plotting boxplots for each variable
ggplot(PROG8430_Clst_21F_long, aes(x=variable, y=value)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, lwd=0.5) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 12)) +
  labs(title="Boxplots Showing Distribution", x="Variable", y="Value")

# Adjusting margins
par(mar=c(2, 2, 2, 2))

# Plotting one histogram at a time
for (var in names(PROG8430_Clst_21F)) {
  hist(PROG8430_Clst_21F[[var]], main=paste("Histogram of", var), xlab=var)
  # You might want to pause or hit Enter in the console to view one plot at a time
}



# 3. Clustering the data (4 points)
# 3.1 Using the K-Means procedure, create clusters with k=3,4,5,6,7. 
# You will be using only two variables as your centroids (Food and Work). (2 points)

# Selecting only 'Food' and 'Work' columns for clustering
data_for_clustering <- PROG8430_Clst_21F_normalized[, c("Food", "Work")]

# Initialize an empty list to store k-means results
kmeans_results <- list()

# Initialize an empty vector to store WSS (within-cluster sum of squares)
wss_values <- numeric()

for (k in 3:7) {
  set.seed(123) # Setting seed for reproducibility
  kmeans_result <- kmeans(data_for_clustering, centers=k, iter.max = 10, nstart=10)
  print(kmeans_result)
  kmeans_results[[as.character(k)]] <- kmeans_result
  wss_values[k-2] <- kmeans_result$tot.withinss # k-2 to adjust index since we start from 3
}

# Optional: Print out cluster centers for each k
for (k in 3:7) {
  cat("k =", k, "\n")
  print(kmeans_results[[as.character(k)]]$centers)
  cat("\n")
}

# 3.2 Create the WSS plots as demonstrated in class and select a suitable k value based
# on the "elbow". [NOTE -you can create the chart in Excel]. (2 points)

# Plotting the WSS values
plot(3:7, wss_values, type="b", pch=19, frame=FALSE, 
     xlab="Number of clusters K", ylab="Total within-cluster sum of squares",
     main="Elbow Method for Choosing Optimal K")


# 4. Evaluation of clusters (7 points)

# 4.1 Based on the "k" chosen above, create scatter plot showing the clusters and 
# colour-coded datapoints for each of "k-1", "k", "k+1". For example, if you think 
# the "elbow" is at k=4 create the charts for k=3, k=4 and k=S. (2 points)

plot_clusters <- function(k) {
  set.seed(123)  # Ensure reproducibility
  kmeans_result <- kmeans(data_for_clustering, centers=k, nstart=25)
  
  # Create a data frame with original data and cluster assignments
  data_with_clusters <- cbind(data_for_clustering, Cluster = factor(kmeans_result$cluster))
  
  # Create a data frame for the centers
  centers <- as.data.frame(kmeans_result$centers)
  centers$Cluster <- factor(1:k)
  
  # Plot
  ggplot(data_with_clusters, aes(x=Food, y=Work)) +
    geom_point(aes(color=Cluster, shape=Cluster), alpha=0.5) +  # Data points
    geom_point(data=centers, aes(x=Food, y=Work, color=Cluster, shape=Cluster),  # Cluster centers
               size=3, shape=23) +
    scale_color_brewer(palette="Set1") +
    labs(title=paste("Scatter Plot with", k, "Clusters"), x="Food", y="Work") +
    theme_minimal()
}

# Plots for k=4, k=5, and k=6
plot_clusters(4)
plot_clusters(5)
plot_clusters(6)

# 4.2 Based on the WSS plot (3.2) and the charts (4.1), choose one set of clusters
# that best describes the data. (1 point)



# 4.3 Based on the step 4.2, create summary tables for the segmentation/clustering 
# scheme. (2 points)

set.seed(123)  # Ensure reproducibility
final_kmeans <- kmeans(data_for_clustering, centers=5, nstart=25)
data_with_clusters <- cbind(PROG8430_Clst_21F_normalized, Cluster = factor(final_kmeans$cluster))

# First, we join the cluster assignments with the original data
PROG8430_Clst_21F_with_clusters <- cbind(PROG8430_Clst_21F_normalized, Cluster = data_with_clusters$Cluster)

# Now, we create the summary table
cluster_summary <- PROG8430_Clst_21F_with_clusters %>%
  group_by(Cluster) %>%
  summarise(
    FoodMean = mean(Food),
    EntrMean = mean(Entr),
    EducMean = mean(Educ),
    TranMean = mean(Tran),
    WorkMean = mean(Work),
    HousMean = mean(Hous),
    OthrMean = mean(Othr),
    N = n()
  ) %>%
  arrange(Cluster)

# View the summary table
print(cluster_summary)


# 4.4 Create suitable descriptive names for each cluster. (1 point)


# 4.5 Suggest possible uses for this clustering scheme. (1 point)
