##################################################
### PROG8431                                    ##
### Data Analysis Mathematics, Algorithms       ##
##################################################
#                                               ##
##################################################
# Written by David Marsh
# ID: 123456789
#
##################################################
### Basic Set Up                                ##
##################################################
setwd("C:/Users/Owner/Desktop/AIML/DataAnalysisMath/ass1") # Set work directory
load("A1_data.Rdata")

head(PROG8431_Assign1_Explore, n = 5)

## PART 1
# 1a Create a table to show the total income by each category of marital status.
total_income_by_status <- aggregate(income ~ m.status, data = PROG8431_Assign1_Explore, sum)
total_income_by_status

# 1b Which status has the highest total income?
highest_income_status <- total_income_by_status[which.max(total_income_by_status$income), ]
highest_income_status


#2a Calculate the mean age of respondents born in Asia.
mean_age_asia <- mean(PROG8431_Assign1_Explore$age[PROG8431_Assign1_Explore$nation == "Asia"], na.rm = TRUE)
mean_age_asia

#2b Calculate the mean age of respondents born in Asia weighted by the number of children they have.
weighted_mean_age_asia <- weighted.mean(PROG8431_Assign1_Explore$age[PROG8431_Assign1_Explore$nation == "Asia"], PROG8431_Assign1_Explore$n.child[PROG8431_Assign1_Explore$nation == "Asia"], na.rm = TRUE)
weighted_mean_age_asia


#3a Create a table to show the mean score on the political awareness test for males compared to females.
mean_score_by_gender <- aggregate(score ~ gender, data = PROG8431_Assign1_Explore, mean)
mean_score_by_gender

#3b Which has a higher score?
higher_score_gender <- mean_score_by_gender[which.max(mean_score_by_gender$score), ]
higher_score_gender

#4a Calculate the 34th and 63rd percentiles of percentage of time taken on the test.
time_percentiles <- quantile(PROG8431_Assign1_Explore$time1, probs = c(0.34, 0.63), na.rm = TRUE)
time_percentiles


#PART 2
#1a Create a pie chart showing the number of respondents by Political Affiliation.
# Create the table
counts <- table(PROG8431_Assign1_Explore$political)
# Calculate percentages
percentages <- round(counts / sum(counts) * 100, 1)
# Create labels with percentages
labels <- paste(names(counts), "\n", percentages, "%", sep="")
# Define colors for each slice
colors <- rainbow(length(counts))
# Plot the pie chart
pie(counts, labels = labels, main = "Respondents by Political Affiliation", col = colors, cex = 0.8)
# Add a legend
legend("bottomleft", legend = names(counts), fill = colors, cex = 0.8, title = "Political Affiliation")

#1b Which Political Affiliation contains the most respondents (remember each row of your study file represents one respondent)?
most_affiliation <- names(which.max(counts))
most_affiliation

#1c Which Political Affiliation has the fewest respondents?
fewest_affiliation <- names(which.min(counts))
fewest_affiliation

#2a Create a table that shows the percentage of respondents from each Region that are in the Treatment group.
treatment_by_region <- aggregate(group ~ nation, data = PROG8431_Assign1_Explore, function(x) mean(x == "treat") * 100)
treatment_by_region

#2b Which region has the highest percentage of people in the Treatment group?
highest_treatment_region <- treatment_by_region[which.max(treatment_by_region$group), ]
highest_treatment_region

#2c Which region has the lowest percentage of people in the Treatment group?
lowest_treatment_region <- treatment_by_region[which.min(treatment_by_region$group), ]
lowest_treatment_region

#3a Create a bar chart showing the mean Standardized Test Score on the Political Awareness Test for each Region.
mean_score_by_region <- aggregate(scr ~ nation, data = PROG8431_Assign1_Explore, mean)
mean_score_by_region
# Calculate the range for y-axis
y_max <- max(mean_score_by_region$scr) * 1.2  # Extend the maximum by 20% for some headroom

# Create the bar plot with an extended y-axis range
barplot(mean_score_by_region$scr,
        names.arg = mean_score_by_region$nation,
        main = "Mean Standardized Test Score by Region",
        ylim = c(0, y_max),  # Set the y-axis limits
        col = "skyblue",     # Optional: Color the bars for better visualization
        border = "white"     # Optional: Set the border color of the bars
)

#3b Which Region has the lowest mean score?
lowest_score_region <- mean_score_by_region[which.min(mean_score_by_region$scr), ]
lowest_score_region

#3c Which Region has the highest mean score?
highest_score_region <- mean_score_by_region[which.max(mean_score_by_region$scr), ]
highest_score_region

#4a Create a histogram with 5 bins showing the distribution of the percentage of household income going to food.
# Create the histogram with adjusted y-axis limits
hist(PROG8431_Assign1_Explore$food, breaks = 5, main = "Distribution of Income Spent on Food", xlab = "Percentage of Income Spent on Food", col = "lightgreen", ylim = c(0, max(hist(PROG8431_Assign1_Explore$food, breaks = 5, plot = FALSE)$counts) * 1.2))

#4b Which range of values has the highest frequency?

#5a Create a sequence of box plots showing the distribution of income separated by marital status.
boxplot(income ~ m.status, data = PROG8431_Assign1_Explore, main = "Income Distribution by Marital Status", xlab = "Marital Status", ylab = "Income")

#5b 5c
# According to the charts, which martial status has the highest average income?
# c. Which marital status has the lowest average income?
# Calculate summary statistics for income by marital status
income_summary_by_mstatus <- aggregate(income ~ m.status, data = PROG8431_Assign1_Explore, summary)
# View the result
print(income_summary_by_mstatus)

#5d
income_variability_by_mstatus <- aggregate(income ~ m.status, data = PROG8431_Assign1_Explore, function(x) sd(x, na.rm = TRUE))
# To find the marital status with the greatest variability in income
max_variability_status <- income_variability_by_mstatus[which.max(income_variability_by_mstatus$income), ]
max_variability_status

# 6a Create a histogram for income.
hist(PROG8431_Assign1_Explore$income, main="Histogram of Income", xlab="Income", border="white")

# 6b. Create a histogram for standardized score.
hist(PROG8431_Assign1_Explore$scr, main="Histogram of Standardized Scores", xlab="Standardized Score", border="white")

# 6c. Create a scatter plot showing the relationship between the income and standardized score. (note: income should be on the x-axis, standardized score should be the y-axis)
plot(PROG8431_Assign1_Explore$income, PROG8431_Assign1_Explore$scr, main="Scatter Plot of Income vs. Standardized Score", xlab="Income", ylab="Standardized Score", pch=19)

# 6d. What conclusions, if any, can you draw from the chart?


# 6e. Calculate a correlation coefficient between these two variables. What conclusion you draw from it?
correlation_coefficient <- cor(PROG8431_Assign1_Explore$income, PROG8431_Assign1_Explore$scr, use="complete.obs")
correlation_coefficient

