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
setwd("C:/Users/Owner/Desktop/AIML/DataAnalysisMath/ass5") # Set work directory
load("A5_tumor_data.Rdata")

if(!require(pROC)){install.packages("pROC")}
library("pROC")

if(!require(MASS)){install.packages("MASS")}
library("MASS")

if(!require(klaR)){install.packages("klaR")}
library("klaR")

head(Tumor_data, n = 5)

#1: Data Preparation (2 points)
# 1.1. As demonstrated in class and conducted in previous assignments (MLR), make sure that the data is free from outliers 
# or unnecessary data.

# Check for missing values
sum(is.na(Tumor_data))

boxplot(Tumor_data)

# 2: Exploratory Analysis (2 points)
# 2.1	Correlations: Create numeric correlations and comment on what you see. Are there co-linear variables?

# Calculate and print the correlation matrix
correlations <- cor(Tumor_data)  
print(round(correlations, 2))

# 2.2	Identify the two most significant predictors of tumors and provide statistical evidence (in addition 
# to the correlation coefficients) that suggest they are associated with tumors (Think of the contingency tables)

for(variable in colnames(Tumor_data)) {
  if(variable != "Out") {  # Exclude the outcome variable itself
    cat("\nContingency Table for:", variable, "\n")
    
    # Create the contingency table for 'Out' and each variable
    Tbl_Out_Variable <- table(Tumor_data$Out, Tumor_data[[variable]], dnn=list("Out", variable))
    
    # Print the contingency table
    print(Tbl_Out_Variable)
    
    # Calculate and print column percentages
    col_percentages <- prop.table(Tbl_Out_Variable, 2) * 100  # Multiply by 100 for percentage
    print(col_percentages)
    
    # Perform the Chi-squared test on the contingency table
    chi_test <- chisq.test(Tbl_Out_Variable)
    
    cat("Chi-squared Test Results for:", variable, "\n")
    print(chi_test)
  }
}



# 3: Logistic Regression (5 points)
# 3.1	Use the criteria option in the glm function to fit the model (stepwise)
model_logistic <- glm(Out ~ ., data = Tumor_data, family = "binomial")
step_model <- step(model_logistic, direction="both")  # Stepwise model selection

model_logistic
step_model
# 3.2	Summarize the results in a Confusion Matrix

# Predict probabilities
resp_logistic <- predict(step_model, type = "response")

# Convert probabilities to class predictions using a threshold of 0.5
Class_logistic <- ifelse(resp_logistic > 0.5, 1, 0)

# Create the confusion matrix
T1 <- table(Tumor_data$Out, Class_logistic, dnn = list("Actual Out", "Predicted"))

# Print the confusion matrix
print(T1)
# 3.3	Based on the confusion matrix, calculate and comment on:
#  a. Accuracy b. Specificity c. Sensitivity d. Precision
# Extracting the elements from the confusion matrix
TN <- T1[1, 1]
FP <- T1[1, 2]
FN <- T1[2, 1]
TP <- T1[2, 2]

# Calculating the metrics and converting them to percentages
Accuracy <- ((TP + TN) / sum(T1)) * 100
Specificity <- (TN / (TN + FP)) * 100
Sensitivity <- (TP / (TP + FN)) * 100
Precision <- (TP / (TP + FP)) * 100

# Printing the metrics as percentages
cat("Accuracy (%): ", Accuracy, "\n")
cat("Specificity (%): ", Specificity, "\n")
cat("Sensitivity (Recall) (%): ", Sensitivity, "\n")
cat("Precision (%): ", Precision, "\n")

# 3.4	Create the ROC curve and calculate the AUC. Comment on how you interpret them.

# Generating predictions as probabilities
predictions_prob <- predict(step_model, newdata = Tumor_data, type = "response")

# Creating the ROC curve
roc_obj <- roc(Tumor_data$Out, predictions_prob)

# Plotting ROC curve
plot(roc_obj, main="ROC Curve")
abline(a=0, b=1, col="red")  # Adding reference line

# Calculating AUC
auc_value <- auc(roc_obj)
cat("AUC: ", auc_value, "\n")

# 3.5	Calculate the time (in seconds) it took to train the model and the prediction time. Including
# the running time in your summary.
# Timing the model training
start_time <- Sys.time()
step_model <- step(glm(Out ~ ., data = Tumor_data, family = "binomial"), direction = "both")
end_time <- Sys.time()
training_time <- end_time - start_time
cat("Time taken to train the model (seconds): ", training_time, "\n")

# Timing the prediction
start_time <- Sys.time()
predictions_prob <- predict(step_model, newdata = Tumor_data, type = "response")
end_time <- Sys.time()
prediction_time <- end_time - start_time
cat("Time taken to make predictions (seconds): ", prediction_time, "\n")


# 4: Naïve-Bayes Classification (5 points)
# 4.1	Transform the variables as necessary for N-B classification.
NB_Tumor_data <- transform(Tumor_data, Out = as.factor(Out), Age = as.factor(Age), Sex = as.factor(Sex), Bone = as.factor(Bone), Marrow = as.factor(Marrow), Lung = as.factor(Lung), Pleura = as.factor(Pleura), Liver = as.factor(Liver), Brain = as.factor(Brain), Skin = as.factor(Skin), Neck = as.factor(Neck), Supra = as.factor(Supra), Axil = as.factor(Axil), Media = as.factor(Media))
NB_Tumor_data

# 4.2	Use all the variables in the dataset to fit a Naïve-Bayesian classification model.
nb_model <- NaiveBayes(Out ~ ., data = NB_Tumor_data, na.action = na.omit)
nb_model

# 4.3	Summarize the results in a Confusion Matrix.
nb_predictions <- predict(nb_model, NB_Tumor_data)

conf_matrix <- table(Actual = NB_Tumor_data$Out, Predicted = nb_predictions$class)

# Print the confusion matrix
print(conf_matrix)

# 4.4	Based on the confusion matrix, calculate and comment on:
#  a. Accuracy b. Specificity c. Sensitivity d. Precision

TN <- conf_matrix[1, 1]
FP <- conf_matrix[1, 2]
FN <- conf_matrix[2, 1]
TP <- conf_matrix[2, 2]

Accuracy <- (TP + TN) / sum(conf_matrix)
Specificity <- TN / (TN + FP)
Sensitivity <- TP / (TP + FN)
Precision <- TP / (TP + FP)

cat("Accuracy: ", Accuracy * 100, "%\n")
cat("Specificity: ", Specificity * 100, "%\n")
cat("Sensitivity: ", Sensitivity * 100, "%\n")
cat("Precision: ", Precision * 100, "%\n")


# 4.5	Calculate the time (in seconds) it took to train the model and the prediction time. Including the running time in your summary.
# Timing the model training
start_train_time <- Sys.time()
nb_model <- NaiveBayes(Out ~ ., data = NB_Tumor_data, na.action = na.omit)
end_train_time <- Sys.time()
training_time <- end_train_time - start_train_time

# Timing the prediction
start_pred_time <- Sys.time()
nb_predictions <- predict(nb_model, NB_Tumor_data)
end_pred_time <- Sys.time()
prediction_time <- end_pred_time - start_pred_time

cat("Time taken to train the model (seconds): ", training_time, "\n")
cat("Time taken to make predictions (seconds): ", prediction_time, "\n")


# 5: Linear Discriminant Analysis (5 points)
# 5.1	Transform the variables as necessary for LDA classification.
LDA_Tumor_data <- transform(Tumor_data, Out = as.factor(Out), Age = as.factor(Age), Sex = as.factor(Sex), Bone = as.factor(Bone), Marrow = as.factor(Marrow), Lung = as.factor(Lung), Pleura = as.factor(Pleura), Liver = as.factor(Liver), Brain = as.factor(Brain), Skin = as.factor(Skin), Neck = as.factor(Neck), Supra = as.factor(Supra), Axil = as.factor(Axil), Media = as.factor(Media))
LDA_Tumor_data

# 5.2	Use all the variables in the dataset to fit a LDA classification model.
# Fit LDA model
lda_model <- lda(Out ~ ., data = Tumor_data)
lda_model

# 5.3	Summarize the results in a Confusion Matrix.
# Predict class labels
lda_predictions <- predict(lda_model, Tumor_data)$class

# Create confusion matrix
lda_conf_matrix <- table(Actual = Tumor_data$Out, Predicted = lda_predictions)
print(lda_conf_matrix)


# 5.4	Based on the confusion matrix, calculate and comment on:
#  a. Accuracy b. Specificity c. Sensitivity d. Precision
# Extracting elements from the confusion matrix
TN <- lda_conf_matrix[1, 1]
FP <- lda_conf_matrix[1, 2]
FN <- lda_conf_matrix[2, 1]
TP <- lda_conf_matrix[2, 2]

# Calculating metrics
Accuracy <- (TP + TN) / sum(lda_conf_matrix)
Specificity <- TN / (TN + FP)
Sensitivity <- TP / (TP + FN)
Precision <- TP / (TP + FP)

# Printing the metrics
cat("Accuracy: ", Accuracy * 100, "%\n")
cat("Specificity: ", Specificity * 100, "%\n")
cat("Sensitivity: ", Sensitivity * 100, "%\n")
cat("Precision: ", Precision * 100, "%\n")


# 5.5	Calculate the time (in seconds) it took to train the model and the prediction time. Including the running time in your summary.

# Measure training time
start_train_time <- Sys.time()
lda_model <- lda(Out ~ ., data = Tumor_data)
end_train_time <- Sys.time()
lda_training_time <- end_train_time - start_train_time

# Measure prediction time
start_pred_time <- Sys.time()
lda_predictions <- predict(lda_model, Tumor_data)$class
end_pred_time <- Sys.time()
lda_prediction_time <- end_pred_time - start_pred_time

# Print the training and prediction times
cat("Time taken to train the LDA model (seconds): ", lda_training_time, "\n")
cat("Time taken to make predictions with the LDA model (seconds): ", lda_prediction_time, "\n")


# 6: Compare the above three classifiers (4 points)
# For all questions below please provide evidence.
# 6.1	Which classifier is most accurate?
#  6.2	Which classifier is most suitable when processing speed is most important?
#  6.3	Which classifier minimizes Type 1 errors?
# 6.4	Which classifier minimizes Type 2 errors?
  
  
