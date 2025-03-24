# do kNN model
library('mlr3')
library('mlr3verse')
library('mlr3learners')
library('kknn')
library('dplyr')
selected_features <- c("InDegree", "OutDegree", "TotalPosts", "MeanPostsPerSubForum", "AccountAge", "cluster") # "MeanPostsPerSubForum", "PercBiNeighbours"

social_media_selected_features <- socialMedia_with_cluster[, selected_features]

# Consider users who have 2 years as test data set
train_data <- socialMedia_with_cluster %>%
  filter(AccountAge >24)
test_data <- socialMedia_with_cluster %>%
  filter(AccountAge <=24)
# get the size of train data
# set value in list 20 values as 0
acc_list <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

for (i in 7:27){
  set.seed(127)
  # Create task
  task <- as_task_classif(train_data, target = 'cluster')
  
  train_set = sample(task$row_ids, 0.7 * task$nrow)
  validation_set = setdiff(task$row_ids, train_set)
  
  learner = lrn("classif.kknn", k=i)
  learner
  
  # training
  learner$train(task, row_ids = train_set)
  
  #lets use the model to predict (training set!!)
  pred_train = learner$predict(task, row_ids=train_set) # predicting 
  pred_train$confusion #print the conf matrix
  
  # Make predictions on validation set
  pred_validation <- learner$predict(task, row_ids = validation_set)
  pred_validation$confusion
  measures = msrs(c('classif.acc', 'classif.ce')) #accuracy and class error
  x <- pred_train$score(measures) #print the scores 
  acc_list[i-7] <- x["classif.acc"]
}

# This code performs 5-fold cross-validation for different values of k (from 7 to 27) and stores the average accuracy for each value of k in the acc_list vector. Adjustments may be needed based on the specifics of your dataset and modeling requirements.
# Initialize vector to store accuracy values
acc_list <- numeric(length = 21)

# Define number of folds
num_folds <- 5

# Create task
task <- as_task_classif(train_data, target = 'cluster')

# Loop over different values of k
for (i in 7:27){
  set.seed(127)
  
  # Initialize vector to store accuracy values for each fold
  fold_acc <- numeric(length = num_folds)
  
  # Split data into folds
  folds <- cut(1:task$nrow, breaks = num_folds, labels = FALSE)
  
  # Perform cross-validation
  for (fold in 1:num_folds) {
    # Define train and validation sets for current fold
    train_set <- task$row_ids[folds != fold]
    validation_set <- task$row_ids[folds == fold]
    
    # Create and train model
    learner <- lrn("classif.kknn", k = i)
    learner$train(task, row_ids = train_set)
    
    # Make predictions on validation set
    pred_validation <- learner$predict(task, row_ids = validation_set)
    
    # Compute accuracy for current fold
    measures <- msrs(c('classif.acc'))
    x <- pred_validation$score(measures)
    acc <- x["classif.acc"]
    
    # Store accuracy for current fold
    fold_acc[fold] <- acc
  }
  
  # Calculate average accuracy across all folds
  avg_acc <- mean(fold_acc)
  
  # Store average accuracy for current value of k
  acc_list[i - 6] <- avg_acc
}

# Print accuracy values
print(acc_list)
write.csv(acc_list, file = "accuracy_list.csv")

selected_k <- 6 + which.max(acc_list)
task_test <- as_task_classif(test_data, target = 'cluster')
learner_test <- lrn("classif.kknn", k = selected_k)
learner_test$train(task_test)
pred_test <- learner_test$predict(task_test)
pred_test$confusion
# Compute accuracy for current fold
measures <- msrs(c('classif.acc'))
x <- pred_test$score(measures)
acc <- x["classif.acc"]

# compute the evalution metrics
# Confusion matrix
conf_matrix <- matrix(c(59, 0, 1, 0,
                        0, 0, 0, 0,
                        0, 1, 11, 0,
                        2, 0, 0, 424), 
                      nrow = 4, byrow = TRUE)

# Function to calculate precision
precision <- function(conf_matrix, cluster) {
  tp <- conf_matrix[cluster, cluster]
  fp <- sum(conf_matrix[, cluster]) - tp
  return(tp / (tp + fp))
}

# Function to calculate sensitivity
sensitivity <- function(conf_matrix, cluster) {
  tp <- conf_matrix[cluster, cluster]
  fn <- sum(conf_matrix[cluster, ]) - tp
  return(tp / (tp + fn))
}

# Function to calculate specificity
specificity <- function(conf_matrix, cluster) {
  tn <- sum(diag(conf_matrix)) - conf_matrix[cluster, cluster]
  fp <- sum(conf_matrix[, cluster]) - conf_matrix[cluster, cluster]
  return(tn / (tn + fp))
}

# Calculate precision, sensitivity, and specificity for each cluster
for (cluster in 1:4) {
  cat("Cluster", cluster, ":\n")
  cat("Precision:", precision(conf_matrix, cluster), "\n")
  cat("Sensitivity:", sensitivity(conf_matrix, cluster), "\n")
  cat("Specificity:", specificity(conf_matrix, cluster), "\n\n")
}

# visualization of the role transition compared between the prior users and new members
library(ggplot2)
data <- read.csv("percentage of users in each role.csv")
data <- data.frame(data)

ggplot(data) +
  geom_bar(aes(x = reorder(Status, desc(Status)),
               y= Proportion, fill= Position), 
              position = "stack", stat = "identity") +
  labs(x = "Member types", y = "% Proportion") +
  theme_classic()
