# load required libraries
library(caret)
library(dplyr)
library(doParallel)
library(quanteda)
library(varhandle)

# load and register the parallel processing package
cl <- makePSOCKcluster(7)
registerDoParallel(cl)

# load document feature matrix for full set
load("~/OneDrive/Projects/corporate-veil/data/full-set-dfm.RData")

# load document feature matrix for complete coded set
load("~/OneDrive/Projects/corporate-veil/data/complete-coded-set-dfm.RData")

# limit complete coded set features to just those in full set
complete_coded_set_dfm <- dfm_select(complete_coded_set_dfm, 
                                     pattern = full_set_dfm, 
                                     selection = "keep")

# divide complete coded set into training and test sets
cases <- ndoc(complete_coded_set_dfm)
set.seed(300)
id_train <- sample(1:cases, 0.9 * cases, replace = FALSE)
docvars(complete_coded_set_dfm, "id_numeric") <- 1:cases
training_dfm <- dfm_subset(complete_coded_set_dfm, id_numeric %in% id_train)
test_dfm <- dfm_subset(complete_coded_set_dfm, !id_numeric %in% id_train)

# extract features and outcomes from training set and test set dfm objects
x_train <- convert(training_dfm, to = "matrix")[, 2:6443]
y_train <- training_dfm@docvars$outcome
x_test <- convert(test_dfm, to = "matrix")[, 2:6443]
y_test <- test_dfm@docvars$outcome

# train naive Bayes classifier on training set
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)
nbcv_outcome <- train(x = x_train, 
                  y = y_train,
                  method = "nb",
                  trControl = fitControl)

# predict outcome on test set and measure performance
predicted_class <- predict(nbcv_outcome, x_test)
class_table <- table(y_test, predicted_class)
confusionMatrix(class_table, mode = "everything")

# clean up the workspace
rm.all.but("nbcv_outcome")

# stop parallel processing
stopCluster(cl)

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/nbcv-outcome-classifier.RData")