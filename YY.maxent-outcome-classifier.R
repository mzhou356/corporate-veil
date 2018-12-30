# load required libraries
library(caret)
library(dplyr)
library(quanteda)
library(varhandle)

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

# calculate number of cases to hold out
holdout <- round(complete_coded_set_dfm@Dim[1] / 10)

# extract features and outcomes from training set
training_dtm <- convert(training_dfm, to = "tm")
outcomes <- training_dfm@docvars$outcome

# train maximum entropy model
maxent_outcome <- maxent(training_dtm, outcomes, set_heldout = holdout)

# extract features and outcomes from test set
test_dtm <- convert(test_dfm, to = "tm")
outcomes <- test_dfm@docvars$outcome

# predict outcome on complete coded set and measure performance
predicted_class <- predict(maxent_outcome, test_dtm)[, 1]
class_table <- table(outcomes, predicted_class)
confusionMatrix(class_table, mode = "everything")

# clean up the workspace
rm.all.but("maxent_outcome")

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/maxent-outcome-classifier.RData")