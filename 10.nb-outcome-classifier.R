# load required libraries
require(caret)
require(dplyr)
require(quanteda)
library(varhandle)

# set quanteda options
quanteda_options(threads = 7)

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

# fit naive Bayes outcome classifier to training set
nb_outcome <- textmodel_nb(training_dfm, docvars(training_dfm, "outcome"),
                             prior = "docfreq")

# limit test set features to just those in training set
test_dfm <- dfm_select(test_dfm, pattern = training_dfm, 
                       selection = "keep")

# predict outcome on test set and measure performance
actual_class <- docvars(test_dfm, "outcome")
predicted_class <- predict(nb_outcome, newdata = test_dfm)
class_table <- table(actual_class, predicted_class)
confusionMatrix(class_table, mode = "everything")

# clean up the workspace
rm.all.but("nb_outcome")

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/nb-outcome-classifier.RData")