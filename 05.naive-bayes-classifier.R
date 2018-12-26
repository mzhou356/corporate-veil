# load required libraries
require(dplyr)
require(quanteda)
library(varhandle)

# set quanteda options
quanteda_options(threads = 7)

# load document feature matrix for full set
load("~/OneDrive/Projects/corporate-veil/data/full-set-dfm.RData")

# load document feature matrix for coded set
load("~/OneDrive/Projects/corporate-veil/data/coded-set-dfm.RData")

# limit coded set features to just those in full set
coded_dfm <- dfm_select(coded_dfm, pattern = full_dfm, 
                          selection = "keep")

# eliminate bigrams that occur in less than 2% of the coded opinions
# coded_dfm <- dfm_trim(coded_dfm, min_docfreq = 0.02, docfreq_type = "prop")

# divided coded set into training and test sets
cases <- ndoc(coded_dfm)
set.seed(300)
id_train <- sample(1:cases, 0.9 * cases, replace = FALSE)
docvars(coded_dfm, "id_numeric") <- 1:cases
training_dfm <- dfm_subset(coded_dfm, id_numeric %in% id_train)
test_dfm <- dfm_subset(coded_dfm, !id_numeric %in% id_train)

# fit naive Bayes relevance classifier to training set
nb <- textmodel_nb(training_dfm, docvars(training_dfm, "relevance"),
                   prior = "docfreq")

# limit test set features to just those in training set
test_dfm <- dfm_select(test_dfm, pattern = training_dfm, 
                       selection = "keep")

# predict relevance on test set and measure performance
actual_class <- docvars(test_dfm, "relevance")
predicted_class <- predict(nb, newdata = test_dfm)
class_table <- table(actual_class, predicted_class)
confusionMatrix(class_table, mode = "everything")

# clean up the workspace
rm.all.but("nb")

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/naive-bayes-classifier.RData")