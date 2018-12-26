# load required libraries
require(dplyr)
require(quanteda)
require(topicmodels)
library(varhandle)

# set quanteda options
quanteda_options(threads = 7)

# load document feature matrix for full set of opinions
load("~/OneDrive/Projects/corporate-veil/data/full-set-dfm.RData")

# load document feature matrix for coded set of opinions
load("~/OneDrive/Projects/corporate-veil/data/coded-set-dfm.RData")

# load naive Bayes classifier
load("~/OneDrive/Projects/corporate-veil/data/naive-bayes-classifier.RData")

# limit full set features to just those in coded set
full_dfm <- dfm_select(full_dfm, pattern = coded_dfm, 
                          selection = "keep")

# predict relevance of cases in full set
predicted_class <- predict(nb, newdata = full_dfm, force = TRUE)

# add predicted class relevance document variable to full set dfm
docvars(full_dfm, "relevance") <- predicted_class

# select opinions coded as relevant
relevant_dfm <- dfm_subset(full_dfm, relevance == "relevant")

# clean up the workspace
rm.all.but("relevant_dfm")

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/relevant-set-dfm.RData")