# load required libraries
require(dplyr)
require(quanteda)
library(varhandle)

# set quanteda options
quanteda_options(threads = 7)

# load document feature matrix for full set of opinions
load("~/OneDrive/Projects/corporate-veil/data/full-set-dfm.RData")

# load document feature matrix for complete coded set of opinions
load("~/OneDrive/Projects/corporate-veil/data/complete-coded-set-dfm.RData")

# load naive Bayes outcome classifier
load("~/OneDrive/Projects/corporate-veil/data/nb-outcome-classifier.RData")

# limit full set features to just those in complete coded set
full_set_dfm <- dfm_select(full_set_dfm, pattern = complete_coded_set_dfm, 
                           selection = "keep")

# predict outcome of cases in full set
predicted_class <- predict(nb_outcome, newdata = full_set_dfm, force = TRUE)

# add predicted class relevance document variable to full set dfm
docvars(full_set_dfm, "outcome") <- predicted_class

# select opinions coded as relevant
complete_relevant_set_dfm <- dfm_subset(full_set_dfm, outcome %in% c("-1", "1"))

# clean up the workspace
rm.all.but("complete_relevant_set_dfm")

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/complete-relevant-set-dfm.RData")