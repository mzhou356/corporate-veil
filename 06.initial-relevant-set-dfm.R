# load required libraries
require(dplyr)
require(quanteda)
library(varhandle)

# set quanteda options
quanteda_options(threads = 7)

# load document feature matrix for full set of opinions
load("~/OneDrive/Projects/corporate-veil/data/full-set-dfm.RData")

# load document feature matrix for initial coded set of opinions
load("~/OneDrive/Projects/corporate-veil/data/initial-coded-set-dfm.RData")

# load naive Bayes relevance classifier
load("~/OneDrive/Projects/corporate-veil/data/nb-relevance-classifier.RData")

# limit full set features to just those in initial coded set
full_set_dfm <- dfm_select(full_set_dfm, pattern = initial_coded_set_dfm, 
                          selection = "keep")

# predict relevance of cases in full set
predicted_class <- predict(nb_relevance, newdata = full_set_dfm, force = TRUE)

# add predicted class relevance document variable to full set dfm
docvars(full_set_dfm, "relevance") <- predicted_class

# select opinions coded as relevant
initial_relevant_set_dfm <- dfm_subset(full_set_dfm, relevance == "relevant")

# clean up the workspace
rm.all.but("initial_relevant_set_dfm")

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/initial-relevant-set-dfm.RData")