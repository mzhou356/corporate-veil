# load required libraries
require(dplyr)
require(quanteda)
require(topicmodels)
library(varhandle)

# set quanteda options
quanteda_options(threads = 7)

# load document feature matrix for relevant set of opinions
load("~/OneDrive/Projects/corporate-veil/data/relevant-set-dfm.RData")

# fit a model with four topics
dtm <- convert(relevant_dfm, to = "topicmodels")
lda <- LDA(dtm, k = 4)