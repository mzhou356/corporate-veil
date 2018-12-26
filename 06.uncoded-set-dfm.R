# load required libraries
require(dplyr)
require(quanteda)
library(varhandle)

# set quanteda options
quanteda_options(threads = 7)

# load coded and uncoded sets
load("~/OneDrive/Projects/corporate-veil/data/coded-uncoded-sets.RData")

# load document feature matrix for coded set
load("~/OneDrive/Projects/corporate-veil/data/coded-set-dfm.RData")

# load legal terms and combine with English stopwords
load("~/OneDrive/Projects/corporate-veil/data/legal-terms.RData")
noisewords <- c(stopwords(),  legal_terms)
noisewords <- sort(unique(noisewords))

# load naive Bayes classifier
load("~/OneDrive/Projects/corporate-veil/data/naive-bayes-classifier.RData")

# create uncoded set document feature matrix containing bigrams
my_corpus <- corpus(uncoded_set)
my_tokens <- tokens(my_corpus, remove_numbers = TRUE, 
                    remove_punct = TRUE, remove_symbols = TRUE, 
                    remove_separators = TRUE, include_docvars = TRUE)
my_tokens <- tokens_tolower(my_tokens)
my_tokens <- tokens_remove(my_tokens, noisewords)
my_tokens <- tokens_wordstem(my_tokens)
my_tokens <- tokens_select(my_tokens, min_nchar = 4)
my_tokens <- tokens(my_tokens, ngrams = 2)
uncoded_dfm <- dfm(my_tokens)

# limit uncoded set features to just those in coded set
uncoded_dfm <- dfm_select(uncoded_dfm, pattern = coded_dfm, 
                       selection = "keep")

# predict relevance of cases in uncoded set
predicted_class <- predict(nb, newdata = uncoded_dfm, force = TRUE)

# add predicted class relevance document variable to uncoded set dfm
docvars(uncoded_dfm, "relevance") <- predicted_class

# clean up the workspace
rm.all.but("uncoded_dfm")

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/uncoded-set-dfm.RData")