# load required libraries
library(stringi)
library(quanteda)
library(varhandle)

# set quanteda options
quanteda_options(threads = 7)

# load initial coded set
load("~/OneDrive/Projects/corporate-veil/data/initial-coded-set.RData")

# load legal terms and combine with English stopwords
load("~/OneDrive/Projects/corporate-veil/data/legal-terms.RData")
noisewords <- c(stopwords(),  legal_terms)
noisewords <- sort(unique(noisewords))

# create initial coded set document feature matrix containing bigrams
my_corpus <- corpus(initial_coded_set)
my_tokens <- tokens(my_corpus, remove_numbers = TRUE, 
                    remove_punct = TRUE, remove_symbols = TRUE, 
                    remove_separators = TRUE, include_docvars = TRUE)
my_tokens <- tokens_tolower(my_tokens)
my_tokens <- tokens_remove(my_tokens, noisewords)
my_tokens <- tokens_wordstem(my_tokens)
my_tokens <- tokens_select(my_tokens, min_nchar = 4)
my_tokens <- tokens(my_tokens, ngrams = 2)
initial_coded_set_dfm <- dfm(my_tokens)

# clean up the workspace
rm.all.but("initial_coded_set_dfm")

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/initial-coded-set-dfm.RData")