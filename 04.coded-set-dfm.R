# load required libraries
library(stringi)
library(quanteda)
library(varhandle)

# set quanteda options
quanteda_options(threads = 7)

# load coded and uncoded sets
load("~/OneDrive/Projects/corporate-veil/data/coded-uncoded-sets.RData")

# load legal terms and combine with English stopwords
load("~/OneDrive/Projects/corporate-veil/data/legal-terms.RData")
noisewords <- c(stopwords(),  legal_terms)
noisewords <- sort(unique(noisewords))

# create coded set document feature matrix containing bigrams
my_corpus <- corpus(coded_set)
my_tokens <- tokens(my_corpus, remove_numbers = TRUE, 
                    remove_punct = TRUE, remove_symbols = TRUE, 
                    remove_separators = TRUE, include_docvars = TRUE)
my_tokens <- tokens_tolower(my_tokens)
my_tokens <- tokens_remove(my_tokens, noisewords)
my_tokens <- tokens_wordstem(my_tokens)
my_tokens <- tokens_select(my_tokens, min_nchar = 4)
my_tokens <- tokens(my_tokens, ngrams = 2)
coded_dfm <- dfm(my_tokens)

# clean up the workspace
rm.all.but("coded_dfm")

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/coded-set-dfm.RData")