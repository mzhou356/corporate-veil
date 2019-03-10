#' ---
#' title: "Corporate Veil Project"
#' author: "Douglas C. Barnard"
#' date: "2/3/2019"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
## ---- include=FALSE, eval=TRUE-------------------------------------------

#############################################################
# Load required packages and enable parallel processing
#############################################################

(if(!require(tidyverse)) 
  install.packages("tidyverse",repos = "http://cran.us.r-project.org"))
(if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org"))
(if(!require(checkmate))
  install.packages("checkmate", repos = "http://cran.us.r-project.org"))
(if(!require(doMC))
  install.packages("doMC", repos = "http://cran.us.r-project.org"))
(if(!require(dplyr))
  install.packages("dplyr", repos = "http://cran.us.r-project.org"))
(if(!require(httr))
  install.packages("httr", repos = "http://cran.us.r-project.org"))
(if(!require(jsonlite))
  install.packages("jsonlite", repos = "http://cran.us.r-project.org"))
(if(!require(knitr))
  install.packages("knitr", repos = "http://cran.us.r-project.org"))
(if(!require(quanteda))
  install.packages("quanteda", repos = "http://cran.us.r-project.org"))
(if(!require(stringi))
  install.packages("stringi", repos = "http://cran.us.r-project.org"))
(if(!require(textclean))
  install.packages("textclean", repos = "http://cran.us.r-project.org"))
(if(!require(varhandle))
  install.packages("varhandle", repos = "http://cran.us.r-project.org"))

# Important: This script uses the doMC package referred to above
# and the following three lines of code (as well as the temination
# line of code at the very end of the script) to enable parallel
# processing. With parallel processing enabled, the complete script
# was run in just under 30 minutes on an iMac Pro (2017) with
# CPU 3.2 GHz Intel Xeon W, RAM 32 GB 2666 MHz DDR4, and
# GPU Radeon Pro Vega 56 8176 MB. The script will require more time
# to execute without parallel processing, and may exceed memory limits
# on a computer with less than 32 GB of RAM. Comment out the line above
# installing the doMC package, the following three lines of code, and
# the termination line of code at the very end of this script if the
# script will not be run on a Mac with multiple cores. The packages and
# lines of code needed to enable parallel processing on a Windows machine
# are described in the following vignette on the Cran site:
# https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf

num_cores <- ceiling(detectCores() / 2)
registerDoMC(cores = num_cores)
quanteda_options(threads = num_cores)


#' 
## ---- include=FALSE, eval=TRUE-------------------------------------------

#############################################################
# Download potentially relevant data from courtlistener website
#############################################################

# specify username and password for courtlistener website

username <- "dcbarnard"
password <- "9KjvHViweMbyLRT9kukl"

# set global httr configuration

set_config(authenticate(username, password), override = FALSE)

# specify baseURL that searches for potentially relevant cases

subStr1 <- "https://www.courtlistener.com/api/rest/v3/search/?q="
subStr2 <- "(%22pierc*+veil%22~3+OR+%22disregard*+corpor*%22~5)"
subStr3 <- "&type=o&order_by=score+desc&stat_Precedential=on"
baseURL <- paste0(subStr1, subStr2, subStr3)

# download metadata regarding the identified cases

i <- 1
pages <- list()
newURL <- paste0(baseURL, "&page=", "1")
while(testNull(newURL) != TRUE){
  httpResponse <- GET(newURL, content_type_json())
  newdata <- fromJSON(content(httpResponse, "text"))
  pages[[i]] <- newdata$results
  i <- i + 1
  newURL <- newdata$`next`
}
metadata <- rbind_pages(pages)

# download the text of the relevant opinions

id <- metadata$id
count <- length(id)
text <- vector(length = count)
url1 <- "https://www.courtlistener.com/api/rest/v3/opinions/"
url3 <- "/?format=json"
for (i in 1:count){
  url2 <- id[i]
  newURL <- paste0(url1, url2, url3)
  httpResponse <- GET(newURL, content_type_json())
  newdata <- fromJSON(content(httpResponse, "text"))
  htmlOpinion <- newdata$html_with_citations
  textOpinion <- newdata$plain_text
  if (textOpinion != "") text[i] <- textOpinion else text[i] <- htmlOpinion
}
opinions <- data_frame(doc_id = id, text = text)

# remove html from the downloaded opinions

opinions$text <- replace_html(opinions$text)

# clean up the workspace

rm.all.but(c("metadata", "opinions"))


#' 
## ---- include=FALSE, eval=TRUE-------------------------------------------

#############################################################
# create document features matrix for the downloaded opinions
#############################################################

# load legal terms and combine with English stopwords

load("~/OneDrive/Projects/corporate-veil/data/legal_terms.RData")
noisewords <- c(stopwords(),  legal_terms)
noisewords <- sort(unique(noisewords))

# create document feature matrix containing bigrams for full set

my_corpus <- corpus(opinions)
my_tokens <- tokens(my_corpus, remove_numbers = TRUE, 
                    remove_punct = TRUE, remove_symbols = TRUE, 
                    remove_separators = TRUE, include_docvars = TRUE)
my_tokens <- tokens_tolower(my_tokens)
my_tokens <- tokens_remove(my_tokens, noisewords)
my_tokens <- tokens_wordstem(my_tokens)
my_tokens <- tokens_select(my_tokens, min_nchar = 4)
my_tokens <- tokens(my_tokens, ngrams = 2)
full_set_dfm <- dfm(my_tokens)

# eliminate bigrams that occur in less than 1% and 2% of the opinions

full_set_1_dfm <- dfm_trim(full_set_dfm, min_docfreq = 0.01, 
                      docfreq_type = "prop")
full_set_2_dfm <- dfm_trim(full_set_dfm, min_docfreq = 0.02, 
                      docfreq_type = "prop")

# clean up the workspace

rm.all.but(c("metadata", "opinions", "full_set_1_dfm",
             "full_set_2_dfm", "noisewords"))


#' 
## ---- include=FALSE, eval=TRUE-------------------------------------------

#############################################################
# create document features matrix for the hand-coded opinions
#############################################################

# load hand-coded dataset

load("~/OneDrive/Projects/corporate-veil/data/hand_coded_set.RData")

# create document feature matrix containing bigrams for hand-coded set

my_corpus <- corpus(hand_coded_set)
my_tokens <- tokens(my_corpus, remove_numbers = TRUE, 
                    remove_punct = TRUE, remove_symbols = TRUE, 
                    remove_separators = TRUE, include_docvars = TRUE)
my_tokens <- tokens_tolower(my_tokens)
my_tokens <- tokens_remove(my_tokens, noisewords)
my_tokens <- tokens_wordstem(my_tokens)
my_tokens <- tokens_select(my_tokens, min_nchar = 4)
my_tokens <- tokens(my_tokens, ngrams = 2)
hand_coded_set_dfm <- dfm(my_tokens)

# limit hand-coded set dfm features to just those in full set dfm (2%)
hand_coded_set_dfm <- dfm_select(hand_coded_set_dfm, 
                                    pattern = full_set_2_dfm, 
                                    selection = "keep")

# clean up the workspace

rm.all.but(c("metadata", "opinions", "full_set_1_dfm",
             "full_set_2_dfm", "noisewords", "hand_coded_set_dfm"))


#' 
## ---- include=FALSE, eval=TRUE-------------------------------------------

#############################################################
# create and train naive bayes classifier model
#############################################################

# divide hand-coded set dfm into training and test dfms

cases <- ndoc(hand_coded_set_dfm)
set.seed(300)
id_train <- sample(1:cases, 0.9 * cases, replace = FALSE)
docvars(hand_coded_set_dfm, "id_numeric") <- 1:cases
training_dfm <- dfm_subset(hand_coded_set_dfm, id_numeric %in% id_train)
test_dfm <- dfm_subset(hand_coded_set_dfm, !id_numeric %in% id_train)

# fit naive Bayes outcome classifier to training dfm

nb_outcome <- textmodel_nb(training_dfm, docvars(training_dfm, "outcome"),
                             prior = "docfreq")

# predict outcome on test dfm and measure performance

actual_class <- docvars(test_dfm, "outcome")
predicted_class <- predict(nb_outcome, newdata = test_dfm)
class_table <- table(actual_class, predicted_class)
performance <- confusionMatrix(class_table, mode = "everything")

# clean up the workspace

rm.all.but(c("metadata", "opinions", "full_set_1_dfm",
             "full_set_2_dfm", "noisewords", "hand_coded_set_dfm",
             "nb_outcome", "performance"))


#' 
## ---- include=FALSE, eval=TRUE-------------------------------------------

#############################################################
# create dfm for opinions that nb model predicts are relevant
#############################################################

# select full set dfm (2%) features that are also in hand-coded set dfm
temp_dfm <- dfm_select(full_set_2_dfm, pattern = hand_coded_set_dfm, 
                           selection = "keep")

# predict outcome of cases in full set dfm (2%) using selected features
predicted_class <- predict(nb_outcome, newdata = temp_dfm, force = TRUE)

# add predicted class relevance document variable
docvars(temp_dfm, "outcome") <- predicted_class

# create dfm for the opinions predicted to be relevant
relevant_set_dfm <- dfm_subset(temp_dfm, outcome %in% c("-1", "1"))

# clean up the workspace

rm.all.but(c("metadata", "opinions", "full_set_1_dfm",
             "full_set_2_dfm", "noisewords", "hand_coded_set_dfm",
             "nb_outcome", "performance", "relevant_set_dfm"))


#' 
