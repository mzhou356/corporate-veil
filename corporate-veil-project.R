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
(if(!require(gbm))
  install.packages("gbm", repos = "http://cran.us.r-project.org"))
(if(!require(httr))
  install.packages("httr", repos = "http://cran.us.r-project.org"))
(if(!require(jsonlite))
  install.packages("jsonlite", repos = "http://cran.us.r-project.org"))
(if(!require(kernlab))
  install.packages("kernlab", repos = "http://cran.us.r-project.org"))
(if(!require(knitr))
  install.packages("knitr", repos = "http://cran.us.r-project.org"))
(if(!require(openxlsx))
  install.packages("openxlsx", repos = "http://cran.us.r-project.org"))
(if(!require(quanteda))
  install.packages("quanteda", repos = "http://cran.us.r-project.org"))
(if(!require(stringi))
  install.packages("stringi", repos = "http://cran.us.r-project.org"))
(if(!require(textclean))
  install.packages("textclean", repos = "http://cran.us.r-project.org"))
(if(!require(tidytext))
  install.packages("tidytext", repos = "http://cran.us.r-project.org"))
(if(!require(topicmodels))
  install.packages("topicmodels", repos = "http://cran.us.r-project.org"))
(if(!require(varhandle))
  install.packages("varhandle", repos = "http://cran.us.r-project.org"))
(if(!require(xgboost))
  install.packages("xgboost", repos = "http://cran.us.r-project.org"))

# Important Note: This script uses the doMC package referred to above
# and the following three lines of code (as well as the temination
# line of code at the very end of the script) to enable parallel
# processing. With parallel processing enabled, the complete script
# was run in ~30 minutes on an iMac Pro (2017) with CPU 3.2 GHz Intel
# Xeon W, RAM 32 GB 2666 MHz DDR4, and GPU Radeon Pro Vega 56 8176 MB.
# The script will require more time to execute without parallel
# processing, and may exceed memory limits on a computer with less
# than 32 GB of RAM. The doMC package will only run on Macs and other
# Unix-type machines with multiple processors, multiple cores, or both.
# The line above installing the doMC package, the following three lines
# of code, and the termination line of code at the very end of this
# script must be commented out if the script will not be run on a Mac.

num_cores <- ceiling(detectCores() / 2)
registerDoMC(cores = num_cores)
quanteda_options(threads = num_cores)

## 
## #############################################################
## # Download potentially relevant data from courtlistener website
## #############################################################
## 
## # specify username and password for courtlistener website
## 
## username <- "username"
## password <- "password"
## 
## # set global httr configuration
## 
## set_config(authenticate(username, password), override = FALSE)
## 
## # specify baseURL that searches for potentially relevant cases
## 
## subStr1 <- "https://www.courtlistener.com/api/rest/v3/search/?q="
## subStr2 <- "(%22pierc*+veil%22~3+OR+%22disregard*+corpor*%22~5)"
## subStr3 <- "&type=o&order_by=score+desc&stat_Precedential=on"
## baseURL <- paste0(subStr1, subStr2, subStr3)
## 
## # download metadata regarding the identified cases
## 
## i <- 1
## pages <- list()
## newURL <- paste0(baseURL, "&page=", "1")
## while(testNull(newURL) != TRUE){
##   httpResponse <- GET(newURL, content_type_json())
##   newdata <- fromJSON(content(httpResponse, "text"))
##   pages[[i]] <- newdata$results
##   i <- i + 1
##   newURL <- newdata$`next`
## }
## metadata <- rbind_pages(pages)
## 
## # download the text of the relevant opinions
## 
## id <- metadata$id
## count <- length(id)
## text <- vector(length = count)
## url1 <- "https://www.courtlistener.com/api/rest/v3/opinions/"
## url3 <- "/?format=json"
## for (i in 1:count){
##   url2 <- id[i]
##   newURL <- paste0(url1, url2, url3)
##   httpResponse <- GET(newURL, content_type_json())
##   newdata <- fromJSON(content(httpResponse, "text"))
##   htmlOpinion <- newdata$html_with_citations
##   textOpinion <- newdata$plain_text
##   if (textOpinion != "") text[i] <- textOpinion else text[i] <- htmlOpinion
## }
## opinions <- data_frame(doc_id = id, text = text)
## 
## # remove html from the downloaded opinions
## 
## opinions$text <- replace_html(opinions$text)
## 

#############################################################
# load previously downloaded CourtListener data
#############################################################

load("data/courtlistener-data.RData")


#############################################################
# create document features matrix for the downloaded opinions
#############################################################

# load legal terms and combine with standard English stopwords

load("data/legal-stopwords.RData")
noisewords <- c(stopwords(), legal_terms)
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

# eliminate features that occur in less than 1% of the opinions

full_set_dfm <- dfm_trim(full_set_dfm, min_docfreq = 0.01, 
                      docfreq_type = "prop")

# create matrix with remaining features for manual examination

feats <- full_set_dfm@Dimnames$features
rows <- ceiling(length(feats) / 3)
feats <- matrix(feats, nrow = rows, ncol = 3, byrow = FALSE)
write.csv(feats, "spreadsheets/features.csv")

# select subset of opinions that appear likely to involve personal
# injuries, wrongful deaths, or product liability because certain
# specific features appear one or more times in those opinions

tort_features <- c("person_injuri", "wrong_death",
                   "product_liabil", "worker_compens")

tort_set_dfm <- full_set_dfm
docs <- length(tort_set_dfm@Dimnames$docs)
type <- vector(mode = "character", length = docs)

for (i in 1:docs) {
  type[i] <- if_else(sum(as.numeric(tort_set_dfm[i, tort_features]))
                          > 0, "tort", "other")
}

docvars(tort_set_dfm, "type") <- type
tort_set_dfm <- dfm_subset(tort_set_dfm, type == "tort")

# eliminate opinions that, while likely to involve these "torts," are
# nonetheless likely to be irrelevant for our purposes because certain
# other specific features (indicative of the case involving threshold
# jurisdictional issues) appear three or more times in those opinions

juris_features <- c("divers_citizenship", "violat_constitut",
                    "lack_person", "confer_jurisdict",
                    "minimum_contact", "tradit_notion",
                    "jurisdict_matter", "jurisdict_assert",
                    "within_jurisdict", "jurisdict_hear",
                    "regist_agent", "jurisdict_purpos",
                    "jurisdict_also", "jurisdict_jurisdict",
                    "notion_fair",  "personam_jurisdict",
                    "jurisdict_base",  "jurisdict_person",
                    "support_jurisdict", "jurisdict_proper",
                    "suffici_contact", "exercis_person",
                    "exercis_jurisdict", "establish_person",
                    "assert_jurisdict", "offend_tradit",
                    "jurisdict_nonresid", "lack_jurisdict",
                    "establish_jurisdict", "jurisdict_must",
                    "whether_jurisdict", "divers_jurisdict",
                    "divers_action", "matter_jurisdict",
                    "person_jurisdict", "jurisdict_action",
                    "jurisdict_requir", "jurisdict_corpor",
                    "find_jurisdict")

docs <- length(tort_set_dfm@Dimnames$docs)
stage <- vector(mode = "character", length = docs)

for (i in 1:docs) {
  stage[i] <- if_else(sum(as.numeric(tort_set_dfm[i, juris_features]))
                          > 2, "prelim", "ripe")
}

docvars(tort_set_dfm, "stage") <- stage
tort_set_dfm <- dfm_subset(tort_set_dfm, stage == "ripe")

# remove temporary docvars from tort set dfm

docvars(tort_set_dfm) <- NULL

# create table with top 10 bigram features for each dfm

rankings <- tibble("Full Set" = names(topfeatures(full_set_dfm)),
                   "Tort Set" = names(topfeatures(tort_set_dfm)))


kable(convert(tort_set_dfm[1:5, 1:5], to = "data.frame"))


kable(rankings)

## 
## #############################################################
## # create hand-coded dataset for random 50% sample of opinions
## #############################################################
## 
## # extract document ids for all of the likely tort opinions
## 
## tort_docs <- as.numeric(tort_set_dfm@Dimnames$docs)
## 
## # random selection of 50% of likely tort opinions without replacement
## 
## num_cases <- length(tort_docs)
## set.seed(99)
## id_hand <- sort(sample(tort_docs, round(num_cases / 2), replace = FALSE))
## id_machine <- sort(tort_docs %>% .[!. %in% id_hand])
## 
## # create dataframe for opinions that will be hand coded
## 
## hand_coded_set <- (opinions %>% filter(doc_id %in% id_hand) %>%
##                      arrange(doc_id))
## 
## # export spreadsheet with opinions for manual coding
## 
## for_coding <- (metadata %>%
##                       filter(id %in% id_train) %>%
##                       select(id, absolute_url))
## for_coding$absolute_url <- paste0("https://www.courtlistener.com",
##                                        for_coding$absolute_url)
## write.xlsx(for_coding,
##            "spreadsheets/for_coding.xlsx")
## 
## # Open the spreadsheet in excel for manual coding of each opinion.
## # Expand the width of column B to encompass the longest URL.
## # In column C type the formula =HYPERLINK(B2) in row 2.
## # Then copy the formula down the rest of the rows to create
## # clickable URLs for convenience in manually coding each opinion.
## # Expand the width of column C to encompass the longest URL.
## # Hide column B and use column D to record the outcome for each case.
## # Type "url" in cell C1 and "outcome" in cell D1 for identification.
## # Complete column D for each opinion, specifying -1 (veil was pierced),
## # 0 (irrelevant opinion), or 1 (veil was not pierced). Finally, save
## # the completed spreadsheet for further processing.
## 
## # import the spreadsheet after the opinions have been manually coded
## 
## for_coding <- read.xlsx("spreadsheets/for_coding.xlsx",
##                              cols = c(1, 4))
## colnames(for_coding) <- c("doc_id", "code")
## 
## # translate the numeric coding into descriptive words
## 
## for_coding$outcome <- if_else(for_coding$code == -1,
##                               "disregarded",
##                               if_else(for_coding$code == 1,
##                                       "respected",
##                                       "irrelevant"))
## 
## # add the outcome information to the hand-coded set
## 
## hand_coded_set <- merge(hand_coded_set, for_coding, sort = FALSE)
## 
## # convert codes and outcomes in hand-coded set to factors
## 
## hand_coded_set$code <- as.factor(hand_coded_set$code)
## hand_coded_set$outcome <- as.factor(hand_coded_set$outcome)
## 

#############################################################
# load previously hand-coded data
#############################################################

load("data/hand-coded-data.RData")


#############################################################
# create document features matrices from the hand-coded data
#############################################################

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

# limit hand-coded set dfm features to just those in tort set dfm

hand_coded_set_dfm <- dfm_select(hand_coded_set_dfm,
                                 pattern = tort_set_dfm,
                                 selection = "keep")

# add relevance factor to hand-coded set dfm

coding <- hand_coded_set_dfm@docvars$code
docvars(hand_coded_set_dfm,
        "relevance") <- as.factor(ifelse(coding == 0,
                                         "unlikely",
                                         "potential"))

# create subset dfm with opinions that were hand-coded relevant

hand_coded_rel_dfm <- dfm_subset(hand_coded_set_dfm,
                                 relevance == "potential")

# drop unused levels from hand-coded relevant dfm

docvars(hand_coded_rel_dfm,
        "code") <- droplevels(hand_coded_rel_dfm@docvars$code)
docvars(hand_coded_rel_dfm,
        "outcome") <- droplevels(hand_coded_rel_dfm@docvars$outcome)


#############################################################
# random classification models
#############################################################

# validation of random *relevance* classifier

## create vectors to hold the confusion matrix results

tp <- vector()
fp <- vector()
fn <- vector()
tn <- vector()

## prevalence of relevant opinions

(prevalence <- sum(hand_coded_set_dfm@docvars$relevance
                   == "potential") / hand_coded_set_dfm@Dim[1])

## set seed for reproducibility

set.seed(99)

## fit model 10000 times

for (i in 1:10000) {

  # predict relevance for opinions and create confusion matrix
  
  actual_class <- docvars(hand_coded_set_dfm, "relevance")
  total_preds <- length(actual_class)
  pred <- runif(total_preds)
  predicted_class <- ifelse(pred <= prevalence,
                            "potential", "unlikely")
  confusion_matrix <- table(actual_class,
                            predicted_class,
                            useNA = "always")
  
  # record confusion matrix results for this iteration
  
  tp[i] <- confusion_matrix[1, 1] / total_preds
  fp[i] <- confusion_matrix[2, 1] / total_preds
  fn[i] <- confusion_matrix[1, 2] / total_preds
  tn[i] <- confusion_matrix[2, 2] / total_preds
  
}

## confusion matrix and statistics

tp <- mean(tp, na.rm = TRUE)
fp <- mean(fp, na.rm = TRUE)
fn <- mean(fn, na.rm = TRUE)
tn <- mean(tn, na.rm = TRUE)
acc <- (tp + tn) / (tp + fp + fn + tn)
rcl <- tp / (tp + fn)
prec <- tp / (tp + fp)
f1 <- 2 * (rcl * prec) / (rcl + prec)

## create validation results tibble

relevance <- tibble(model = "random benchmark",
                    tp = tp,
                    fp = fp,
                    fn = fn,
                    tn = tn,
                    acc = acc,
                    rcl = rcl,
                    prec = prec,
                    f1 = f1)

# validation of random *outcome* classifier

## create vectors to hold the confusion matrix results

tp <- vector()
fp <- vector()
fn <- vector()
tn <- vector()

## prevalence of relevant opinions

(prevalence <- sum(hand_coded_rel_dfm@docvars$outcome
                   == "disregarded") / hand_coded_rel_dfm@Dim[1])

## set seed for reproducibility

set.seed(99)

## fit model 10000 times

for (i in 1:10000) {

  # predict outcome for opinions and create confusion matrix
  
  actual_class <- docvars(hand_coded_rel_dfm, "outcome")
  total_preds <- length(actual_class)
  pred <- runif(total_preds)
  predicted_class <- ifelse(pred <= prevalence,
                            "disregarded", "respected")
  confusion_matrix <- table(actual_class,
                            predicted_class,
                            useNA = "always")
  
  # record confusion matrix results for this iteration
  
  tp[i] <- confusion_matrix[1, 1] / total_preds
  fp[i] <- confusion_matrix[2, 1] / total_preds
  fn[i] <- confusion_matrix[1, 2] / total_preds
  tn[i] <- confusion_matrix[2, 2] / total_preds
  
}

## confusion matrix and statistics

tp <- mean(tp, na.rm = TRUE)
fp <- mean(fp, na.rm = TRUE)
fn <- mean(fn, na.rm = TRUE)
tn <- mean(tn, na.rm = TRUE)
acc <- (tp + tn) / (tp + fp + fn + tn)
rcl <- tp / (tp + fn)
prec <- tp / (tp + fp)
f1 <- 2 * (rcl * prec) / (rcl + prec)

## create validation results tibble

outcome <- tibble(model = "random benchmark",
                    tp = tp,
                    fp = fp,
                    fn = fn,
                    tn = tn,
                    acc = acc,
                    rcl = rcl,
                    prec = prec,
                    f1 = f1)


kable(relevance, digits = 3)


kable(outcome, digits = 3)


#############################################################
# naive Bayes classification models
#############################################################

# bootstrap validation of nb *relevance* classifier

## create vectors to hold the confusion matrix results

tp <- vector()
fp <- vector()
fn <- vector()
tn <- vector()

## set seed for reproducibility

set.seed(99)

## fit model 100 times using random 90/10 training/test samples

for (i in 1:100) {
  
  # divide hand-coded set dfm into training and test dfms
  
  temp_dfm <- hand_coded_set_dfm
  cases <- ndoc(temp_dfm)
  id_train <- sample(1:cases, 0.9 * cases, replace = FALSE)
  docvars(temp_dfm, "id_numeric") <- 1:cases
  training_dfm <- dfm_subset(temp_dfm, id_numeric %in% id_train)
  test_dfm <- dfm_subset(temp_dfm, !id_numeric %in% id_train)
  
  # fit naive bayes relevance classifier to training dfm
  
  nb_relevance <- textmodel_nb(training_dfm,
                               docvars(training_dfm, "relevance"),
                               prior = "docfreq",
                               distribution = "multinomial")

  # predict relevance for test dfm and create confusion matrix
  
  actual_class <- docvars(test_dfm, "relevance")
  predicted_class <- predict(nb_relevance, newdata = test_dfm)
  confusion_matrix <- table(actual_class,
                            predicted_class,
                            useNA = "always")
  total_preds <- length(predicted_class)
  
  # record confusion matrix results for this iteration
  
  tp[i] <- confusion_matrix[1, 1] / total_preds
  fp[i] <- confusion_matrix[2, 1] / total_preds
  fn[i] <- confusion_matrix[1, 2] / total_preds
  tn[i] <- confusion_matrix[2, 2] / total_preds
  
}

## confusion matrix and statistics for bootstrap validation

tp <- mean(tp, na.rm = TRUE)
fp <- mean(fp, na.rm = TRUE)
fn <- mean(fn, na.rm = TRUE)
tn <- mean(tn, na.rm = TRUE)
acc <- (tp + tn) / (tp + fp + fn + tn)
rcl <- tp / (tp + fn)
prec <- tp / (tp + fp)
f1 <- 2 * (rcl * prec) / (rcl + prec)

## append to relevance results tibble

relevance <- bind_rows(relevance,
                       tibble(model = "naive Bayes",
                              tp = tp,
                              fp = fp,
                              fn = fn,
                              tn = tn,
                              acc = acc,
                              rcl = rcl,
                              prec = prec,
                              f1 = f1))

# bootstrap validation of nb *outcome* classifier

## create vectors to hold the confusion matrix results

tp <- vector()
fp <- vector()
fn <- vector()
tn <- vector()

## set seed for reproducibility

set.seed(99)

## fit model 100 times using random 90/10 training/test samples

for (i in 1:100) {
  
  # divide hand-coded relevant set dfm into training and test dfms
  
  temp_dfm <- hand_coded_rel_dfm
  cases <- ndoc(temp_dfm)
  id_train <- sample(1:cases, 0.9 * cases, replace = FALSE)
  docvars(temp_dfm, "id_numeric") <- 1:cases
  training_dfm <- dfm_subset(temp_dfm, id_numeric %in% id_train)
  test_dfm <- dfm_subset(temp_dfm, !id_numeric %in% id_train)
  
  # fit naive bayes outcome classifier to training dfm
  
  nb_outcome <- textmodel_nb(training_dfm,
                             docvars(training_dfm, "outcome"),
                             prior = "docfreq",
                             distribution = "multinomial")

  # predict outcome for test dfm and create confusion matrix
  
  actual_class <- docvars(test_dfm, "outcome")
  predicted_class <- predict(nb_outcome, newdata = test_dfm)
  confusion_matrix <- table(actual_class,
                            predicted_class,
                            useNA = "always")
  total_preds <- length(predicted_class)
  
  # record confusion matrix results for this iteration
  
  tp[i] <- confusion_matrix[1, 1] / total_preds
  fp[i] <- confusion_matrix[2, 1] / total_preds
  fn[i] <- confusion_matrix[1, 2] / total_preds
  tn[i] <- confusion_matrix[2, 2] / total_preds
  
}

## confusion matrix and statistics for bootstrap validation

tp <- mean(tp, na.rm = TRUE)
fp <- mean(fp, na.rm = TRUE)
fn <- mean(fn, na.rm = TRUE)
tn <- mean(tn, na.rm = TRUE)
acc <- (tp + tn) / (tp + fp + fn + tn)
rcl <- tp / (tp + fn)
prec <- tp / (tp + fp)
f1 <- 2 * (rcl * prec) / (rcl + prec)

## append to outcome results tibble

outcome <- bind_rows(outcome,
                     tibble(model = "naive Bayes",
                            tp = tp,
                            fp = fp,
                            fn = fn,
                            tn = tn,
                            acc = acc,
                            rcl = rcl,
                            prec = prec,
                            f1 = f1))


kable(relevance, digits = 3)


kable(outcome, digits = 3)


#############################################################
# extreme gradient boosting classification models
#############################################################

# bootstrap validation of xgb *relevance* classifier

## specify parameters for xgb relevance classifier

param <- list(verbosity = 0,
              nthread = num_cores,
              eta = 0.2,
              gamma = 0,
              max_depth = 6,
              min_child_weight = 2,
              max_delta_step = 0,
              subsample = 1,
              lambda = 1,
              alpha = 0)

## initial cross-validation tuning using hand-coded set dfm

cv_data <- as.matrix(hand_coded_set_dfm)
cv_labels <- as.numeric(hand_coded_set_dfm@docvars$relevance) - 1
cv_xgb <- xgb.DMatrix(data = cv_data,
                      label = cv_labels)

xgb_rel_cv <- xgb.cv(param,
                     data = cv_xgb,
                     nrounds = 100,
                     nfold = 10,
                     verbose = FALSE,
                     objective = "binary:logistic",
                     early_stopping_rounds = 10)

## create vectors to hold the confusion matrix results

tp <- vector()
fp <- vector()
fn <- vector()
tn <- vector()

## set seed for reproducibility

set.seed(99)

## fit model 100 times using random 90/10 training/test samples

for (i in 1:100) {
  
  # divide hand-coded set dfm into training and test dfms
  
  temp_dfm <- hand_coded_set_dfm
  cases <- ndoc(temp_dfm)
  id_train <- sample(1:cases, 0.9 * cases, replace = FALSE)
  docvars(temp_dfm, "id_numeric") <- 1:cases
  training_dfm <- dfm_subset(temp_dfm, id_numeric %in% id_train)
  test_dfm <- dfm_subset(temp_dfm, !id_numeric %in% id_train)
  
  # create training and test matrices for xgb relevance classifier
  
  training_data <- as.matrix(training_dfm)
  training_labels <- as.numeric(training_dfm@docvars$relevance) - 1
  training_xgb <- xgb.DMatrix(data = training_data,
                              label = training_labels)
  
  test_data <- as.matrix(test_dfm)
  test_labels <- as.numeric(test_dfm@docvars$relevance) - 1
  test_xgb <- xgb.DMatrix(data = test_data,
                          label = test_labels)
  
  # fit xgb relevance classifier to training matrix
  
  watchlist <- list(train = training_xgb, test = test_xgb)
  
  xgb_relevance <- xgb.train(param,
                             data = training_xgb,
                             nrounds = xgb_rel_cv$best_iteration,
                             watchlist = watchlist,
                             objective = "binary:logistic",
                             verbose = 0)
  
  # predict relevance for test matrix and create confusion matrix
  
  actual_class <- as.factor(test_labels)
  pred <- predict(xgb_relevance, test_xgb)
  predicted_class <- as.factor(ifelse(pred > 0.5, 1, 0))
  confusion_matrix <- table(actual_class,
                            predicted_class,
                            useNA = "always")
  total_preds <- length(predicted_class)
  
  # record confusion matrix results for this iteration
  
  tp[i] <- confusion_matrix[1, 1] / total_preds
  fp[i] <- confusion_matrix[2, 1] / total_preds
  fn[i] <- confusion_matrix[1, 2] / total_preds
  tn[i] <- confusion_matrix[2, 2] / total_preds
  
}

## confusion matrix and statistics for bootstrap validation

tp <- mean(tp, na.rm = TRUE)
fp <- mean(fp, na.rm = TRUE)
fn <- mean(fn, na.rm = TRUE)
tn <- mean(tn, na.rm = TRUE)
acc <- (tp + tn) / (tp + fp + fn + tn)
rcl <- tp / (tp + fn)
prec <- tp / (tp + fp)
f1 <- 2 * (rcl * prec) / (rcl + prec)

## append to relevance results tibble

relevance <- bind_rows(relevance,
                       tibble(model = "extreme gradient boosting",
                              tp = tp,
                              fp = fp,
                              fn = fn,
                              tn = tn,
                              acc = acc,
                              rcl = rcl,
                              prec = prec,
                              f1 = f1))

# bootstrap validation of xgb *outcome* classifier

## specify parameters for xgb outcome classifier

param <- list(verbosity = 0,
              nthread = num_cores,
              eta = 0.1,
              gamma = 0,
              max_depth = 6,
              min_child_weight = 1,
              max_delta_step = 0,
              subsample = 1,
              lambda = 1,
              alpha = 0)

## initial cross-validation tuning using hand-coded relevant set dfm

cv_data <- as.matrix(hand_coded_rel_dfm)
cv_labels <- as.numeric(hand_coded_rel_dfm@docvars$outcome) - 1
cv_xgb <- xgb.DMatrix(data = cv_data,
                      label = cv_labels)

xgb_out_cv <- xgb.cv(param,
                     data = cv_xgb,
                     nrounds = 100,
                     nfold = 10,
                     verbose = FALSE,
                     objective = "binary:logistic",
                     early_stopping_rounds = 10)

## create vectors to hold the confusion matrix results

tp <- vector()
fp <- vector()
fn <- vector()
tn <- vector()

## set seed for reproducibility

set.seed(99)

## fit model 100 times using random 90/10 training/test samples

watchlist <- list(train = training_xgb, test = test_xgb)

for (i in 1:100) {
  
  # divide hand-coded relevant set dfm into training and test dfms
  
  temp_dfm <- hand_coded_rel_dfm
  cases <- ndoc(temp_dfm)
  id_train <- sample(1:cases, 0.9 * cases, replace = FALSE)
  docvars(temp_dfm, "id_numeric") <- 1:cases
  training_dfm <- dfm_subset(temp_dfm, id_numeric %in% id_train)
  test_dfm <- dfm_subset(temp_dfm, !id_numeric %in% id_train)
  
  # create training and test matrices for xgb outcome classifier
  
  training_data <- as.matrix(training_dfm)
  training_labels <- as.numeric(training_dfm@docvars$outcome) - 1
  training_xgb <- xgb.DMatrix(data = training_data,
                              label = training_labels)
  
  test_data <- as.matrix(test_dfm)
  test_labels <- as.numeric(test_dfm@docvars$outcome) - 1
  test_xgb <- xgb.DMatrix(data = test_data,
                          label = test_labels)
  
  # fit xgb outcome classifier to training matrix
  
  watchlist <- list(train = training_xgb, test = test_xgb)
  
  xgb_outcome <- xgb.train(param,
                           data = training_xgb,
                           nrounds = xgb_out_cv$best_iteration,
                           watchlist = watchlist,
                           objective = "binary:logistic",
                           verbose = 0)
  
  # predict outcome for test matrix and create confusion matrix
  
  actual_class <- as.factor(test_labels)
  pred <- predict(xgb_outcome, test_xgb)
  predicted_class <- as.factor(ifelse(pred > 0.5, 1, 0))
  confusion_matrix <- table(actual_class,
                            predicted_class,
                            useNA = "always")
  total_preds <- length(predicted_class)
  
  # record confusion matrix results for this iteration
  
  tp[i] <- confusion_matrix[1, 1] / total_preds
  fp[i] <- confusion_matrix[2, 1] / total_preds
  fn[i] <- confusion_matrix[1, 2] / total_preds
  tn[i] <- confusion_matrix[2, 2] / total_preds
  
}

## confusion matrix and statistics for bootstrap validation

tp <- mean(tp, na.rm = TRUE)
fp <- mean(fp, na.rm = TRUE)
fn <- mean(fn, na.rm = TRUE)
tn <- mean(tn, na.rm = TRUE)
acc <- (tp + tn) / (tp + fp + fn + tn)
rcl <- tp / (tp + fn)
prec <- tp / (tp + fp)
f1 <- 2 * (rcl * prec) / (rcl + prec)

## append to outcome results tibble

outcome <- bind_rows(outcome,
                     tibble(model = "extreme gradient boosting",
                            tp = tp,
                            fp = fp,
                            fn = fn,
                            tn = tn,
                            acc = acc,
                            rcl = rcl,
                            prec = prec,
                            f1 = f1))


kable(relevance, digits = 3)


kable(outcome, digits = 3)


#############################################################
# random forest classification models
#############################################################

# cross validation of ranger *relevance* classifier

## create matrix for ranger relevance classifier

rgr_mat <- as.matrix(hand_coded_set_dfm)
rgr_rel <- hand_coded_set_dfm@docvars$relevance
rgr_mat <- cbind(rgr_mat, relevance = as.numeric(rgr_rel) - 1)

## fit ranger relevance classifier to matrix

fit_control <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 10)

set.seed(99)
rgr_relevance <- train(as.factor(relevance) ~ .,
                       data = rgr_mat,
                       method = "ranger",
                       trControl = fit_control,
                       verbose = FALSE,
                       metric = "Kappa")

## confusion matrix and statistics for ranger relevance classifier

confusion_matrix <- confusionMatrix(rgr_relevance)

tp <- confusion_matrix$table[1, 1] / 100
fp <- confusion_matrix$table[1, 2] / 100
fn <- confusion_matrix$table[2, 1] / 100
tn <- confusion_matrix$table[2, 2] / 100
acc <- (tp + tn) / (tp + fp + fn + tn)
rcl <- tp / (tp + fn)
prec <- tp / (tp + fp)
f1 <- 2 * (rcl * prec) / (rcl + prec)

## append to relevance results tibble

relevance <- bind_rows(relevance,
                       tibble(model = "random forest",
                              tp = tp,
                              fp = fp,
                              fn = fn,
                              tn = tn,
                              acc = acc,
                              rcl = rcl,
                              prec = prec,
                              f1 = f1))

# cross validation of ranger *outcome* classifier

## create matrix for ranger outcome classifier

rgr_mat <- as.matrix(hand_coded_rel_dfm)
rgr_out <- hand_coded_rel_dfm@docvars$outcome
rgr_mat <- cbind(rgr_mat, outcome = as.numeric(rgr_out) - 1)

## fit ranger outcome classifier to matrix

fit_control <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 10)

set.seed(99)
rgr_outcome <- train(as.factor(outcome) ~ .,
                     data = rgr_mat,
                     method = "ranger",
                     trControl = fit_control,
                     verbose = FALSE,
                     metric = "Kappa")

## confusion matrix for ranger outcome classifier

confusion_matrix <- confusionMatrix(rgr_outcome)

tp <- confusion_matrix$table[1, 1] / 100
fp <- confusion_matrix$table[1, 2] / 100
fn <- confusion_matrix$table[2, 1] / 100
tn <- confusion_matrix$table[2, 2] / 100
acc <- (tp + tn) / (tp + fp + fn + tn)
rcl <- tp / (tp + fn)
prec <- tp / (tp + fp)
f1 <- 2 * (rcl * prec) / (rcl + prec)

## append to outcome results tibble

outcome <- bind_rows(outcome,
                     tibble(model = "random forest",
                            tp = tp,
                            fp = fp,
                            fn = fn,
                            tn = tn,
                            acc = acc,
                            rcl = rcl,
                            prec = prec,
                            f1 = f1))


kable(relevance, digits = 3)


kable(outcome, digits = 3)


#############################################################
# support vector machine classification models
#############################################################

# cross validation of svm *relevance* classifier

## create objects for svm relevance classifier

svm_data <- as.kernelMatrix(as.matrix(hand_coded_set_dfm))
svm_labels <- hand_coded_set_dfm@docvars$relevance

## fit svm relevance classifier to objects

fit_control <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 10)

set.seed(99)
svm_relevance <- train(x = svm_data,
                       y = svm_labels,
                       method = "svmRadialWeights",
                       trControl = fit_control,
                       verbose = FALSE,
                       metric = "Kappa")

## confusion matrix for svm relevance classifier

confusion_matrix <- confusionMatrix(svm_relevance)

tp <- confusion_matrix$table[1, 1] / 100
fp <- confusion_matrix$table[1, 2] / 100
fn <- confusion_matrix$table[2, 1] / 100
tn <- confusion_matrix$table[2, 2] / 100
acc <- (tp + tn) / (tp + fp + fn + tn)
rcl <- tp / (tp + fn)
prec <- tp / (tp + fp)
f1 <- 2 * (rcl * prec) / (rcl + prec)

## append to relevance results tibble

relevance <- bind_rows(relevance,
                       tibble(model = "support vector machine",
                              tp = tp,
                              fp = fp,
                              fn = fn,
                              tn = tn,
                              acc = acc,
                              rcl = rcl,
                              prec = prec,
                              f1 = f1))

# cross validation of svm *outcome* classifier

## create objects for svm outcome classifier

svm_data <- as.kernelMatrix(as.matrix(hand_coded_rel_dfm))
svm_labels <- hand_coded_rel_dfm@docvars$outcome

## fit svm outcome classifier to objects

fit_control <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 10)

set.seed(99)
svm_outcome <- train(x = svm_data,
                     y = svm_labels,
                     method = "svmRadialWeights",
                     trControl = fit_control,
                     verbose = FALSE,
                     metric = "Kappa")

## confusion matrix for svm outcome classifier

confusion_matrix <- confusionMatrix(svm_outcome)

tp <- confusion_matrix$table[1, 1] / 100
fp <- confusion_matrix$table[1, 2] / 100
fn <- confusion_matrix$table[2, 1] / 100
tn <- confusion_matrix$table[2, 2] / 100
acc <- (tp + tn) / (tp + fp + fn + tn)
rcl <- tp / (tp + fn)
prec <- tp / (tp + fp)
f1 <- 2 * (rcl * prec) / (rcl + prec)

## append to outcome results tibble

outcome <- bind_rows(outcome,
                     tibble(model = "support vector machine",
                            tp = tp,
                            fp = fp,
                            fn = fn,
                            tn = tn,
                            acc = acc,
                            rcl = rcl,
                            prec = prec,
                            f1 = f1))


kable(relevance, digits = 3)


kable(outcome, digits = 3)


#############################################################
# generalized boosted classification models
#############################################################

# cross validation of gbm *relevance* classifier

## create objects for gbm relevance classifier

gbm_data <- as.kernelMatrix(as.matrix(hand_coded_set_dfm))
gbm_labels <- hand_coded_set_dfm@docvars$relevance

## fit gbm relevance classifier to objects

fit_control <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 10)

set.seed(99)
gbm_relevance <- train(x = gbm_data,
                       y = gbm_labels,
                       method = "gbm",
                       trControl = fit_control,
                       verbose = FALSE,
                       metric = "Kappa")

## confusion matrix for gbm relevance classifier

confusion_matrix <- confusionMatrix(gbm_relevance)

tp <- confusion_matrix$table[1, 1] / 100
fp <- confusion_matrix$table[1, 2] / 100
fn <- confusion_matrix$table[2, 1] / 100
tn <- confusion_matrix$table[2, 2] / 100
acc <- (tp + tn) / (tp + fp + fn + tn)
rcl <- tp / (tp + fn)
prec <- tp / (tp + fp)
f1 <- 2 * (rcl * prec) / (rcl + prec)

## append to relevance results tibble

relevance <- bind_rows(relevance,
                       tibble(model = "generalized boosted",
                              tp = tp,
                              fp = fp,
                              fn = fn,
                              tn = tn,
                              acc = acc,
                              rcl = rcl,
                              prec = prec,
                              f1 = f1))

# cross validation of gbm *outcome* classifier

## create objects for gbm outcome classifier

gbm_data <- as.kernelMatrix(as.matrix(hand_coded_rel_dfm))
gbm_labels <- hand_coded_rel_dfm@docvars$outcome

## fit gbm outcome classifier to objects

fit_control <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 10)

set.seed(99)
gbm_outcome <- train(x = gbm_data,
                     y = gbm_labels,
                     method = "gbm",
                     trControl = fit_control,
                     verbose = FALSE,
                     metric = "Kappa")

## confusion matrix for gbm outcome classifier

confusion_matrix <- confusionMatrix(gbm_outcome)

tp <- confusion_matrix$table[1, 1] / 100
fp <- confusion_matrix$table[1, 2] / 100
fn <- confusion_matrix$table[2, 1] / 100
tn <- confusion_matrix$table[2, 2] / 100
acc <- (tp + tn) / (tp + fp + fn + tn)
rcl <- tp / (tp + fn)
prec <- tp / (tp + fp)
f1 <- 2 * (rcl * prec) / (rcl + prec)

## append to outcome results tibble

outcome <- bind_rows(outcome,
                     tibble(model = "generalized boosted",
                            tp = tp,
                            fp = fp,
                            fn = fn,
                            tn = tn,
                            acc = acc,
                            rcl = rcl,
                            prec = prec,
                            f1 = f1))


kable(relevance, digits = 3)


kable(outcome, digits = 3)


#############################################################
# predict relevance and outcome for unread tort opinions
#############################################################

# recreate id_machine index for tort opinions to be machine coded

tort_docs <- as.numeric(tort_set_dfm@Dimnames$docs)
num_cases <- length(tort_docs)
set.seed(99)
id_hand <- sort(sample(tort_docs, round(num_cases / 2), replace = FALSE))
id_machine <- sort(tort_docs %>% .[!. %in% id_hand])

# create subset dfm for tort opinions to be machine coded

machine_coded_set_dfm <- (dfm_subset(tort_set_dfm,
                                     subset = tort_set_dfm@Dimnames$docs
                                     %in% id_machine))

# predict relevance of opinions in machine-coded set dfm

predicted_class <- predict(nb_relevance,
                           newdata = machine_coded_set_dfm,
                           force = TRUE)

# add predicted relevance document variable

docvars(machine_coded_set_dfm, "relevance") <- predicted_class

# select opinions predicted to be potentially relevant

machine_coded_rel_dfm <- dfm_subset(machine_coded_set_dfm,
                                    relevance == "potential")

# predict outcome of opinions in machine-coded set dfm

predicted_class <- predict(svm_outcome,
                           newdata = machine_coded_rel_dfm,
                           force = TRUE)

# add predicted outcome document variable

docvars(machine_coded_rel_dfm, "outcome") <- predicted_class


#############################################################
# create relevant set dfm with hand- and machine-coded opinions
#############################################################

# relevant set dfm with hand-coded and machine-coded opinions

relevant_set_dfm <- rbind(hand_coded_rel_dfm,
                          machine_coded_rel_dfm)

# add doc variable with outcomes for relevant opinions

out <- unlist(list(hand_coded_rel_dfm@docvars$outcome,
                       machine_coded_rel_dfm@docvars$outcome))
docvars(relevant_set_dfm, "outcome") <- out


#############################################################
# calculate outcome-predictive feature weightings
#############################################################

# extract svm outcome predictive factor weightings

svm_imp <- varImp(svm_outcome)
out_imp <- svm_imp$importance

# remove the redundant relevance column

out_imp <- out_imp[ , "disregarded", drop = FALSE]

# assign importance of zero to certain uninteresting features

unimport <- c("person_liabl", "liabl_corpor", "corpor_individu",
             "corpor_entiti", "hold_sharehold", "evid_also",
             "find_corpor", "person_liabil", "restat_tort",
             "incorpor_corpor", "corpor_offic", "breach_contract",
             "evid_show", "execut_offic", "exist_whether",
             "genuin_materi", "injuri_caus", "also_note",
             "must_show", "support_court", "evid_support",
             "abus_discret", "damag_award", "affirm_court",
             "caus_action", "admiss_evid", "award_damag",
             "court_find", "citat_omit", "give_rise",
             "proxim_caus", "admit_evid", "introduc_evid",
             "damag_result", "find_support", "corpor_veil",
             "pierc_corpor", "director_offic", "offic_director",
             "impos_liabil", "determin_whether", "veil_pierc",
             "corpor_corpor", "sharehold_corpor", "fail_establish",
             "insur_compani", "busi_entiti", "liabil_corpor",
             "veil_hold", "corpor_oper", "corpor_organ",
             "pierc_veil", "corpor_busi", "requir_corpor",
             "disregard_corpor", "corpor_exist", "entiti_disregard",
             "grant_favor", "therefor_consid", "therefor_entitl",
             "person_injuri", "delawar_corpor")

out_imp[unimport, ] <- 0

# sort the predictive factor weightings in descending order

out_imp <- out_imp[order(-out_imp$disregarded), , drop = FALSE]
colnames(out_imp) <- "importance"
out_imp$feature <- as.character(row.names(out_imp))
out_imp <- out_imp[c(2, 1)]


kable(out_imp[1:25, 1:2], digits = 3, row.names = FALSE)

## 
## #############################################################
## # create topic model for hand-coded relevant opinions
## #############################################################
## 
## # fit an LDA model with three topics
## 
## dtm <- convert(hand_coded_rel_dfm, to = "topicmodels")
## lda_model <- LDA(dtm, k = 3, method = "Gibbs",
##                  control = list(seed = 99))
## 
## # extract the features associated with each topic and their LDA weights
## 
## lda_terms <- posterior(lda_model)
## topic_1_terms <- data.frame(feature = names(lda_terms$terms[1, ]),
##                             weight = lda_terms$terms[1, ])
## topic_1_terms$feature <- as.character(topic_1_terms$feature)
## topic_2_terms <- data.frame(feature = names(lda_terms$terms[2, ]),
##                             weight = lda_terms$terms[2, ])
## topic_2_terms$feature <- as.character(topic_2_terms$feature)
## topic_3_terms <- data.frame(feature = names(lda_terms$terms[3, ]),
##                             weight = lda_terms$terms[3, ])
## topic_3_terms$feature <- as.character(topic_3_terms$feature)
## 
## # calculate an adjusted weight for each feature equal to
## # the product of its LDA weight and its svm importance
## 
## topic_1_terms <- (merge(topic_1_terms, out_imp) %>%
##                     mutate("adj_weight" = weight * importance))
## topic_2_terms <- (merge(topic_2_terms, out_imp) %>%
##                     mutate("adj_weight" = weight * importance))
## topic_3_terms <- (merge(topic_3_terms, out_imp) %>%
##                     mutate("adj_weight" = weight * importance))
## 
## # select top 25 features for each topic using adj prob
## 
## topic_1_top25 <- (top_n(topic_1_terms %>% arrange(desc(adj_weight)),
##                         25, adj_weight))[, 1]
## topic_2_top25 <- (top_n(topic_2_terms %>% arrange(desc(adj_weight)),
##                         25, adj_weight))[, 1]
## topic_3_top25 <- (top_n(topic_3_terms %>% arrange(desc(adj_weight)),
##                         25, adj_weight))[, 1]
## 
## # create a data frame to hold the top 25 features for each topic
## 
## topics <- tibble("Topic 1" = topic_1_top25,
##                  "Topic 2" = topic_2_top25,
##                  "Topic 3" = topic_3_top25)
## 

#############################################################
# load previously saved results for this step
#############################################################

load("data/topic-model-table.RData")


kable(topics)


#############################################################
# determine whether judges decide workers comp cases differently
#############################################################

# create dfms for the respective subsets of tort opinions

temp_dfm <- hand_coded_rel_dfm

work_comp_features <- c("worker_compens", "workmen_compens")

docs <- length(temp_dfm@Dimnames$docs)
category <- vector(mode = "character", length = docs)

for (i in 1:docs) {
  category[i] <- if_else(sum(as.numeric(temp_dfm[i, work_comp_features]))
                          > 0, "work_comp", "other")
}

docvars(temp_dfm, "category") <- category
wc_set_dfm <- dfm_subset(temp_dfm, category == "work_comp")
oth_set_dfm <- dfm_subset(temp_dfm, category == "other")


#############################################################
# terminate parallel processing
#############################################################

registerDoSEQ()

