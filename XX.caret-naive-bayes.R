# load required libraries
library(caret)
library(dplyr)
library(doParallel)
library(quanteda)
library(varhandle)

# load and register the parallel processing package
cl <- makePSOCKcluster(7)
registerDoParallel(cl)

# load training set document feature matrix
load("~/OneDrive/Projects/corporate-veil/data/training-set-dfm.RData")

# eliminate bigrams that occur in less than 2% of the opinions
training_dtm <- dfm_trim(training_dtm, min_docfreq = 0.02, 
                        docfreq_type = "prop")

# convert Quanteda dfm object to a data frame
training_df <- convert(training_dtm, to = "data.frame")
colnames(training_df)[1] <- "doc_id"
training_df$doc_id <- as.numeric(training_df$doc_id)
coding <- data.frame(doc_id = training_df$doc_id, 
                          coding = training_dtm@docvars$coding)
training_df <- merge(training_df, coding, sort = FALSE)
rownames(training_df) <- training_df$doc_id
training_df <- training_df[-1]
training_df$coding <- as.factor(training_df$coding)

# recode training set into relevant and irrelevant factors
training_df$coding <- recode_factor(training_df$coding,
                             "-1"="relevant", "1"="relevant", 
                             "0"="irrelevant")

# train naive Bayes classifier on data frame
train_nb <- train(coding ~ ., method = "nb", 
                  preProcess = "scale", metric = "Kappa", 
                  data = training_df)

# predict using classifier on training set and compare to manual coding
pred_coding <- predict(train_nb, training_df)
confusionMatrix(pred_coding, training_df$coding)

# stop parallel processing
stopCluster(cl)