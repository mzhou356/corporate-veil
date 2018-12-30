# load required libraries
require(dplyr)
require(quanteda)
require(topicmodels)
library(varhandle)

# set quanteda options
quanteda_options(threads = 7)

# load document feature matrix for complete relevant set
load("~/OneDrive/Projects/corporate-veil/data/complete-relevant-set-dfm.RData")

# load glmnet relevance regressions
load("~/OneDrive/Projects/corporate-veil/data/glmnet-relevance-regressions.RData")

# fit an LDA model with three topics
dtm <- convert(complete_relevant_set_dfm, to = "topicmodels")
lda <- LDA(dtm, k = 3)

# extract the features associated with each topic and their LDA weights
lda_terms <- posterior(lda)
topic_1_terms <- tidy(lda_terms$terms[1, ])
colnames(topic_1_terms) <- c("feature", "weight")
topic_2_terms <- tidy(lda_terms$terms[2, ])
colnames(topic_2_terms) <- c("feature", "weight")
topic_3_terms <- tidy(lda_terms$terms[3, ])
colnames(topic_3_terms) <- c("feature", "weight")

# select features for which there are glmnet regression coefficients,
# and calculate an adjusted probability for each feature equal to the
# product of its LDA weight and its lasso or ridge coefficient
topic_1_lasso <- (merge(lasso_coef, topic_1_terms) %>% 
                    mutate("adj_prob" = abs(coefficient * weight)))
topic_1_ridge <- (merge(ridge_coef, topic_1_terms) %>% 
                    mutate("adj_prob" = abs(coefficient * weight)))
topic_1_hybrid <- (merge(hybrid_coef, topic_1_terms) %>% 
                    mutate("adj_prob" = abs(coefficient * weight)))
topic_2_lasso <- (merge(lasso_coef, topic_2_terms) %>% 
                    mutate("adj_prob" = abs(coefficient * weight)))
topic_2_ridge <- (merge(ridge_coef, topic_2_terms) %>% 
                    mutate("adj_prob" = abs(coefficient * weight)))
topic_2_hybrid <- (merge(hybrid_coef, topic_2_terms) %>% 
                     mutate("adj_prob" = abs(coefficient * weight)))
topic_3_lasso <- (merge(lasso_coef, topic_3_terms) %>% 
                    mutate("adj_prob" = abs(coefficient * weight)))
topic_3_ridge <- (merge(ridge_coef, topic_3_terms) %>% 
                    mutate("adj_prob" = abs(coefficient * weight)))
topic_3_hybrid <- (merge(hybrid_coef, topic_3_terms) %>% 
                     mutate("adj_prob" = abs(coefficient * weight)))

# select top 25 features for each topic using adj prob
topic_1_lasso_terms <- (top_n(topic_1_lasso %>% 
                                arrange(desc(adj_prob)), 25))[, 1]
topic_1_ridge_terms <- (top_n(topic_1_ridge %>% 
                                arrange(desc(adj_prob)), 25))[, 1]
topic_1_hybrid_terms <- (top_n(topic_1_hybrid %>% 
                                arrange(desc(adj_prob)), 25))[, 1]
topic_2_lasso_terms <- (top_n(topic_2_lasso %>% 
                                arrange(desc(adj_prob)), 25))[, 1]
topic_2_ridge_terms <- (top_n(topic_2_ridge %>% 
                                arrange(desc(adj_prob)), 25))[, 1]
topic_2_hybrid_terms <- (top_n(topic_2_hybrid %>% 
                                 arrange(desc(adj_prob)), 25))[, 1]
topic_3_lasso_terms <- (top_n(topic_3_lasso %>% 
                                arrange(desc(adj_prob)), 25))[, 1]
topic_3_ridge_terms <- (top_n(topic_3_ridge %>% 
                                arrange(desc(adj_prob)), 25))[, 1]
topic_3_hybrid_terms <- (top_n(topic_3_hybrid %>% 
                                 arrange(desc(adj_prob)), 25))[, 1]

# create dataframes to hold the results
topic_1 <- data.frame(lasso = topic_1_lasso_terms, 
                      ridge = topic_1_ridge_terms, 
                      hybrid = topic_1_hybrid_terms)
topic_2 <- data.frame(lasso = topic_2_lasso_terms,
                      ridge = topic_2_ridge_terms, 
                      hybrid = topic_2_hybrid_terms)
topic_3 <- data.frame(lasso = topic_3_lasso_terms, 
                      ridge = topic_3_ridge_terms, 
                      hybrid = topic_3_hybrid_terms)

# clean up the workspace
rm.all.but(c("topic_1", "topic_2", "topic_3"))

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/weighted-topic-model.RData")