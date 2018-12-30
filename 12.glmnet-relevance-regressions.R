# load required libraries
require(broom)
require(doMC)
require(dplyr)
require(ggplot2)
library(glmnet)
require(quanteda)
library(varhandle)

# initiate parallel processing
registerDoMC(cores = 7)

# load document feature matrix for complete relevant set
load("~/OneDrive/Projects/corporate-veil/data/complete-relevant-set-dfm.RData")

# normalize the complete relevant set dfm
normalized_dfm <- dfm_weight(complete_relevant_set_dfm, scheme = "prop")

# extract inputs and outcomes for the relevant cases
x <- as.matrix(normalized_dfm)
y <- droplevels(normalized_dfm@docvars$outcome)

# perform cross-validated lasso, ridge, and hybrid glmnet regressions
lasso_glm <- cv.glmnet(x, y, family = "binomial", alpha = 1, 
                       type.measure = "class", parallel = TRUE, 
                       intercept = FALSE)
ridge_glm <- cv.glmnet(x, y, family = "binomial", alpha = 0, 
                       type.measure = "class", parallel = TRUE, 
                       intercept = FALSE)
hybrid_glm <- cv.glmnet(x, y, family = "binomial", alpha = 0.5, 
                       type.measure = "class", parallel = TRUE, 
                       intercept = FALSE)

# extract selected features and their coefficients
lasso_coef <- tidy(coef.cv.glmnet(lasso_glm))[, -2]
colnames(lasso_coef) <- c("feature", "coefficient")
ridge_coef <- tidy(coef.cv.glmnet(ridge_glm))[, -2]
colnames(ridge_coef) <- c("feature", "coefficient")
hybrid_coef <- tidy(coef.cv.glmnet(hybrid_glm))[, -2]
colnames(hybrid_coef) <- c("feature", "coefficient")

# order the features by the coefficients
lasso_coef <- lasso_coef %>% arrange(desc(coefficient))
ridge_coef <- ridge_coef %>% arrange(desc(coefficient))
hybrid_coef <- hybrid_coef %>% arrange(desc(coefficient))

# terminate parallel processing
registerDoSEQ()

# clean up the workspace
rm.all.but(c("hybrid_coef", "hybrid_glm", "lasso_coef", 
             "lasso_glm", "ridge_coef", "ridge_glm"))

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/glmnet-relevance-regressions.RData")