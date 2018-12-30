# load required libraries
library(dplyr)
library(openxlsx)
library(quanteda)
library(varhandle)

# load downloaded metadata and opinions
load("~/OneDrive/Projects/corporate-veil/data/download-data.RData")

# random selection of 10% of cases without replacement
num_cases <- nrow(metadata)
set.seed(99)
id_train <- sample(1:num_cases, round(num_cases / 10), replace = FALSE)

# create initial coded set dataframe
initial_coded_set <- opinions[id_train, ]

# export spreadsheet with cases for manual coding
for_coding <- metadata[id_train, ] %>% select(id, absolute_url)
for_coding$absolute_url <- paste0("https://www.courtlistener.com",
                                  for_coding$absolute_url)
write.xlsx(for_coding,
           "~/OneDrive/Projects/corporate-veil/spreadsheets/for_coding.xlsx")

# Open the spreadsheet in excel for manual coding of each case.
# Expand the width of column B to encompass the longest URL.
# In column C type the formula =HYPERLINK(B2) in row 2.
# Then copy the formula down the rest of the rows to create
# clickable URLs for convenience in manually coding each case.
# Expand the width of column C to encompass the longest URL.
# Hide column B and use column D to record the outcome for each case.
# Type "url" in cell C1 and "outcome" in cell D1 for identification.
# Complete column D for each case, specifying -1 (bad outcome, affiliate
# liable), 0 (irrelevant case), or 1 (good outcome, affiliate not liable).
# Next, type "relevance" in cell E1, and in cell E2 type the formula
# =IF(D2=0,"irrelevant","relevant"). Then copy the formula down the rest of
# the rows. Finally, save the completed spreadsheet for further processing.

# import the spreadsheet after the cases have been manually coded
for_coding <- read.xlsx("spreadsheets/for_coding.xlsx", 
                         cols = c(1, 4, 5))
colnames(for_coding) <- c("doc_id", "outcome", "relevance")

# add the outcome and relevance information to the coded set
initial_coded_set <- merge(initial_coded_set, for_coding, sort = FALSE)

# convert outcome and relevance columns in initial coded set to factors
initial_coded_set$outcome <- as.factor(initial_coded_set$outcome)
initial_coded_set$relevance <- as.factor(initial_coded_set$relevance)

# clean up workspace
rm.all.but("initial_coded_set")

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/initial-coded-set.RData")