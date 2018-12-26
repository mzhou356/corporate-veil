# load required libraries
library(dplyr)
library(openxlsx)
library(quanteda)
library(varhandle)

# load downloaded metadata and opinions
load("~/OneDrive/Projects/corporate-veil/data/download-data.RData")

# load relevant set dfm
load("~/OneDrive/Projects/corporate-veil/data/relevant-set-dfm.RData")

# extract document ids for all of the relevant cases
rel_docs <- relevant_dfm@Dimnames$docs

# random selection of 25% of cases without replacement
num_cases <- length(rel_docs)
set.seed(99)
id_train <- sample(rel_docs, round(num_cases / 4), replace = FALSE)
id_train <- as.numeric(id_train)

# create training set and test set dataframes
coded_set_2 <- opinions %>% filter(doc_id %in% id_train)
uncoded_set_2 <- opinions %>% filter(!doc_id %in% id_train)

# export spreadsheet with cases for manual coding
for_coding_2 <- (metadata %>% 
                   filter(id %in% id_train) %>% 
                   select(id, absolute_url))
for_coding_2$absolute_url <- paste0("https://www.courtlistener.com",
                                  for_coding_2$absolute_url)
write.xlsx(for_coding_2,
           "~/OneDrive/Projects/corporate-veil/spreadsheets/for_coding_2.xlsx")

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
# Next, type "relevance" in cell E1, and complete column E for each
# case, specifying relevant (outcome -1 or 1) or irrelevant (outcome 0).
# Finally, save the completed spreadsheet for further processing in R.

# import spreadsheet with cases manually coded
for_coding_2 <- read.xlsx("spreadsheets/for_coding_2.xlsx", 
                        cols = c(1,4,5))
colnames(for_coding_2) <- c("doc_id", "outcome", "relevance")

# add the outcome and relevance information to the coded set
coded_set_2 <- merge(coded_set_2, for_coding_2, sort = FALSE)

# convert outcome and relevance columns in coded set to factors
coded_set_2$outcome <- as.factor(coded_set_2$outcome)
coded_set_2$relevance <- as.factor(coded_set_2$relevance)

# clean up workspace
rm.all.but(c("coded_set_2", "uncoded_set_2"))

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/coded-uncoded-sets-2.RData")