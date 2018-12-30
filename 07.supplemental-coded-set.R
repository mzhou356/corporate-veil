# load required libraries
library(dplyr)
library(openxlsx)
library(quanteda)
library(varhandle)

# load downloaded metadata and opinions
load("~/OneDrive/Projects/corporate-veil/data/download-data.RData")

# load initial coded set
load("~/OneDrive/Projects/corporate-veil/data/initial-coded-set.RData")

# load initial relevant set dfm
load("~/OneDrive/Projects/corporate-veil/data/initial-relevant-set-dfm.RData")

# extract document ids for the initially coded cases
initial_coded <- initial_coded_set$doc_id

# extract document ids for all of the relevant cases
rel_docs <- initial_relevant_set_dfm@Dimnames$docs

# random selection of 25% of relevant cases without replacement
num_cases <- length(rel_docs)
set.seed(99)
id_train <- sample(rel_docs, round(num_cases / 4), replace = FALSE)
id_train <- as.numeric(id_train)

# create supplemental coded set dataframe
supplemental_coded_set <- (opinions %>% 
                             filter(doc_id %in% id_train) %>%
                             filter(!doc_id %in% initial_coded))

# export spreadsheet with cases for supplemental manual coding
for_supp_coding <- (metadata %>% 
                      filter(id %in% id_train) %>% 
                      filter(!id %in% initial_coded) %>%
                      select(id, absolute_url))
for_supp_coding$absolute_url <- paste0("https://www.courtlistener.com",
                                  for_supp_coding$absolute_url)
write.xlsx(for_supp_coding,
           "~/OneDrive/Projects/corporate-veil/spreadsheets/for_supp_coding.xlsx")

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

# import the spreadsheet after the supplemental cases have been manually coded
for_supp_coding <- read.xlsx("spreadsheets/for_supp_coding.xlsx", 
                        cols = c(1, 4, 5))
colnames(for_supp_coding) <- c("doc_id", "outcome", "relevance")

# add the outcome and relevance information to the supplemental coded set
supplemental_coded_set <- merge(supplemental_coded_set, for_supp_coding, 
                                sort = FALSE)

# convert outcome and relevance columns in supplemental coded set to factors
supplemental_coded_set$outcome <- as.factor(supplemental_coded_set$outcome)
supplemental_coded_set$relevance <- as.factor(supplemental_coded_set$relevance)

# clean up workspace
rm.all.but("supplemental_coded_set")

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/supplemental-coded-set.RData")