# load required libraries
library(dplyr)
library(quanteda)
library(varhandle)

# load initial coded set
load("~/OneDrive/Projects/corporate-veil/data/initial-coded-set.RData")

# load supplemental coded set
load("~/OneDrive/Projects/corporate-veil/data/supplemental-coded-set.RData")

# combine to create complete coded set
complete_coded_set <- rbind(initial_coded_set, supplemental_coded_set)

# clean up workspace
rm.all.but("complete_coded_set")

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/complete-coded-set.RData")