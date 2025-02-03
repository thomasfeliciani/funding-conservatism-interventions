

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# MeSH pairs
#
# This script prepares all the data we need to date MeSH pairs. 
# 
# The first step is to find all pairs for which we need to calculate the 
# age/recency. We only need the date of the most recent pair of MeSH terms for  
# each application. 

# Clearing environment
rm(list = ls())

library("compiler")

# Loading the MeSH terms we have previously identified
mt <- read.csv2("./NIH MeSH/output/MeSH_terms.csv")
#mui <- read.csv2("./NIH MeSH/output/MeSH_UI.csv")
mdate <- read.csv2("./NIH MeSH/output/MeSH_date.csv")


# Reformatting for our convenience: extracting the first column containing
# applications' reference ID.
appIDs <- mt$Application.source.record.id
mt <- mt[,-1]
mdate <- mdate[,-1]


# Defining the objects we'll be working with: 
MeSH_pairs <- data.frame(term1 = character(), term2 = character())
MeSH_pairMap <- list()


# Then we loop through all applications in our dataset.
# This is not implemented efficiently and it will take a long while.
compiler::enableJIT(1)
pb = txtProgressBar(
  title = "Finding MeSH pairs...",
  min = 0, max = length(appIDs), initial = 0, style = 3
)
for (i in 1:length(appIDs)) {
  
  # Extracting the terms of application i
  t <- data.frame(cbind(
    term = as.character(mt[i,]),
    date = as.integer(mdate[i,]) 
  ))
  t$date <- as.integer(t$date)
  
  # Removing empty elements
  t <- t[!is.na(t$term),]
  
  # Checking that we have at least one pair to work with
  if(nrow(t) < 2) {
    MeSH_pairMap[[i]] <- NA
    next
  }
  
  # Selecting the most recent terms (not that two or more of these terms
  # might tie for recency).
  nt <- t[which(t$date == max(t$date)),]
  
  # Relevant pairs are those that include at least one of the most recent
  # terms:
  pairs <- expand.grid(
    term1 = t$term,
    term2 = t$term
  )
  pairs$term1 <- as.character(pairs$term1)
  pairs$term2 <- as.character(pairs$term2)
  
  # Removing redundant combinations:
  pairs <- pairs[pairs$term1 > pairs$term2,]
  
  # Removing combinations that don't include the most recent term(s):
  pairs <- subset(
    pairs,
    pairs$term1 %in% nt$term | pairs$term2 %in% nt$term
  )
  
  # Adding these pairs to our global list of pairs:
  MeSH_pairs <- rbind(MeSH_pairs, pairs)
  
  # Removing duplicates we might have added:
  MeSH_pairs <- MeSH_pairs[!duplicated(MeSH_pairs),]
  
  # Retrieving the row numbers in MeSH_pairs that identify the terms we just
  # added.
  MeSH_pairMap[[i]] <- which(
    duplicated(
      rbind(pairs, MeSH_pairs)
    ) [(nrow(pairs) + 1):(nrow(pairs) + nrow(MeSH_pairs))]
  )
  
  
  # Updating progress bar
  setTxtProgressBar(pb, i)
}
close(pb)
compiler::enableJIT(0)

# Checking output
nrow(MeSH_pairs)
sum(duplicated(MeSH_pairs)) # Should be zero
table(MeSH_pairs$term1 > MeSH_pairs$term2) # should all be TRUE
#
# And this list of MeSH terms
mt[500,][!is.na(mt[500,])]
# ... should contain some MeSH terms that can be found in either one of these
# two columns:
MeSH_pairs[MeSH_pairMap[[500]],]


# Saving to file:
write.csv2(MeSH_pairs, "./NIH MeSH/output/MeSH_pairs_b.csv", row.names = FALSE)
save(MeSH_pairMap, file = "./NIH MeSH/output/MeSH_pairMap_b.RData")

