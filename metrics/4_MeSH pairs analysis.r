# This script imports the date of first pairing previously obtained by querying
# PubMed APIs, scans the records for errors, runs some descriptive statistics,
# and calculates the final indices.
#
# Runs in R 4.4.0

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Loading resources

# Clearing environment
rm(list = ls())


# Loading the single MeSH terms we have previously identified and their date of
# creation.
mt <- read.csv2("./NIH MeSH/output/MeSH_terms.csv")
mdate <- read.csv2("./NIH MeSH/output/MeSH_date.csv")

# Then I load the pairs of MeSH terms and their date of first pairing:
mp <- read.csv2("./NIH MeSH/output/MeSH_pairs.csv")
mpdate <- read.csv2("./NIH MeSH/output/MeSH_pairs_date.csv")
mpdate$dateOldestPairing_raw <- as.integer(mpdate$dateOldestPairing)
mpdate$nPairings <- as.integer(mpdate$nPairings)
mpdate$repaired <- as.integer(mpdate$repaired)

# Loading the list to match applications and their MeSH pairs.
load("./NIH MeSH/output/MeSH_pairMap.RData")

# Loading application data
a <- as.data.frame(haven::read_dta(
  "./NNF Project 20231228/2.DATA/original_source_imports/applications.dta"
))


# Checking structure is consistent
nrow(mt) == nrow(mdate)
nrow(mp) == nrow(mpdate)
tail(mt)[,1]; tail(mdate)$Application.source.record.id
tail(mp); tail(mpdate)


# Checking that there are no missing values
table(mpdate$NCBIapiStatus, useNA = "ifany")
table(is.na(mpdate$dateOldestPairing_raw), mpdate$NCBIapiStatus != "done")

# Showing rows containing missing values, if there are any:
mpdate[which(mpdate$NCBIapiStatus != "done"),]

table(is.na(mpdate$dateOldestPairing_raw), mpdate$nPairings == 0)


# Date format __________________________________________________________________
# Let's clean the formatting of dates.
#
# First of all, sometimes we only have years (YYYY) instead of precise dates
# (YYYYMMDD). In these cases I conservatively assume the date to be January 1st,
# thus converting YYYY into YYYY0101.
# So, first I create a temporary variable for my convenienve:
dates <- as.character(mpdate$dateOldestPairing_raw)

# Then I note down which dates have only four digits -- in other words, which
# entries are in the YYYY format and not in the YYYYMMDD format.
datesToRepair <- which(nchar(dates) == 4 & !is.na(dates))

# And now I can fill them in with our conservative MMDD:
dates[datesToRepair] <- paste0(dates[datesToRepair], "0101")
mpdate$dateOldestPairing <- dates
rm(dates)

# Lastly, I convert it to class "Date":
mpdate$dateOldestPairing <- as.Date(mpdate$dateOldestPairing, format = "%Y%m%d")




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Now I calculate, for each application, the min, mean, median and max date of
# first pairing from among the MeSH pairs assigned to it.
#
# This is the dataset I'm going to fill in. Rows are applications.
d <- data.frame(
  Application.source.record.id = mdate$Application.source.record.id,
  nMeSHpairs = NA,
  nMeSHpairsEvaluated = NA,
  minDateFirstPairing = as.Date(NA),
  maxDateFirstPairing = as.Date(NA),
  meanDateFirstPairing = as.Date(NA),
  medianDateFirstPairing = as.Date(NA),
  novelty_new_MeSH_pairs_count = NA,
  novelty_new_MeSH_pairs_dic = NA
)



pb = txtProgressBar(min = 0, max = nrow(d), initial = 0, style = 3)
for (i in 1:nrow(d)) { # For each  ith application...
  
  # Let's note down how many pairs of MeSH terms we had available for this 
  # application and for how many of them we have the date:
  np <- t(mt[i, -1]) # Single MeSH terms for i
  np <- sum(!is.na(np)) # removing NA's and tallying up actual MeSH terms
  d$nMeSHpairs[i] <- (np * (np - 1)) / 2 # Calculating all unique pairs
  
  # If we have no pairs (e.g. there is only one MeSH term), then we ignore
  # the ith application.
  if (np < 2) next 
  
  # Find the date of first pairing of all MeSH pairs assigned to it:
  dates = mpdate$dateOldestPairing[MeSH_pairMap[[i]]]
  d$nMeSHpairsEvaluated[i] <- length(dates) # How many we got the dates for
  
  
  # Find the submission date of application i:
  sdate <- a$Applicationdate[
    a$Applicationsourcerecordid == d$Application.source.record.id[i]
  ]
  
  
  # Now take the min, max, mean and median. We skip this (and leave NA) if all
  # dates are missing:
  if (any(!is.na(dates))) {
    
    # Note that the most novel pairs will not have been paired yet, and their 
    # date will be NA. To reflect this, when taking the max date (i.e. the 
    # most recent date) this code will return NA if at least one of the dates
    # is also NA.
    d$maxDateFirstPairing[i] <- max(dates, na.rm = FALSE) ##
    
    
    # For all others (min, mean, median) I replace NA's with the date of
    # submission.
    dates_noNA <- dates
    dates_noNA[is.na(dates_noNA)] <- sdate
    d$minDateFirstPairing[i] <- min(dates_noNA, na.rm = TRUE)
    d$meanDateFirstPairing[i] <- mean(dates_noNA, na.rm = TRUE)
    d$medianDateFirstPairing[i] <- median(dates_noNA, na.rm = TRUE)
  }
  
  
  # Lastly I can calculate our MeSH-pair-based index of novelty.
  # I create two versions that aligns with how I handled single MeSH terms:
  #   - one version is the tally of new MeSH pairs;
  #   - the other is binary (1 for applications with at least 1 new MeSH pair,
  #     and 0 otherwise).
  ifelse (
    d$nMeSHpairs[i] > 0, # if there are any pairs at all...
    newPairs <- is.na(dates) | dates > sdate, # ...then take the new pairs...
    newPairs <- FALSE # ... else, there are no new pairs.
  )
  #newPairs <- is.na(dates) | dates > sdate
  d$novelty_new_MeSH_pairs_count[i] <- sum(newPairs, na.rm = TRUE)
  d$novelty_new_MeSH_pairs_dic[i] <- any(newPairs)
  
  
  # Updating progress bar:
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb)



# Removing all applications that have no MeSH pairs and saving to file.
write.csv2(
  d[d$nMeSHpairs > 0,], 
  "./NIH MeSH/output/MeSH_pairs_novelty.csv", 
  row.names = FALSE
)

# Checking for potential MeSH pairs not being examined:
table(
  potentuallyUnexamined = d$nMeSHpairs > 0 & d$nMeSHpairsEvaluated == 0,
  novel = d$novelty_new_MeSH_pairs_dic
)
# So, all applications with potentially unexamined MeSH pairs were correctly
# marked as not novel. This shows that I conservatively code as "novel" only
# applications for which I am sure there are some novel MeSH pairs.