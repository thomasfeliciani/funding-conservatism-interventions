# Runs in R 4.4.1.
#
# Loading, joining and cleaning novelty indicators.


# Cleaning environment and loading libraries.
rm(list = ls())

library("haven")
library("arrow")
library("reshape2")
library("dplyr")
library("ggpubr")
library("moments")
library("priceR")
library("ggplot2")
library("gridExtra")
library("irr")
library("stargazer")
library("writexl")
library("psych")
#library("gtable")

source("./utils.r")


# Loading the datasets.
datadir <- "./NNF Project 20231228/2.DATA/original_source_imports/"
a <- as.data.frame(haven::read_dta(paste0(datadir, "applications.dta")))
#p <- as.data.frame(haven::read_dta(paste0(datadir, "persons.dta")))
#o <- as.data.frame(haven::read_dta(paste0(datadir, "outcome_granted.dta")))
s <- as.data.frame(haven::read_dta(paste0(datadir, "scores.dta")))
callclass <- as.data.frame(haven::read_dta(paste0(
  datadir, "call_classification.dta"
)))

# Note that some calls appear more than once in "callclass". We ought to remove
# these duplicates.
callclass <- callclass[!duplicated(callclass$Callid), ]


# Recoding
a$ApplicationYear <- format(as.Date(a$Applicationdate, format="%Y-%m-%d"),"%Y")
a$ApplicationYear <- as.numeric(a$ApplicationYear)
a$malePI <- a$Mainapplicantgender == "Male"
a$femalePI <- a$Mainapplicantgender == "Female"
unknowngender <- a$Mainapplicantgender %in% c("Unknown", "")
#a$malePI[unknowngender] <- NA ##################### If commented, treating missings as opposite gender
#a$femalePI[unknowngender] <- NA ###################

a$Mainapplicantage[a$Mainapplicantage %in% c("N/A", "")] <- NA
a$Mainapplicantage <- as.numeric(a$Mainapplicantage)
a$youngPI <- a$Mainapplicantage < 35


a$fundingOutcome <- "Declined"
a$fundingOutcome[a$Applicationstatus %in% c(
  "Granted", "In progress", "Withdrawn")] <- "Granted, in progress or withdrawn"
a$granted <-  a$Applicationstatus %in% c("Granted", "In progress", "Withdrawn")

# Filtering by year. We only analyze calls run between 2012 and 2021. Outside of
# this range, NNF data seem partial or provisional
#a <- subset(a, a$ApplicationYear %in% 2012:2021)

# Appending funding call information.
a <- merge(
  x = a,
  y = callclass[,c("Callid", "Funding_type")]
)

# ... and recoding
a$calltype <- NA
a$calltype[a$Funding_type == "grant_hrhg"] <- "Synergy calls"
a$calltype[a$Funding_type %in% c(
  "grant",
  "grant_challenge",
  "grant_collaborative",
  "grant_infrastructure",
  "grant_investigator"
)] <- "regular calls"
callclass$calltype <- NA
callclass$calltype[callclass$Funding_type == "grant_hrhg"] <- "Synergy calls"
callclass$calltype[callclass$Funding_type %in% c(
  "grant",
  "grant_challenge",
  "grant_collaborative",
  "grant_infrastructure",
  "grant_investigator"
)] <- "regular calls"


s$Applicationreferencenumeric <- as.numeric(s$Applicationreferencenumeric)

# Merging relevant application- and call-level data ____________________________
a$competition <- NA
callclass$competition <- NA
callclass$NapplicationsInCall <- NA
callclass$NgrantedInCall <- NA
callclass$avgAppliedAmountInCall <- NA
callclass$avgAppliedAmountInCalltypeYear <- NA
callclass$NknownScoresInCall <- NA

print("Calculating call-level stats")
pb = txtProgressBar(min = 0, max = nrow(callclass), initial = 0, style = 3)
for(call in 1:nrow(callclass)) { # Foreach call
  apps <- subset(a, a$Callid == callclass$Callid[call])
  
  # Competition is the complement to the funding rate, i.e. the number of 
  # declined applications over the number of submissions.
  competition <- sum(!apps$granted)  / nrow(apps)
  NapplicationsInCall <- nrow(apps)
  NgrantedInCall <- sum(apps$granted)
  whichApps <- 
    a$Applicationreferencenumeric %in% apps$Applicationreferencenumeric
  a$competition[whichApps] <- competition
  a$NapplicationsInCall[whichApps] <- NapplicationsInCall
  a$NgrantedInCall[whichApps] <- NgrantedInCall
  callclass$competition[call] <- competition
  callclass$NapplicationsInCall[call] <- NapplicationsInCall
  callclass$NgrantedInCall[call] <- NgrantedInCall
  
  # Let's also keep track of the averege requested grant size:
  amounts <- apps$Appliedamount
  # ...removing NAs and zeros:
  amounts <- amounts[!is.na(amounts) & amounts > 0]
  if (length(amounts) == 0) {
    a$avgAppliedAmountInCall[whichApps] <- NA
    callclass$avgAppliedAmountInCall[call] <- NA
  } else {
    a$avgAppliedAmountInCall[whichApps] <- mean(amounts)
    callclass$avgAppliedAmountInCall[call] <- mean(amounts)
  } 
  
  # ... and the average requested grant size in each calltype and year
  amountsTY <- a$Appliedamount[
    a$Funding_type == callclass$Funding_type[call] &
      a$ApplicationYear == callclass$Applicationyear_new[call]
  ]
  # Removing NAs and zeros, again:
  amountsTY <- amountsTY[!is.na(amountsTY) & amountsTY > 0]
  # And updating the tables:
  if (length(amountsTY) == 0) {
    a$avgAppliedAmountInCalltypeYear[whichApps] <- NA
    callclass$avgAppliedAmountInCalltypeYear[call] <- NA
  } else {
    a$avgAppliedAmountInCalltypeYear[whichApps] <- mean(amountsTY)
    callclass$avgAppliedAmountInCalltypeYear[call] <- mean(amountsTY)
  } 
  
  # Then we calculate the "normalization factor" for the scores given in this 
  # call.
  # Let's fetch the scores first:
  scores <- as.numeric(subset(
    s,
    s$Applicationreferencenumeric %in% apps$Applicationreferencenumeric
  )$Assessmentscore)
  
  # Adding the number of known scores issued in the call:
  a$NknownScoresInCall[whichApps] <- length(scores)
  callclass$NknownScoresInCall[call] <- length(scores)
  
  # Adding the summary statistics for these scores:
  callScoresAvg <- mean(scores, na.rm = TRUE)
  callScoresQuint1 <- quantile(scores, prob = 0.2, na.rm = TRUE) # type = 7 is default
  callScoresQuint2 <- quantile(scores, prob = 0.4, na.rm = TRUE) #######
  callScoresQuint3 <- quantile(scores, prob = 0.6, na.rm = TRUE)
  callScoresQuint4 <- quantile(scores, prob = 0.8, na.rm = TRUE)
  
  a$callScoresAvg[whichApps] <- callScoresAvg
  a$callScoresQuint1[whichApps] <- callScoresQuint1
  a$callScoresQuint2[whichApps] <- callScoresQuint2
  a$callScoresQuint3[whichApps] <- callScoresQuint3
  a$callScoresQuint4[whichApps] <- callScoresQuint4
  
  callclass$callScoresAvg[call] <- callScoresAvg
  callclass$callScoresQuint1[call] <- callScoresQuint1
  callclass$callScoresQuint2[call] <- callScoresQuint2
  callclass$callScoresQuint3[call] <- callScoresQuint3
  callclass$callScoresQuint4[call] <- callScoresQuint4
  
  setTxtProgressBar(pb, call) # updating progress bar
}
close(pb)
  
# Creating call aliases so we don't have to work with long character strings:
class(a$Callname)
a$callAlias <- as.factor(a$Callname)
levels(a$callAlias) <- make.unique(
  names = rep(
    letters,
    length.out = length(levels(a$callAlias))
  ),
  sep = ""
)

  
#a$quantScoreAvg <- a$meanScore / a$callScoresAvg

# Here I'm counting how many unique reviewers reviewed each application,
# along with some stats on the distrbution of scores that were issued:
a$Nreviewers <- NA
a$sdScore <- a$meanScore <- a$medianScore <- a$bestScore <- a$worstScore <- NA

print("Updating application data with scores")
pb = txtProgressBar(min = 0, max = nrow(a), initial = 0, style = 3)
for (i in 1:nrow(a)) {
  id <- a$Applicationreferencenumeric[i]
  
  # These are the reviews it got:
  revs <- subset(s, s$Applicationreferencenumeric == id)
  
  # And this is the number of unique reviewers who gave it those grades:
  a$Nreviewers[i] <- length(unique(revs$Personsourcerecordid))
  
  # These are the standard deviation, mean, median, min and max score
  # for application i.
  scores <- as.numeric(revs$Assessmentscore)
  
  if (!is.null(scores)) {
    a$sdScore[i] <- sd(scores, na.rm = TRUE)
    a$meanScore[i] <- mean(scores, na.rm = TRUE)
    a$medianScore[i] <- median(scores, na.rm = TRUE)
    a$bestScore[i] <- min(scores, na.rm = TRUE)
    a$worstScore[i] <- max(scores, na.rm = TRUE)
  }
  
  setTxtProgressBar(pb, i) # updating progress bar
}
close(pb)





# Loading Shibayama ____________________________________________________________
datadir <- ""

novs <- read.csv(
  "./NNF Project 20231228/2.DATA/novelty/Shibayama/Novelty_Shibayama.csv")

# For convenience I also save the average of the two.
novs$shibayama_avg <- (novs$Novelty_score_title + novs$Novelty_score_abs) / 2

# And I assign it to the proposal data.frame
names(novs)[names(novs) == "Novelty_score_title"] <- "shibayama_title"
names(novs)[names(novs) == "Novelty_score_abs"] <- "shibayama_abs"
a <- merge(
  x = a,
  y = novs[,c(
    "Applicationid",
    "shibayama_title",
    "shibayama_abs",
    "shibayama_avg"
  )],
  by.x = "Applicationsourcerecordid",
  by.y = "Applicationid",
  all.x = TRUE
)
rm(novs)

# Loading MeSH-based metrics ___________________________________________________
#
# Loading novelty indicators based on single MeSH terms
novm <- read.csv2("./NIH MeSH/output/MeSH_novelty.csv")
a <- merge(
  x = a,
  y = novm,
  by.x = "Applicationsourcerecordid",
  by.y = "Applicationsourcerecordid",
  all.x = TRUE
)
rm(novm)

# And then loading indicators based on MeSH term pairs:
novm <- read.csv2("./NIH MeSH/output/MeSH_pairs_novelty.csv")
a <- merge(
  x = a,
  y = novm,
  by.x = "Applicationsourcerecordid",
  by.y = "Application.source.record.id",
  all.x = TRUE
)
rm(novm)

# Recoding one of the variables based on MeSH term pairs
a$novelty_new_MeSH_pairs_dic[is.na(a$novelty_new_MeSH_pairs_dic)] <- FALSE


# Loading Arts _________________________________________________________________
nova <- list()
nova$words <- read.csv(
  "./NNF Project 20231228/2.DATA/novelty/Melluso/ROUND 2/new_words.csv")
nova$bigrams <- read.csv(
  "./NNF Project 20231228/2.DATA/novelty/Melluso/ROUND 2/new_bigrams.csv")
nova$trigrams <- read.csv(
  "./NNF Project 20231228/2.DATA/novelty/Melluso/ROUND 2/new_trigrams.csv")


# Cleaning Arts ________________________________________________________________
# Checking what are the 20 most re-used Ngrams
nova$words[order(nova$words$reuse, decreasing = TRUE)[1:20],]
nova$bigrams[
  order(nova$bigrams$reuse, decreasing = TRUE)[1:20],]
nova$trigrams[
  order(nova$trigrams$reuse, decreasing = TRUE)[1:20],]

# Based on this, we can write some regular expression to filter out the most
# obvious, recurring and thus influential false positives. The following
# code removes all Ngrams containing:
#   - the "ha" token at the end of the string ("*_ha");
#   - the "ha" token at the start of the string ("ha_*");
#   - the "ha" token in the middle of the string ("*_ha_*");
#   - no letters at all (e.g. years or page numbers).
nova$words <- subset(
  nova$words,
  !endsWith(x = nova$words$words, suffix = "_ha") &
    !startsWith(x = nova$words$words, prefix = "ha_") &
    !grepl(pattern = "_ha_", x = nova$words$words, ignore.case = TRUE) &
    grepl(pattern = "[a-zA-Z]+", x = nova$words$words, ignore.case = TRUE)
)
nova$bigrams <- subset(
  nova$bigrams,
  !endsWith(x = nova$bigrams$bigrams, suffix = "_ha") &
    !startsWith(x = nova$bigrams$bigrams, prefix = "ha_") &
    !grepl(pattern = "_ha_", x = nova$bigrams$bigrams, ignore.case = TRUE) &
    grepl(pattern = "[a-zA-Z]+", x = nova$bigrams$bigrams, ignore.case = TRUE)
)
nova$trigrams <- subset(
  nova$trigrams,
  !endsWith(x = nova$trigrams$trigrams, suffix = "_ha") &
    !startsWith(x = nova$trigrams$trigrams, prefix = "ha_") &
    !grepl(pattern = "_ha_", x = nova$trigrams$trigrams, ignore.case = TRUE) &
    grepl(pattern = "[a-zA-Z]+", x = nova$trigrams$trigrams, ignore.case = TRUE)
)

# Calculating Arts final index:
# Let's tally all new Ngrams for each proposal:
mono <- as.matrix(table(nova$words$PaperID))
mono <- data.frame(id = as.numeric(rownames(mono)), monograms = mono[,1])
a <- merge(
  x = a, 
  y = mono, 
  by.x = "Applicationsourcerecordid", 
  by.y = "id",
  all.x = TRUE
)
bi <- as.matrix(table(nova$bigrams$PaperID))
bi <- data.frame(id = as.numeric(rownames(bi)), bigrams = bi[,1])
a <- merge(
  x = a, 
  y = bi, 
  by.x = "Applicationsourcerecordid", 
  by.y = "id",
  all.x = TRUE
)
tri <- as.matrix(table(nova$trigrams$PaperID))
tri <- data.frame(id = as.numeric(rownames(tri)), trigrams = tri[,1])
a <- merge(
  x = a, 
  y = tri, 
  by.x = "Applicationsourcerecordid", 
  by.y = "id",
  all.x = TRUE
)

# Replacing NAs with zeros
a$monograms[is.na(a$monograms)] <- 0
a$bigrams[is.na(a$bigrams)] <- 0
a$trigrams[is.na(a$trigrams)] <- 0


a$Ngrams <- a$monograms + a$bigrams + a$trigrams
#hist(a$Ngrams)

# And let's tally the reuses:
rmono <- stats::aggregate(
  x = nova$words$reuse,
  by = list(nova$words$PaperID),
  FUN = \(x) sum(x, na.rm = TRUE)
)
rbi <- stats::aggregate(
  x = nova$bigrams$reuse,
  by = list(nova$bigrams$PaperID),
  FUN = \(x) sum(x, na.rm = TRUE)
)
rtri <- stats::aggregate(
  x = nova$trigrams$reuse,
  by = list(nova$trigrams$PaperID),
  FUN = \(x) sum(x, na.rm = TRUE)
)
names(rmono) <- c("id", "monograms_reuses")
names(rbi) <- c("id", "bigrams_reuses")
names(rtri) <- c("id", "trigrams_reuses")
a <- merge(
  x = a, 
  y = rmono, 
  by.x = "Applicationsourcerecordid", 
  by.y = "id",
  all.x = TRUE
)
a <- merge(
  x = a, 
  y = rbi, 
  by.x = "Applicationsourcerecordid", 
  by.y = "id",
  all.x = TRUE
)
a <- merge(
  x = a, 
  y = rtri, 
  by.x = "Applicationsourcerecordid", 
  by.y = "id",
  all.x = TRUE
)

rm(mono, bi, tri, rmono, rbi, rtri)

# Replacing NAs with zeros
a$monograms_reuses[is.na(a$monograms_reuses)] <- 0
a$bigrams_reuses[is.na(a$bigrams_reuses)] <- 0
a$trigrams_reuses[is.na(a$trigrams_reuses)] <- 0

a$Ngrams_reuses <- a$monograms_reuses + a$bigrams_reuses + a$trigrams_reuses

# Finally, we get at the novelty score as of Arts, Melluso & Veugelers (2023), 
# which we call just "Arts" for short.
a$Arts <- a$Ngrams + a$Ngrams_reuses

# And we calculate a couple of related measures
a$log10Ngrams <- log10(a$Ngrams)
a$log10Ngrams[a$log10Ngrams == -Inf] <- 0

a$Ngrams_dic_1 <- a$Ngrams > 0 # at least one new Ngram to qualify as novel
a$Ngrams_dic_2 <- a$Ngrams > 1 # at least two new Ngrams to qualify
a$Ngrams_ordinal <- 0
a$Ngrams_ordinal[a$Ngrams == 1] <- 1
a$Ngrams_ordinal[a$Ngrams > 1] <- 2


# ______________________________________________________________________________
# Selection

# Total number of applications:
nrow(a)


# Now we filter out applications from calls with insufficient data or from
# irrelevant calls:
#
# (1) filtering out non-research calls:
table(a$calltype, useNA = "always")
table(
  Funding_type = callclass$Funding_type,
  calltype = callclass$calltype, 
  useNA = "always"
)
table(is.na(callclass$calltype))

a <- subset(a, !is.na(a$calltype))
callclass <- subset(callclass, !is.na(callclass$calltype))


# (2) filtering out by year:
# We only analyze calls run between 2012 and 2022. Outside of this range, NNF
# data seem partial or provisional
table(a$ApplicationYear)
table(a$ApplicationYear %in% 2012:2022)

#a <- subset(a, a$ApplicationYear %in% 2012:2021)
a <- subset(a, a$ApplicationYear %in% 2012:2022)

table(callclass$Callid %in% a$Callid)
callclass <- subset(callclass, callclass$Callid %in% a$Callid)

nrow(a) # = 14042
length(unique(a$Callid)) # = 323


# (3) filtering out calls with only one application:
summary(a$NapplicationsInCall < 2)
table(
  Funding_type = callclass$Funding_type,
  fewer_than_two_applications = callclass$NapplicationsInCall < 2, 
  useNA = "always"
)
a <- subset(a, a$NapplicationsInCall >= 2)
callclass <- subset(callclass, callclass$Callid %in% a$Callid)

nrow(a) # = 14024
length(unique(a$Callid)) # = 305


# (4) filtering out calls with no winners:
summary(a$NgrantedInCall == 0)
table(
  no_winners = callclass$NgrantedInCall == 0, useNA = "always"
)
table(
  Funding_type = callclass$Funding_type,
  no_winners = callclass$NgrantedInCall == 0, 
  useNA = "always"
)
a <- subset(a, a$NgrantedInCall > 0)
callclass <- subset(callclass, callclass$Callid %in% a$Callid)

nrow(a) # = 13512
length(unique(a$Callid)) # = 287


# (5) filtering out calls with no sufficient score data:
summary(a$NknownScoresInCall < 2)
table(
  Funding_type = callclass$Funding_type,
  fewer_than_two_known_grades = callclass$NknownScoresInCall < 2, 
  useNA = "always"
)
a <- subset(a, a$NknownScoresInCall > 1)
callclass <- subset(callclass, callclass$Callid %in% a$Callid)

nrow(a) # = 13106
length(unique(a$Callid)) # = 273


# Checking for presence of calls where randomization was used. Affected calls
# are described in an internal document that was circulated to us and which
# cannot be shared. Therefore, these calls are hard-coded here.
affected <- grep("synergy", a$Callname, ignore.case = TRUE, value = FALSE)
affected <- which(a[affected,]$ApplicationYear == 2022)
length(affected)
# There are no affected calls among the novelty-dedicated Synergy calls.

affected <- grep("project", a$Callname, ignore.case = TRUE, value = FALSE)
affected <- which(a[affected,]$ApplicationYear == 2022)
table(a[affected,]$Callname)
# Four calls are flagged here, but none of them is a 2022 call (these are older
# calls that received applications through 2022).

affected <- grep("research", a$Callname, ignore.case = TRUE, value = FALSE)
affected <- which(a[affected,]$ApplicationYear == 2022)
table(a[affected,]$Callname)
# Same here. Some calls are flagged, but none is affected.


# resulting data:
nrow(a)
nrow(callclass)
table(callclass$calltype)



# ______________________________________________________________________________
# Preliminary checks

#############################
# Dep. var.: funding success
table(
  Applicationstatus = a$Applicationstatus,
  granted = a$granted,
  useNA = "always"
)
table(a$granted)


##################################
# Indep. var.: Novelty-as-recency

# applications that have new MeSH terms:
summary(as.logical(a$novelty_dic_minMeSHlag_0days))
table(as.logical(a$novelty_dic_minMeSHlag_0days))

# applications that have new Ngrams:
summary(a$Ngrams)

ggplot(data = a, aes(x = Ngrams)) +
  geom_histogram(binwidth = 1, color = "white") +
  theme_minimal()

moments::kurtosis(a$Ngrams, na.rm = TRUE)
moments::kurtosis(log10(a$Ngrams + 1), na.rm = TRUE)

# It seems better to use a binary or ordinal recoding:
table(a$Ngrams_dic_1)
table(a$Ngrams_dic_2)
table(a$Ngrams_ordinal)


#####################################
# Indep. var.: Novelty-as-creativity 

# Shibayama
summary(a$shibayama_title) # notice there are many missings
moments::kurtosis(a$shibayama_title, na.rm = TRUE) # Hm. 

ggplot(data = a, aes(x = shibayama_title)) +
  geom_histogram(color = "white") +
  theme_minimal()

# Let's see where are all these missings:
table(
  year = a$ApplicationYear,
  missing_Shibayama_index = is.na(a$shibayama_title)
)


######################################
# call type: Synergy vs regular calls 
table(a$calltype)
table(
  Funding_type = a$Funding_type,
  calltype = a$calltype,
  useNA = "always"
)

# dropping all applications from discarded calls (those with calltype == NA)
#a <- subset(a, !is.na(a$calltype))


############
# Call year
#
# We have already dropped anything outside the range [2012,2022],
# but let's do it again in case we shifted things around:
#a <- subset(a, a$ApplicationYear %in% 2012:2022)
cbind(
  table(
    ApplicationYear = a$ApplicationYear,
    calltype = a$calltype
  ),
  total = table(a$ApplicationYear)
)

#########
# Gender
table(
  Mainapplicantgender = a$Mainapplicantgender,
  femalePI = a$femalePI,
  useNA = "always"
)


######
# Age
table(
  Mainapplicantage = a$Mainapplicantage,
  youngPI = a$youngPI,
  useNA = "always"
)
table(
  year = a$ApplicationYear,
  missing_Mainapplicantage = is.na(a$Mainapplicantage),
  useNA = "always"
)
# or, better:
summary(a$Mainapplicantage)
summary(a$youngPI)
ggplot(
  data = a,
  #aes(x = Mainapplicantage, fill = as.factor(youngPI))) +
  aes(
    x = Mainapplicantage, 
    fill = as.factor(youngPI)
  )
) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(fill = "youngPI") +
  scale_x_continuous(breaks = c(1:20 * 5), minor_breaks = c(1:100)) +
  theme_minimal()


##############
# Competition
summary(a$competition)

ggplot(data = a, aes(x = competition)) +
  geom_histogram(color = "white") +
  theme_minimal()


#########################
# Average score received
summary(a$meanScore)
ggplot(data = a, aes(x = meanScore)) +
  geom_histogram(color = "white") +
  theme_minimal()


###################
# Requested amount
summary(a$Appliedamount)

# This requires some cleaning. Let's set up a temporary variable for that.
temp <- a$Appliedamount

# Now let's see how many missings there are
table(is.na(temp))
table(temp == 0) # 0 requested amount still counts as missing!
temp[temp == 0] <- NA

# Let's see if we can fill in the blanks with the awarded amount:
table(
  missingRequested = is.na(temp),
  missingGranted = is.na(a$Grantedamount)
)
table(
  missingRequested = is.na(temp),
  zeroGranted = a$Grantedamount == 0 
)
# This helps us a bit. There are 136 cases where the missing applied amount
# can be replaced by a non-zero granted amount. Let's do the filling in.
whichMissing <- is.na(temp)
temp[whichMissing] <- a$Grantedamount[whichMissing]
temp[temp == 0] <- NA
table(is.na(temp))

# The next thing we can do to fill in the blanks is to assign the call-level
# average applied amount, whenever available, for all proposals where the
# actual applied amount is missing.
table(
  missingRequested = is.na(temp),
  missingAvgRequested = is.na(a$avgAppliedAmountInCall) 
)
# Seems viable. We have 150 applications for which we can make an imputation
# based on their call average.
whichMissing <- is.na(temp)
temp[whichMissing] <- a$avgAppliedAmountInCall[whichMissing]
temp[temp == 0] <- NA
table(is.na(temp))

# Last thing we can try is to use the average requested amount in that year
# among all applications of the same "Funding_type"
whichMissing <- is.na(temp)
table(
  missingRequested = whichMissing,
  missingAvgReqByCalltypeYear = is.na(a$avgAppliedAmountInCalltypeYear) 
)
temp[whichMissing] <- a$avgAppliedAmountInCalltypeYear[whichMissing]
temp[temp == 0] <- NA
table(is.na(temp))
# We have a few remaining missing values.


# Let's adjust these values for inflation.
a$Appliedamount_corrected <- temp

# Let's adjust it for inflation.
a$Appliedamount_corrected <- priceR::adjust_for_inflation(
  price = a$Appliedamount_corrected,
  from_date = a$ApplicationYear,
  country = "DK",
  to_date = 2022,
  inflation_dataframe = priceR::retrieve_inflation_data("DK")
)

plot(a$Appliedamount, a$Appliedamount_corrected)
# Looks fine. From the plot we can see that many Appliedamounts that were 0 were
# now filled in.



ggplot(data = a, aes(x = Appliedamount_corrected)) +
  geom_histogram(color = "white") +
  theme_minimal()
summary(a$Appliedamount_corrected)
moments::kurtosis(a$Appliedamount_corrected, na.rm = TRUE) 
moments::kurtosis(log10(a$Appliedamount_corrected), na.rm = TRUE) 

a$Appliedamount_corrected_log <- log10(a$Appliedamount_corrected)
a$Appliedamount_corrected_log_sqr <- a$Appliedamount_corrected_log ** 2

###################
# Total numerosity
table(
  is.na(a$shibayama_title) |
  is.na(a$granted) |
    is.na(a$ApplicationYear) |
    is.na(a$calltype) |
    is.na(a$femalePI) |
    is.na(a$youngPI) |
    is.na(a$competition) |
    is.na(a$meanScore) |
    is.na(a$Appliedamount_corrected_log)
)


# ______________________________________________________________________________
# Bivariate correlation matrix

a$synergy <- a$calltype == "Synergy calls"
correlogram <- cor(
  x = cbind(
    granted = as.numeric(a$granted),
    recency_Ngrams_logged = as.numeric(a$log10Ngrams),
    recency_MeSH = as.numeric(a$novelty_dic_minMeSHlag_0days),
    recency_MeSH_pairs = as.numeric(a$novelty_new_MeSH_pairs_dic),
    #recency_Ngrams_dic2 = as.numeric(a$Ngrams_dic_2),
    creativity_Shibayama = a$shibayama_title,
    year = a$ApplicationYear,
    synergy = as.numeric(a$synergy),
    femalePI = as.numeric(a$femalePI),
    youngPI = as.numeric(a$youngPI),
    competition = a$competition,
    meanScore = a$meanScore,
    applied_amount_logged = a$Appliedamount_corrected_log
  ),
  use = "complete.obs",
  method = "spearman"
)

if (FALSE) {
  cortable <- corrr::correlate(
    x = cbind(
      granted = as.numeric(a$granted),
      recency_MeSH = as.numeric(a$novelty_dic_minMeSHlag_0days),
      recency_Ngrams_dic2 = as.numeric(a$Ngrams_dic_2),
      #recency_Ngrams_logged = as.numeric(a$log10Ngrams),
      creativity_Shibayama = a$shibayama_title,
      year = a$ApplicationYear,
      synergy = as.numeric(a$synergy),
      femalePI = as.numeric(a$femalePI),
      youngPI = as.numeric(a$youngPI),
      competition = a$competition,
      meanScore = a$meanScore,
      applied_amount_logged = a$Appliedamount_corrected_log
    ),
    use = "complete.obs",
    method = "spearman",
    diagonal = 1
  )
}


correlogram_p <- corrplot::cor.mtest(
  mat = as.matrix(cbind(
    granted = as.numeric(a$granted),
    recency_Ngrams_logged = as.numeric(a$log10Ngrams),
    recency_MeSH = as.numeric(a$novelty_dic_minMeSHlag_0days),
    recency_MeSH_pairs = as.numeric(a$novelty_new_MeSH_pairs_dic),
    #recency_Ngrams_dic2 = as.numeric(a$Ngrams_dic_2),
    creativity_Shibayama = a$shibayama_title,
    year = a$ApplicationYear,
    synergy = as.numeric(a$synergy),
    femalePI = as.numeric(a$femalePI),
    youngPI = as.numeric(a$youngPI),
    competition = a$competition,
    meanScore = a$meanScore,
    applied_amount_logged = a$Appliedamount_corrected_log
  )),
  use = "complete.obs",
  method = "spearman"
)

corrplot::corrplot(
  corr = correlogram,
  p.mat = correlogram_p$p,
  type = "lower",
  method = "color",
  sig.level = 0.05
  #diag = FALSE,
  #tl.pos = "d"
)

# Exporting to table format

# First I round all estimates to the nearest three decimals
cortable <- round(correlogram, digits = 3)

# Then I convert all numeric values to character strings.
# While at it I add a "new line" to each value to make room for significance 
# levels/stars.
cortable <- apply(
  X = cortable, 
  MARGIN = c(1, 2), 
  FUN = \(value) paste(as.character(value), "", sep = "\n")
)

# Then I 
#cortable <- paste0(cortable, "\n")

# Then I add the significance level:
for (r in 1:nrow(cortable)) for (c in 1:ncol(cortable)) {
  p <- correlogram_p$p[r, c]
  
  # Here is how I map p-values to stars.
  stars <- ""
  if (p < 0.1) stars <- "."
  if (p < 0.05) stars <- "*"
  if (p < 0.01) stars <- "**"
  if (p < 0.001) stars <- "***"
  
  # Appending stars to their value.
  cortable[r, c] <- paste0(cortable[r, c], stars)
  
  # Removing top of the diagonal
  if (c > r) cortable[r, c] <- ""
}

writexl::write_xlsx(
  x = cbind(names(cortable[1,]), as.data.frame(cortable)), 
  path = "./outputGraphics/bivariateCorr.xlsx"
)


# ______________________________________________________________________________
# Cronbach's alpha 

# Versione continua
psych::alpha(data.frame( # with all measures
  novelty_single_MeSH_terms = a$novelty_minMeSHlag,
  novelty_MeSH_term_pairs = normalize(a$novelty_new_MeSH_pairs_count),
  novelty_Ngrams = normalize(a$Ngrams),
  novelty_Shibayama = a$shibayama_title
))
psych::alpha(data.frame( # Excluding Shibayama (thus with higher N)
  novelty_single_MeSH_terms = a$novelty_minMeSHlag,
  novelty_MeSH_term_pairs = normalize(a$novelty_new_MeSH_pairs_count),
  novelty_Ngrams = normalize(a$Ngrams)#,
  #novelty_Shibayama = a$shibayama_title
))

# Versione dicotomica
psych::alpha(data.frame( # with all measures
  novelty_single_MeSH_terms = as.numeric(a$novelty_dic_minMeSHlag_0days),
  novelty_MeSH_term_pairs = as.numeric(a$novelty_new_MeSH_pairs_dic),
  novelty_Ngrams = as.numeric(a$Ngrams_dic_2),
  novelty_Shibayama = a$shibayama_title
))
psych::alpha(data.frame( # without Shibayama (thus with higher N)
  novelty_single_MeSH_terms = as.numeric(a$novelty_dic_minMeSHlag_0days),
  novelty_MeSH_term_pairs = as.numeric(a$novelty_new_MeSH_pairs_dic),
  novelty_Ngrams = as.numeric(a$Ngrams_dic_2)#,
  #novelty_Shibayama = a$shibayama_title
))
psych::alpha(data.frame( # without Shibayama and Ngrams
  novelty_single_MeSH_terms = as.numeric(a$novelty_dic_minMeSHlag_0days),
  novelty_MeSH_term_pairs = as.numeric(a$novelty_new_MeSH_pairs_dic)#,
  #novelty_Ngrams = as.numeric(a$Ngrams_dic_2),
  #novelty_Shibayama = a$shibayama_title
))

# IRT
# Fit a 2-parameter logistic model
irt_d <- data.frame( # with all measures
  novelty_single_MeSH_terms = as.numeric(a$novelty_dic_minMeSHlag_0days),
  novelty_MeSH_term_pairs = as.numeric(a$novelty_new_MeSH_pairs_dic),
  novelty_Ngrams = as.numeric(a$Ngrams_dic_2),
  novelty_Shibayama = a$shibayama_title > quantile(
    a$shibayama_title, 
    probs = 0.75,
    na.rm = TRUE,
  )
)
irt_model <- ltm::ltm(irt_d ~ z1)
summary(irt_model)



# ______________________________________________________________________________
# Checking how many scores we have

nrow(s)
tempS <- subset(
  s,
  s$Applicationreferencenumeric %in% a$Applicationreferencenumeric
)

# finding instances of reviewers giving more than one score to the same proposal
reviewers <- unique(tempS$Personid)
repeatReviews <- rep(NA, times = length(reviewers))

for (rev in 1:length(reviewers)) {
  x <- tempS[tempS$Personid == reviewers[rev],]
  
  repeatReviews[rev] <- sum(table(x$Applicationreferencenumeric) > 1)
}

table(repeatReviews)
table(repeatReviews > 0)

rm(tempS, reviewers, repeatReviews, x, proposals)

# How many scores per call type?
tempS <- merge(
  x = s,
  y = a,
  by = "Applicationreferencenumeric"#,
  #all.x = TRUE
)
table(tempS$synergy)

rm(tempS)


#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
# ______________________________________________________________________________
# Hypothesis testing

binaryModel = "probit"

# This is the independent variable:
#a$novelty = scale(a$log10Ngrams)[,1]; indepvar = "recency_Ngrams_logged"
#a$novelty = scale(a$Ngrams_dic_2)[,1]; indepvar = "recency_Ngrams_dic2"

#a$novelty = as.factor(a$novelty_dic_minMeSHlag_0days); indepvar = "recency_MeSH"
#a$novelty = scale(a$novelty_medianMeSHlag)[,1]; indepvar = "recency_MeSH" #####

#a$novelty = scale(a$novelty_new_MeSH_pairs_dic)[,1]; indepvar = "recency_MeSH_pairs" 


a$novelty = a$novelty_dic_minMeSHlag_0days; indepvar = "recency_MeSH"
#a$novelty = scale(a$shibayama_title)[,1]; indepvar = "creativity_Shibayama"




a$regular = !a$synergy

a$S1_syn_by_novelty <- a$synergy * a$novelty
a$S2_reg_by_novelty <- a$regular * a$novelty


model1 <- glm( #
  data = a,
  family = binomial(link = binaryModel),
  granted ~ 
    novelty +
    synergy
)

model2 <- glm( # Split approach
  data = a,
  family = binomial(link = binaryModel),
  granted ~ 
    #novelty + #####################################
    synergy +
    S2_reg_by_novelty +
    S1_syn_by_novelty
)
#summary(model4)

model3 <- glm( # Split approach with controls
  data = a,
  family = binomial(link = binaryModel),
  granted ~ 
    #novelty + #####################################
    synergy +
    S2_reg_by_novelty +
    S1_syn_by_novelty +
    as.factor(malePI) +
    as.factor(youngPI) +
    scale(competition) +
    scale(meanScore) +
    scale(Appliedamount_corrected_log) +
    #scale(Appliedamount_corrected_log_sqr) +
    as.factor(ApplicationYear)# +
    #callAlias # call f.e.
)
#summary(model5)





summary(model1)
summary(model2)
summary(model3)


stargazer::stargazer(
  model1, model2, model3, #model4, model5,
  type = "html",#"text", # "text" "html"
  out = "./outputGraphics/probit_models_table_3.html",
  dep.var.labels = "funding success (granted)",
  star.char = c("·", "*", "**", "***"),
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  notes = c("· p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
  notes.append = FALSE,
  intercept.bottom = FALSE#,
  #order = c(1:3, length(model3$coefficients), 4:(length(model3$coefficients)-1))
)

car::linearHypothesis(model3, "S2_reg_by_novelty = S1_syn_by_novelty")


# Testing if the estimates for the two split variables differ significantly
coefficients <- coef(summary(model3)) # Extract coefficients table
vcov_matrix <- vcov(model3) 
# Coefficients
beta1 <- coefficients["S2_reg_by_novelty", "Estimate"]
beta2 <- coefficients["S1_syn_by_novelty", "Estimate"]
# Variances and covariance
var_beta1 <- vcov_matrix["S2_reg_by_novelty", "S2_reg_by_novelty"]
var_beta2 <- vcov_matrix["S1_syn_by_novelty", "S1_syn_by_novelty"]
cov_beta1_beta2 <- vcov_matrix["S2_reg_by_novelty", "S1_syn_by_novelty"]
# Variance of the difference
var_diff <- var_beta1 + var_beta2 - 2 * cov_beta1_beta2
# Wald test statistic
W <- (beta1 - beta2)^2 / var_diff
# p-value
p_value <- 1 - pchisq(W, df = 1)
# Print results
cat("Wald test statistic:", W, "\n")
cat("p-value:", p_value, "\n")





#names(model3$coefficients)[2] <- "log10Ngrams"
#x = marginaleffects::plot_predictions(
marginaleffects::plot_predictions(
  model = model3,
  #condition = c("novelty", "synergy"),#c("novelty", "calltype"),
  condition = c("S2_reg_by_novelty", "S1_syn_by_novelty"),#c("novelty", "calltype"),
  conf_level = 0.95
) + 
  labs(
    x = indepvar,
    #title = "Marginal effects from model 3",
    subtitle = "confidence level = 0.95"
  ) +
  theme_minimal()


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Checking for differences between the pool of reviewers for Synergy and 
# regular calls.
#
# Let's start with creating a dataset in a shape that's convenient for us:
reviews <- s[,c("Personsourcerecordid", "Applicationsourcerecordid")]
names(reviews) <- c("reviewer", "application")

# Removing all reviews for calls that we've excluded:
reviews <- subset(reviews, reviews$application %in% a$Applicationsourcerecordid)

# This is the total number of reviews made for the calls we're examining:
nrow(reviews)

# .. and this is how many reviewers have contributed:
length(unique(reviews$reviewer))

# Time add a variable to identify synergy vs regular calls.
reviews$synergy <- FALSE # initializing variable
synapp <- a$Applicationsourcerecordid[a$synergy] # IDs of synergy applications
reviews$synergy[reviews$application %in% synapp] <- TRUE
rm(synapp)

# Now let's create a dataset with reviewers as rows.
reviewer <- data.frame(
  reviewer = unique(reviews$reviewer),
  regular = FALSE,
  synergy = FALSE,
  nreviews = NA,
  nregularreviews = 0,
  nsynergyreviews = 0
)

# Now, for each reviewer let's check if they made a review for a regular call,
# a synergy call or both:
for (i in 1:nrow(reviewer)) { # for each reviewer...
  
  # ... find the reviews they made and keep track if it was within synergy...
  x <- reviews[reviews$reviewer == reviewer$reviewer[i],]$synergy
  
  # Is there any regular (i.e. x == FALSE) among i's reviews? If so, mark
  # "regular" as TRUE.
  if(any(!x)) reviewer$regular[i] <- TRUE
  
  # Do the same for synergy: if there is at least one review done for a 
  # synergy application (x == TRUE)? If so, mark "synergy" as TRUE.
   if(any(x)) reviewer$synergy[i] <- TRUE
   
   # Last, let's also keep track of how reviewers' workload:
   reviewer$nreviews[i] = length(x)
   reviewer$nregularreviews[i] = sum(!x)
   reviewer$nsynergyreviews[i] = sum(x)
}

# Now let's cross-tabulate and see how different are the two sets of reviewers
# (synergy vs regular reviewers):
table(regular = reviewer$regular, synergy = reviewer$synergy)
#psych::cohen.kappa(table(reviewer$regular, reviewer$synergy))

# average number of reviews for synergy and for regular reviewers:
summary(reviewer$nregularreviews[reviewer$regular]) # regular
summary(reviewer$nsynergyreviews[reviewer$synergy]) # synergy



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
#
# Plotting novelty in synergy vs regular, submitted vs granted
#
# Let's write all we need in a table first:
table1 <- data.frame(
  call = c(
    rep("Regular calls", times = 5), 
    rep("Novelty-dedicated calls", times = 5)
  ),
  group = rep(
    c(
      "A) All proposals",
      "B) Funded proposals",
      "C) Declined proposals",
      "Δ ([B]-[A])",
      "Δ ([B]-[C])"
    ),
    times = 2
  ),
  Vintage_mean = NA,
  Vintage_sd = NA,
  Recombination_mean = NA,
  Recombination_sd = NA
)

# Let's populate it, starting with the regular calls:
# all submissions
rnd = 3 # rounding digits

for(call in unique(table1$call)) {
  ifelse(
    call == "Regular calls",
    x <- a[!a$synergy,],
    x <- a[a$synergy,]
  )
  
  table1$Vintage_mean[
    table1$call == call & 
      table1$group == "A) All proposals"] <- 
    round(mean(x$novelty_dic_minMeSHlag_0days, na.rm = TRUE), digits = rnd)
  table1$Vintage_sd[
    table1$call == call & 
      table1$group == "A) All proposals"] <- 
    round(sd(x$novelty_dic_minMeSHlag_0days, na.rm = TRUE), digits = rnd)
  table1$Recombination_mean[
    table1$call == call & 
      table1$group == "A) All proposals"] <- 
    round(mean(x$shibayama_title, na.rm = TRUE), digits = rnd)
  table1$Recombination_sd[
    table1$call == call & 
      table1$group == "A) All proposals"] <- 
    round(sd(x$shibayama_title, na.rm = TRUE), digits = rnd)
  
  # funded submissions
  table1$Vintage_mean[
    table1$call == call & 
      table1$group == "B) Funded proposals"] <- 
    round(mean(x$novelty_dic_minMeSHlag_0days[x$Applicationstatus == "Granted"], 
               na.rm = TRUE), digits = rnd)
  table1$Vintage_sd[
    table1$call == call & 
      table1$group == "B) Funded proposals"] <- 
    round(sd(x$novelty_dic_minMeSHlag_0days[x$Applicationstatus == "Granted"],
             na.rm = TRUE), digits = rnd)
  table1$Recombination_mean[
    table1$call == call & 
      table1$group == "B) Funded proposals"] <- 
    round(mean(x$shibayama_title[x$Applicationstatus == "Granted"], 
               na.rm = TRUE), digits = rnd)
  table1$Recombination_sd[
    table1$call == call & 
      table1$group == "B) Funded proposals"] <- 
    round(sd(x$shibayama_title[x$Applicationstatus == "Granted"], 
             na.rm = TRUE), digits = rnd)
  
  # declined submissions
  table1$Vintage_mean[
    table1$call == call & 
      table1$group == "C) Declined proposals"] <- 
    round(mean(x$novelty_dic_minMeSHlag_0days[x$Applicationstatus != "Granted"], 
               na.rm = TRUE), digits = rnd)
  table1$Vintage_sd[
    table1$call == call & 
      table1$group == "C) Declined proposals"] <- 
    round(sd(x$novelty_dic_minMeSHlag_0days[x$Applicationstatus != "Granted"],
             na.rm = TRUE), digits = rnd)
  table1$Recombination_mean[
    table1$call == call & 
      table1$group == "C) Declined proposals"] <- 
    round(mean(x$shibayama_title[x$Applicationstatus != "Granted"], 
               na.rm = TRUE), digits = rnd)
  table1$Recombination_sd[
    table1$call == call & 
      table1$group == "C) Declined proposals"] <- 
    round(sd(x$shibayama_title[x$Applicationstatus != "Granted"], 
             na.rm = TRUE), digits = rnd)
  
  # B - A
  tt = t.test(
    x = x$novelty_dic_minMeSHlag_0days[x$Applicationstatus == "Granted"],
    y = x$novelty_dic_minMeSHlag_0days
  )
  stars <- ""
  if (tt$p.value < 0.1) stars <- "."
  if (tt$p.value < 0.05) stars <- "*"
  if (tt$p.value < 0.01) stars <- "**"
  if (tt$p.value < 0.001) stars <- "***"
  table1$Vintage_mean[
    table1$call == call & 
      table1$group == "Δ ([B]-[A])"] <- 
    paste(
      as.character(round(tt$estimate[1] - tt$estimate[2], digits = rnd)),
      stars
    )
  sdb = sd(x$novelty_dic_minMeSHlag_0days[x$Applicationstatus == "Granted"], na.rm = T)
  sda = sd(x$novelty_dic_minMeSHlag_0days, na.rm = T)
  sd_delta <- sqrt((sdb^2 / length(sdb)) + (sda^2 / length(sda)))
  table1$Vintage_sd[
    table1$call == call & 
      table1$group == "Δ ([B]-[A])"
    ] <- round(sd_delta, digits = rnd)
  
  tt = t.test(
    x = x$shibayama_title[x$Applicationstatus == "Granted"],
    y = x$shibayama_title
  )
  stars <- ""
  if (tt$p.value < 0.1) stars <- "."
  if (tt$p.value < 0.05) stars <- "*"
  if (tt$p.value < 0.01) stars <- "**"
  if (tt$p.value < 0.001) stars <- "***"
  table1$Recombination_mean[
    table1$call == call & 
      table1$group == "Δ ([B]-[A])"] <- 
    paste(
      as.character(round(tt$estimate[1] - tt$estimate[2], digits = rnd)),
      stars
    )
  sdb = sd(x$shibayama_title[x$Applicationstatus == "Granted"], na.rm = T)
  sda = sd(x$shibayama_title, na.rm = T)
  sd_delta <- sqrt((sdb^2 / length(sdb)) + (sda^2 / length(sda)))
  table1$Recombination_sd[
    table1$call == call & 
      table1$group == "Δ ([B]-[A])"
  ] <- round(sd_delta, digits = rnd)
  
  # B - C
  tt = t.test(
    x = x$novelty_dic_minMeSHlag_0days[x$Applicationstatus == "Granted"],
    y = x$novelty_dic_minMeSHlag_0days[x$Applicationstatus != "Granted"]
  )
  stars <- ""
  if (tt$p.value < 0.1) stars <- "."
  if (tt$p.value < 0.05) stars <- "*"
  if (tt$p.value < 0.01) stars <- "**"
  if (tt$p.value < 0.001) stars <- "***"
  table1$Vintage_mean[
    table1$call == call & 
      table1$group == "Δ ([B]-[C])"] <- 
    paste(
      as.character(round(tt$estimate[1] - tt$estimate[2], digits = rnd)),
      stars
    )
  sdb = sd(x$novelty_dic_minMeSHlag_0days[x$Applicationstatus == "Granted"], na.rm = T)
  sda = sd(x$novelty_dic_minMeSHlag_0days[x$Applicationstatus != "Granted"], na.rm = T)
  sd_delta <- sqrt((sdb^2 / length(sdb)) + (sda^2 / length(sda)))
  table1$Vintage_sd[
    table1$call == call & 
      table1$group == "Δ ([B]-[C])"
  ] <- round(sd_delta, digits = rnd)
  
  tt = t.test(
    x = x$shibayama_title[x$Applicationstatus == "Granted"],
    y = x$shibayama_title[x$Applicationstatus != "Granted"]
  )
  stars <- ""
  if (tt$p.value < 0.1) stars <- "."
  if (tt$p.value < 0.05) stars <- "*"
  if (tt$p.value < 0.01) stars <- "**"
  if (tt$p.value < 0.001) stars <- "***"
  table1$Recombination_mean[
    table1$call == call & 
      table1$group == "Δ ([B]-[C])"] <- 
    paste(
      as.character(round(tt$estimate[1] - tt$estimate[2], digits = rnd)),
      stars
    )
  sdb = sd(x$shibayama_title[x$Applicationstatus == "Granted"], na.rm = T)
  sda = sd(x$shibayama_title[x$Applicationstatus != "Granted"], na.rm = T)
  sd_delta <- sqrt((sdb^2 / length(sdb)) + (sda^2 / length(sda)))
  table1$Recombination_sd[
    table1$call == call & 
      table1$group == "Δ ([B]-[C])"
  ] <- round(sd_delta, digits = rnd)
  
}

# Adding lengend
table1[nrow(table1) + 1, 1] <- "•=p<0.1; *=p<0.05; **=p<0.01; ***=p<0.001"

# replace NAs with empty strings
for (r in 1:nrow(table1)) for (c in 1:ncol(table1)) {
  if (is.na(table1[r, c])) table1[r, c] <- ""
}

# export
writexl::write_xlsx(
  table1, 
  path = "./outputGraphics/table1.xlsx"
)


# Now plotting the same information ____________________________________________
#
# Constructing an ad-hoc dataframe:

a$novelty <- a$novelty_dic_minMeSHlag_0days # keep this as integer
#a$novelty <- a$shibayama_title
df <- a[,c("novelty", "synergy", "granted")]

# recoding:
df$call <- "regular call"
df$call[df$synergy] <- "novelty-dedicated call"
df$synergy <- NULL

df$group <- "declined proposals"
df$group[df$granted] <- "granted proposals"
df$granted <- NULL

df$variable <- "vintage"

#t.test(df$novelty[df$call == "regular call"] ~ df$group[df$call == "regular call"])

# Adding the other measure of novelty:
df2 <- df
df2$novelty <-a$shibayama_title
df2$variable <- "recombination"

df <- rbind(df, df2)
rm(df2)



dff <- expand.grid(call = unique(df$call), variable = unique(df$variable))

for (i in 1:nrow(dff)) {
  icall = dff$call[i]
  ivar = dff$variable[i]
  group_summary <- subset(df, df$call == icall & df$variable == ivar) %>%
    group_by(group) %>%
    summarise(mean_novelty = mean(novelty, na.rm = TRUE),
              sd_novelty = sd(novelty, na.rm = TRUE),
              n = n())
  
  # Calculate the difference in means between the two groups
  mean_diff <- diff(group_summary$mean_novelty)
  
  # Calculate the pooled standard error of the difference
  se_diff <- sqrt((group_summary$sd_novelty[1]^2 / group_summary$n[1]) +
                    (group_summary$sd_novelty[2]^2 / group_summary$n[2]))
  
  # Calculate 95% confidence interval for the difference in means
  ci_lower <- mean_diff - 1.96 * se_diff
  ci_upper <- mean_diff + 1.96 * se_diff
  
  dff$group[i] <- "difference betw.\nrejected and granted"
  #dff$group[i] <- icall
  dff$mean_diff[i] <- mean_diff
  dff$se_diff[i] <- se_diff
  dff$lower[i] <- ci_lower
  dff$upper[i] <- ci_upper
}




Figure_1 <- ggplot(
  data = dff, 
  aes(x = mean_diff, y = group, xmin = lower, xmax = upper)
) +
  geom_bar(stat = "identity", fill = "gray60", width = 0.5) +
  geom_errorbar(width = 0.1) +
  labs(
    title = NULL,
    x = NULL, 
    y = NULL) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  facet_grid(
    call ~ variable,
    #labeller = labeller(
    #  variable = c(
    #    "vintage" = "prosal vintage\n(declined - granted)",
    #    "recombination" = "prosal recombination\n(declined - granted)"
    #  )
    #),
    scales = "free"#,
    #strip.position = "left"
  ) +
  scale_x_continuous(
    breaks = c(0),
    labels = c("conservatism ←                             ")
  ) +
  geom_rect(
    aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), 
    fill = "gray60", 
    alpha = 0.2
  ) +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank()#,
    #strip.position = "bottom"
    #strip.position = c("top", "left")
    #strip.position = "left"
    #strip.text.y = element_text(angle = 0, hjust = 1)
    #strip.text.y = element_text(angle = 0, hjust = 1)
  )
print(Figure_1)

ggsave(
  "./outputGraphics/Figure_1.png",
  plot = Figure_1,
  width = 2000,
  height = 950,
  units = "px",
  bg = "white",
  dpi = 300
)













#############################
# Then: does novelty really undercut one's chances of securing the grant?
# Even kijken
#
# Starting with novelty as recency _____________________________________________

ggplot(data = a[!is.na(a$calltype),], aes(
  #y = granted,
  x = granted,
  fill = as.factor(novelty_dic_minMeSHlag_0days)
)) +
  geom_bar(position = "fill") +
  facet_grid(cols = vars(calltype)) + 
  #geom_violin() +
  theme_minimal() +
  theme(legend.position = "top")

ggplot(a[!is.na(a$calltype),], aes(##########
  #y = granted,
  x = granted,
  fill = as.factor(Ngrams != 0)
)) +
  geom_bar(position = "fill") +
  facet_grid(cols = vars(calltype)) + 
  #geom_violin() +
  theme_minimal() +
  theme(legend.position = "top")

# Confusion matrices
a_syn <- subset(a, a$calltype == "Synergy calls")
a_reg <- subset(a, a$calltype == "regular calls")
table(a_syn$novelty_dic_minMeSHlag_0days, a_syn$granted)
table(a_reg$novelty_dic_minMeSHlag_0days, a_reg$granted)

# Testing correlation on the confusion matrices with Cohen's Kappa
irr::kappa2(ratings = as.matrix(cbind(
  as.numeric(a_reg$granted),
  a_reg$novelty_dic_minMeSHlag_0days
)))
# There is significant agreement for regular calls (novelty predicts rejection).

irr::kappa2(ratings = as.matrix(cbind(
  as.numeric(a_syn$granted),
  a_syn$novelty_dic_minMeSHlag_0days
)))
# No significant effect for Synergy calls.

summary(glm( # effect of novelty
  data = a[!is.na(a$calltype),],
  family = binomial(link = "probit"), 
  granted ~ novelty_dic_minMeSHlag_0days
))
summary(glm( # effect of novelty and call type
  data = a[!is.na(a$calltype),],
  family = binomial(link = "probit"), 
  granted ~ 
    novelty_dic_minMeSHlag_0days +
    calltype
))
summary(glm( # interaction novelty:calltype
  data = a[!is.na(a$calltype),],
  family = binomial(link = "probit"), 
  granted ~ 
    novelty_dic_minMeSHlag_0days +
    calltype +
    novelty_dic_minMeSHlag_0days * calltype
))
summary(glm( # throwing in controls
  data = a[!is.na(a$calltype),],
  family = binomial(link = "probit"), 
  granted ~ 
    novelty_dic_minMeSHlag_0days +
    calltype +
    novelty_dic_minMeSHlag_0days * calltype +
    femalePI +
    youngPI +
    scale(meanScore)
))





# Next, novelty as recombinatorial creativity __________________________________
ggplot(data = a, aes(
  y = shibayama_title,
  x = granted,
  id = calltype,
  fill = calltype
)) +
  geom_violin(width = 0.5, fill = "gray75", color = "gray75") +
  geom_boxplot(width = 0.5) +
  theme_minimal() 

# Testing more formally.

summary(glm( # effect of novelty
  data = a[!is.na(a$calltype),],
  family = binomial(link = "probit"), 
  granted ~ scale(shibayama_title)
))
summary(glm( # effect of novelty and call type
  data = a[!is.na(a$calltype),],
  family = binomial(link = "probit"), 
  granted ~ 
    scale(shibayama_title) +
    calltype
))
summary(glm( # interaction novelty:calltype
  data = a[!is.na(a$calltype),],
  family = binomial(link = "probit"), 
  granted ~ 
    scale(shibayama_title) +
    calltype +
    scale(shibayama_title) * calltype
))
summary(glm( # throwing in controls
  data = a[!is.na(a$calltype),],
  family = binomial(link = "probit"), 
  granted ~ 
    scale(shibayama_title) +
    calltype +
    scale(shibayama_title) * calltype +
    femalePI +
    youngPI +
    meanScore
))




# First for regular calls:
print(c(
  shybayama_of_granted_applicatioins = mean(
    a_reg$shibayama_title[a_reg$granted], 
    na.rm = TRUE
  ),
  shybayama_of_declined_applicatioins = mean(
    a_reg$shibayama_title[!a_reg$granted], 
    na.rm = TRUE
  )
))
t.test(
  x = a_reg$shibayama_title[a_reg$granted],
  y = a_reg$shibayama_title[!a_reg$granted],
  alternative = "two.sided",
  var.equal = FALSE
)
# The odds are indeed stacked against novel applications in regular calls.
# The effect is just shy of significance, though.

# Checking Synergy calls:
print(c(
  shybayama_of_granted_applicatioins = mean(
    a_syn$shibayama_title[a_syn$granted], 
    na.rm = TRUE
  ),
  shybayama_of_declined_applicatioins = mean(
    a_syn$shibayama_title[!a_syn$granted], 
    na.rm = TRUE
  )
))
t.test(
  x = a_syn$shibayama_title[a_reg$granted],
  y = a_syn$shibayama_title[!a_reg$granted],
  alternative = "two.sided",
  var.equal = FALSE
)
# In Synergy calls, novelty is rewarded. The effect is significant at p<0.05


# Grades vs novelty ____________________________________________________________
#
# Novelty as recency:
ggplot(
  data = a[!is.na(a$meanScore),], # removing NAs
  aes(x = as.factor(novelty_dic_minMeSHlag_0days), y = meanScore)
) + 
  geom_violin(width = 0.5, fill = "gray75", color = "gray75") +
  geom_boxplot(width = 0.5) +
  facet_grid(cols = vars(calltype)) +
  theme_minimal()

summary(lm(
  a$meanScore ~
    a$novelty_dic_minMeSHlag_0days + a$calltype + 
    a$novelty_dic_minMeSHlag_0days : a$calltype # interaction
))


# Novelty as recombinatorial creativity
ggplot(
  data = a,
  aes(x = shibayama_title, y = meanScore)
) + 
  geom_point(size = 0.3) +
  geom_smooth(method = "lm") +
  facet_grid(cols = vars(calltype)) +
  theme_minimal()

summary(lm(
  a$meanScore ~ a$shibayama_title + a$calltype + a$shibayama_title * a$calltype
))



save(
  a, 
  callclass,
  s,
  file = "./counterfactuals/input.RData"
)
