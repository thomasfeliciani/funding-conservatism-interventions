# This script generates dummy datasets that allow inspecting and demonstrating
# the counterfactual simulation experiments.
#
# The data used in this research cannot be shared. Therefore we prepared this
# script that generates synthetic data with the same structure as the real data.
# This allows to run the scripts in this repository experiments on the dummy
# data.
#
#
# Runs in R 4.4.1.



# This dataset contains proposal-level data. Each row of "a" represents a
# unique grant proposal submitted to the funder.
a <- data.frame(
  Applicationid = paste("Application ID: ", 1:10000),
  Applicationsourcerecordid = 1:10000,
  Applicationreferencenumeric = 10000:1,
  Applicationstatus = sample(
    x = c("Declined", "Granted", "In progress", "Withdrawn"),
    size = 10000,
    replace = TRUE,
    prob = c(15, 5, 1, 0.1)
  ),
  fundingOutcome = NA,
  Applicationdate = sample(
    x = seq.Date(as.Date("2011-01-01"), as.Date("2022-04-15"), by = "day"),
    size = 10000,
    replace = TRUE
  ),
  Grantedamount = 0,
  Appliedamount = sample (10**7, size = 10000),
  Callid = paste("Call ID:", sample(1:300, size = 10000, replace = TRUE)),
  Callname = NA,
  Callsourcerecordid = NA,
  Mainapplicantid = sample(1:200, size = 10000, replace = TRUE),
  Mainapplicantsourcerecordid = NA,
  Mainapplicantage = sample(x = c(NA, 23:80), size = 10000,replace = TRUE),
  Mainapplicantagegroup = NA,
  Mainapplicantgender = sample(
    x = c("Male", "Female", "Other", "Unknown", ""),
    size = 10000,
    replace = TRUE,
    prob = c(1.3, 1, 0.05, 0.05, 0.05)
  ),
  
  # Adding synthetic (dummy) measures of new vintage and creative recombination:
  novelty_dic_minMeSHlag_0days = sample(
    x = 0:1,
    size = 10000,
    replace = TRUE,
    prob = c(4, 1)
  ),
  novelty_new_MeSH_pairs_dic = sample(
    x = c(FALSE, TRUE),
    size = 10000,
    replace = TRUE,
    prob = c(5, 3)
  ),
  Ngrams_dic_1 = sample(
    x = c(FALSE, TRUE),
    size = 10000,
    replace = TRUE
  ),
  Ngrams_dic_2 = sample(
    x = c(FALSE, TRUE),
    size = 10000,
    replace = TRUE,
    prob = c(2, 1)
  ),
  shibayama_title = truncate(rnorm(n = 10000, mean = 0.5, sd = 0.1)),
  shibayama_abs = truncate(rnorm(n = 10000, mean = 0.2, sd = 0.1)),
  shibayama_avg = NA
)


# Fixing the variables that depend on other ones:

a$fundingOutcome <- "Granted, in progress or withdrawn"
a$fundingOutcome[a$Applicationstatus == "Declined"] <- "Declined"

a$Grantedamount[a$Applicationstatus != "Declined"] <- 
  a$Appliedamount[a$Applicationstatus != "Declined"]

a$Callsourcerecordid <- a$Callid
a$Callname <- paste("Call name", a$Callid)
a$Mainapplicantsourcerecordid = a$Mainapplicantid

a$Mainapplicantagegroup <- as.character(round(a$Mainapplicantage / 10) * 10)
a$Mainapplicantage <- as.character(a$Mainapplicantage)

a$shibayama_avg <- apply(
  X = cbind(a$shibayama_title, a$shibayama_abs), 
  MARGIN = 1, 
  FUN = mean
)


# This is the dataset with review scores. Each row is a review score given
# by a reviewer to a proposal.
s <- data.frame(
  Applicationid = NA,
  Applicationsourcerecordid = sample(
    x = 1:10000,
    size = 50000,
    replace = TRUE
  ),
  Applicationreference = NA,
  Applicationreferencenumeric = NA,
  Personsourcerecordid = 201:600,
  Personid = NA,
  Assessmentscore = as.character(sample(
    x = c(1:6),
    prob = c(0.8, 1.5, 2.5, 1, 1, 0.1),
    size = 50000,
    replace = TRUE
  ))
)

# Filling in variables that depend on others:
s$Applicationid <- paste("Application ID:", s$Applicationsourcerecordid)
s$Applicationreference <- s$Applicationsourcerecordid
s$Applicationreferencenumeric <- s$Applicationsourcerecordid
s$Personid <- paste("Person ID:", s$Personsourcerecordid)


# Finally, call-level data. Each row is a funding call.
callclass <- data.frame(
  Applicationyear_new = sample(x = 2011:2022, size = 300, replace = TRUE),
  Callid = paste("Call ID:", 1:300),
  Funding_type = sample(
    x = c("grant_hrhg", "grant", "grant_challenge", "grant_investigator"),
    prob = c(0.5, 1, 1, 1),
    size = 300,
    replace = TRUE
  )
)
callclass$callAlias <- callclass$Callid


# Adding variables "Funding_type" and "callAlias" to the dataset a.
a <- merge(
  x = a,
  y = callclass,
  by = "Callid",
)
