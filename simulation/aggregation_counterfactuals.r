# Runs in R 4.4.1.
#
# Loading, joining and cleaning novelty indicators.


# Cleaning environment and loading libraries and resources.
rm(list = ls())

library("furrr")
library("progressr")
library("compiler")

source("./utils.r") # utility functions

# Loading data
load("./counterfactuals/input.RData")

# Creating a string to be added to the name of output data files. By default,
# this will be the date in which this sript is launched.
batchDate <- format(Sys.Date(), "%Y%m%d")


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Experiment design
#
# ______________________________________________________________________________
# Real world vs cunterfactuals

noveltyIndicators <- c(
  #"Ngrams",
  "novelty_dic_minMeSHlag_0days",
  "novelty_new_MeSH_pairs_dic",
  #"novelty_minMeSHlag",
  "Ngrams_dic_2",
  "shibayama_title",
  "shibayama_abs",
  "shibayama_avg", ########
  "Ngrams_dic_1"
  
)
interventions <- c(
  "regular call", # This must be the first item of this vector.
  "novelty-dedicated call", # This must be the second item of this vector
  "golden tickets",
  "lottery (if ties)",
  "lottery (if fundable)"
)


# For lotteries, we also need to specify what constitutes a "passable" grade
# that qualifies applications as "fundable". In other words, we need to define 
# the entrance threshold for a lottery pool.
poolThresholdGrade <- 3 # on a scale from 1(best grade) to 6 (worst grade).

# For the golden ticket aggregation rule, what is the probability that a 
# reviewer will spend their ticket in the current call?
goldenTicketProb <- 0.5

# Number of repetitions for each call of each counterfactual scenario. 
# The total number of runs per condition is determined by:
# "repetitions" * "nBatches"
repetitions = 5
nBatches = 200

# Setting a random seed for replications:
set.seed(12345)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Preparing a results dataframe where we are gonna write our results.
#
# Crafting a factorial experiment design, where each row denotes a specific call
# that we will simulate under the specified conditions.
# We only simulate regular calls (Funding_type != "grant_hrhg"), and we do
# multiple repetitions per calls and per condition.
treatments <- expand.grid(
  #call = callclass$Callid[callclass$Funding_type != "grant_hrhg"], # reg. calls
  call = callclass$Callid, # reg. calls
  counterfactual = TRUE,
  repetition = 1:repetitions,
  intervention = interventions[3:length(interventions)],
  increasedSubmissions = c(0, 0.2, 1), # 0 means no change; 1 means doubling
  loweredQuality = c(0, 0.5, 1) # average score difference. 0 means no change.
)
# Verifying that we got the right call ids 
#table(
#  unique(a$Callid[!a$synergy]) %in% 
#    callclass$Callid[callclass$Funding_type != "grant_hrhg"]
#)
# That checks out.


# Removing some of the diagonal designs (those where we test robustness to
# multiple changes simultaneously):
treatments <- subset(
  treatments,
  (treatments$increasedSubmissions > 0) + (treatments$loweredQuality > 0) < 2
    #treatments$adjustedSubmNovelty < 2
)


# Let's add a few more lines for the real-world calls, "regular" and "novelty-
# dedicated", that we will not simulate (counterfactual = FALSE).
treatments <- rbind(
  data.frame( # These are the rows we are adding
    call = callclass$Callid, # this includes regular and novelty-dedicated
    counterfactual = FALSE,
    repetition = NA,
    intervention = sapply(
      callclass$Funding_type, 
      FUN = \(x) ifelse(x == "grant_hrhg", interventions[2], interventions[1])
    ),
    increasedSubmissions = 0,
    loweredQuality = 0#,
    #adjustedSubmNovelty = FALSE
  ),
  treatments # We append the counterfactual treatments.
)

# Let's add a variable that helps us distinguish counterfactuals of regular 
# vs novelty-dedicated calls:
treatments$synergy <- FALSE
treatments$synergy[
  treatments$call %in% callclass$Callid[callclass$Funding_type == "grant_hrhg"]
] <- TRUE
table(syntergy = treatments$synergy, counterf. = treatments$counterfactual)

# Adding random seeds to each simulation:
#treatments$seed <- runif(n = nrow(treatments), min = -99999999:99999999)
treatments$seed <- sample(
  x = -99999999:99999999, 
  size = nrow(treatments), 
  replace = FALSE
)

# Adding the variables we need to fill in:
treatments$valid <- TRUE # Identifies edge cases that can't be simulated.
treatments$nApplications <- NA
treatments$nReviewers <- NA
treatments$nReviews <- NA
treatments$nFunded <- 0
treatments$setDiff <- NA
treatments$propSetDiff <- NA
treatments$CohensKappa <- NA
treatments$pFcF <- NA
treatments$pDcD <- NA
treatments$pFcD <- NA
treatments$pDcF <- NA
treatments[, paste0(noveltyIndicators, "_all")] <- NA
treatments[, paste0(noveltyIndicators, "_granted")] <- NA
treatments[, paste0(noveltyIndicators, "_declined")] <- NA
treatments[, paste0(noveltyIndicators, "_setDiff")] <- NA
treatments[, paste0(
  "maleApplicant", c("_all", "_granted", "_declined", "_setDiff"))] <- NA

 
# The following code is to remove treatments that do not need to be simulated
if (FALSE) {
treatments <- subset(
  treatments,
  treatments$increasedSubmissions == 0.2 | treatments$loweredQuality == 0.5
)
}


## End of chunk


batchBlueprint <- treatments
#table(
#  counterf = treatments$counterfactual, 
#  increasedSumbs = treatments$increasedSubmissions,
#  lowQual = treatments$loweredQuality
#)





################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Defining a simulation run.
#
# here is a function that runs the calculations for each row of our treatments
# dataframe:
sim <- function(treatments, a, s, i) { # Foreach call
  #print(i)
  
  # Set run-specific random seed:
  set.seed(treatments$seed[i])
  
  # Select all proposals submitted to that call.
  rr <- subset(a, a$Callid == treatments$call[i])
  
  # Find the funding rate for that call.
  # In case we have calls with no funded applications, we skip.
  #Nfunded <- sum(rr$Applicationstatus == "Granted")
  Nfunded <- sum(rr$Applicationstatus != "Declined") # Granted+in_progress+withd
  if(nrow(rr) < 2 | Nfunded == 0) {
    treatments$valid[i] <- FALSE
    #next
  }
  
  
  ##############################################################################
  # Robustness: increased number of submissions.
  # To do this we sample some other submissions to similar (but different) calls
  # and we pretend they were submitted here by adding them to "rr".
  if (treatments$increasedSubmissions[i] > 0) {
    
    # find how many submissions we should artifically add 
    n = round(nrow(rr) * treatments$increasedSubmissions[i])
    
    # Define a pool of suitable submissions from a similar funding program
    submPool <- subset(
      a, 
      a$callAlias != rr$callAlias[1] & a$Funding_type == rr$Funding_type[1]
    )
    #table(submPool$callAlias != rr$callAlias[1])
    #table(
    #  submPool$Applicationreferencenumeric %in% rr$Applicationreferencenumeric)
    
    # Randomly draw n of them and add them to our rr submissions:
    tempSample <- sample(x = 1:nrow(submPool), size = n, replace = FALSE)
    rr <- rbind(rr, a[tempSample,])
    rm(tempSample)
  }
  ######
  
  
  ##############################################################################
  # Robustness: adjusted submission novelty
  #if (treatments$adjustedSubmNovelty[i]) {
  #  # Let's take a random novelty-dedicated (Synergy) call:
  #  syn <- callclass$Callid[callclass$Funding_type == "grant_hrhg"]
  #  syn <- sample(x = syn, size = 1)
  #  
  #  # Now let's take the novelty profile of proposals submitted to this 
  #  # novelty-dedicated call:
  #  noveltyProfile <- a[a$Callid == syn, noveltyIndicators]
  #}
  ######
  
  
  # Finding the scores given to applications in this call -- which we'll use
  # to see what would have been different if we aggregated them differently
  x <- s[
    s$Applicationsourcerecordid %in% rr$Applicationsourcerecordid, 
    c("Applicationsourcerecordid", "Personsourcerecordid", "Assessmentscore")
  ]
  
  
  ##############################################################################
  # Robustness: worsened quality.
  # This tests what would happen if submissions were rated more poorly by the
  # reviewers. We model this by adjusting scores by an amount specified by
  # "loweredQuality". The scale by NNF has 1 as the best grade, and 6 or 7 as
  # the worst. Thus, to lower quality, we increase the scores.
  if (treatments$loweredQuality[i] > 0 & nrow(x) > 1) {
    noise <- rnorm(
      n = nrow(x), 
      mean = treatments$loweredQuality[i],
      sd = 1
    )
    x$Assessmentscore <- as.numeric(x$Assessmentscore) + noise
    x$Assessmentscore <- round(truncate(
      x$Assessmentscore,
      min = 0,
      max = 7
    ))
  }
  ######
  
  
  
  # Let's get the number of reviewers and of reviews:
  nRev = length(unique(x$Personsourcerecordid))
  if (nRev == 0) nRev <- NA
  if (all(x$Personsourcerecordid == "")) nRev <- NA
  treatments$nReviews[i] <- nrow(x)
  
  
  # Now, we need these scores to run counterfactuals. Therefore, if scores 
  # are missing and the current treatment is a counterfactual, then we mark this
  # treatment as non valid and we skip to the next one.
  if (nrow(x) < 2 & treatments$counterfactual[i]) {
    treatments$valid[i] <- FALSE
    #next
  }
  
  
  # Next, for each application in this call, we calculate what its aggregated
  # score is (for regular + novelty-dedicated calls), or what it would be (for
  # counterfactual calls).
  if (treatments$counterfactual[i] == FALSE) { # For regular / nov.ded. calls:
    xx <- data.frame(
      Applicationsourcerecordid = rr$Applicationsourcerecordid,
      aggregate = as.numeric(rr$fundingOutcome == "Declined"),
      deliberation_c = !rr$fundingOutcome == "Declined"
    )
    xx <- merge(
      x = xx, 
      y = rr[,c(
        "Applicationsourcerecordid",
        "Applicationstatus",
        noveltyIndicators
      )]
    )
    xx$panelchoice <- xx$deliberation_c
  }
  
  ## Counterfactual calls ######################################################
  if (treatments$counterfactual[i] & nrow(x) > 1) { # For counterfactual calls:
      
    # We convert scores into a matrix where each row is a proposal and each 
    # columns a reviewer. Each matrix cell is the score that a particular 
    # reviewer gave to a particular proposal.
    xx <- buildGradeMatrix(
      x = x, 
      rr = rr, 
      noveltyIndicators = noveltyIndicators
    )
    
    # Aggregating by average
    ifelse(
      is.na(nRev),
      scoreIndex <- 2,
      scoreIndex <- 2:(1 + nRev)
    )
    xx$aggregate <- apply(
      X = xx,
      MARGIN = 1,
      FUN = \(x) mean(x[scoreIndex], na.rm = TRUE)
    )
    
    # In case we are running a counterfactual with an artificially increased
    # number of submissions -- which is one of our robustness checks --
    # it is worth noting that we might have added some proposals that were 
    # granted, thus changing the "call budget". To amend this artifact, we can
    # bring back the number of projects funded (nFunded) by recoding as 
    # "unfunded" the funded projects with the worst aggregate score.
    if (treatments$increasedSubmissions[i] > 0) {
      nToUnfund <- sum(rr$Applicationstatus != "Declined") - Nfunded
      
      if (nToUnfund > 0) {
        tempRank <- xx$aggregate
        tempRank[!xx$deliberation_c] <- max(xx$aggregate) 
        tempRank <- rank(tempRank, ties.method = "random")
        toUnfund <- which(tempRank > Nfunded)
        xx$deliberation_c[toUnfund] <- FALSE
      }
    }
    #cbind(tempRank, xx$aggregate, xx$deliberation_c)
    
    
    ############################################################################
    # Golden tickets
    # This implementation of golden tickets is tailored to the use of
    # this rule at Villum Foundation.
    xx$goldentickets <- 0
    if (treatments$intervention[i] == "golden tickets" & treatments$valid[i]) {
      
      # For each reviewer we roll dice to determine if the reviewer will attempt 
      # to use their ticket
      for (reviewer in scoreIndex) if (sum(xx$goldentickets > 0) < Nfunded) {
        if (rbinom(n = 1, size = 1, prob = goldenTicketProb)) {
          
          # Which proposal would rev give the golden ticket to?
          target <- which(xx[,reviewer] == 1 )
          
          if (length(target) == 1) { # Give it to the one with the top grade
            xx$goldentickets[target] <- xx$goldentickets[target] + 1
          }
          if (length(target) > 1) { # and if there are more, choose one randomly
            target <- sample(x = target, size = 1)
            xx$goldentickets[target] <- xx$goldentickets[target] + 1
          }
        }
      }
      
      # Golden-tickets are implemented as a grade that is better than the
      # best available grade (here, 0):
      xx$aggregate[xx$goldentickets > 0] <- 0
      
      # Constructing the final ranking. Ties are broken at random.
      if (is.null(xx$rank)) {
        xx$rank <- rank(xx$aggregate, ties.method = "random")
      }
      
      # Grants are awararded to the top of the ranking.
      xx$panelchoice <- xx$rank <= Nfunded
    }
    
    
    ############################################################################
    # Funding lottery (Type 1)
    #
    if (treatments$intervention[i] == "lottery (if ties)"& treatments$valid[i]) {
      # Constructing the final ranking. Ties are broken at random.
      if (is.null(xx$rank)) {
        xx$rank <- rank(xx$aggregate, ties.method = "random")
      }
      
      # Grants are awarded to the top of the ranking.
      xx$panelchoice <- xx$rank <= Nfunded
    }
    
    
    ############################################################################
    # Funding lottery (Type 3)
    #
    if (treatments$intervention[i] == "lottery (if fundable)"& treatments$valid[i]) {
      pool <- xx$aggregate <= poolThresholdGrade
      
      # Running the lottery:
      if(sum(pool) == 0) {
        treatments$valid[i] <- FALSE
        #next
        return(treatments[i,])
      }
      if(sum(pool) == 1) lotterywinner <- pool
      if(sum(pool) > 1) {
        temp <- sample(
          x = which(pool),
          size = min(
            c(Nfunded, sum(pool))),
          replace = FALSE
        )
        lotterywinner <- rep(FALSE, times = nrow(xx))
        lotterywinner[temp] <- TRUE
      }
      
      # Grants are awarded to lottery winners:
      xx$panelchoice <- lotterywinner
      
      # This code on golden tickets allows to easily implement Type 2 lotteries.
      # We don't need it here for now so we switch it off.
      if (FALSE) {
        goldtickwinner <- xx$goldentickets > 0
        
        # Remove golden-ticket winners from the lottery pool. These applications
        # will be granted funding directly, bypassing the lottery (Type 2).
        pool[goldtickwinner] <- FALSE
        
        # Running the lottery:
        if(sum(pool) - sum(goldtickwinner) <= 0) lotterywinner <- 
          rep(FALSE, length(nrow(xx)))
        if(sum(pool) - sum(goldtickwinner) == 1) lotterywinner <- pool
        if(sum(pool) - sum(goldtickwinner) > 1) {
          temp <- sample(
            x = which(pool),
            size = min(
              c(Nfunded - sum(goldtickwinner), sum(pool) -sum(goldtickwinner))),
            #size = min(c(Nfunded, sum(pool))),
            replace = FALSE
          )
          lotterywinner <- rep(FALSE, times = nrow(xx))
          lotterywinner[temp] <- TRUE
        }
        # Winners are golden-tickets-winners or lottery winners:
        xx$panelchoice <- lotterywinner | goldtickwinner
      }
    }
  }
  
  
  ##############################################################################
  # Calculating aggregate stats and outcome variables
  #
  treatments$nReviewers[i] <- nRev #length(scoreIndex)
  if (exists("xx")) {
    treatments$nApplications[i] <- nrow(xx)
    treatments$nFunded[i] <- Nfunded
  }

  
  
  # Distance between panel choice and our counterfactuals.
  # setDiff identifies the applications which were chosen by a counterfactual
  # panel and rejected by the actual NNF panel.
  if (treatments$counterfactual[i] & treatments$valid[i]) {
    setDiff <- which(xx$panelchoice & !xx$deliberation_c)
    treatments$setDiff[i] <- length(setDiff) # How large is the set Diff?
    treatments$CohensKappa[i] <- 
      irr::kappa2(as.matrix(cbind(xx$deliberation_c, xx$panelchoice)))$value
    # Confusion matrix:
    treatments$pFcF[i] <- sum(xx$deliberation_c & xx$panelchoice)
    treatments$pDcD[i] <- sum(!xx$deliberation_c & !xx$panelchoice)
    treatments$pFcD[i] <- sum(xx$deliberation_c & !xx$panelchoice)
    treatments$pDcF[i] <- sum(!xx$deliberation_c & xx$panelchoice)
  }

  
  # Then the average novelty for all applications, funded and non-funded ones:
  if (treatments$valid[i]) {
    for (variable in noveltyIndicators) {
      treatments[i, paste0(variable, "_all")] <- 
        mean(xx[,variable], na.rm = TRUE)
      treatments[i, paste0(variable, "_granted")] <- 
        mean(xx[xx$panelchoice, variable], na.rm = TRUE)
      treatments[i, paste0(variable, "_declined")] <- 
        mean(xx[!xx$panelchoice, variable], na.rm = TRUE)
      
      if (treatments$counterfactual[i]) {
        treatments[i, paste0(variable, "_setDiff")] <- 
          mean(xx[setDiff, variable], na.rm = TRUE)
      }
    }
    
    # Proportion female main applicants
    treatments$maleApplicant_all[i] <- 
      sum(rr$Mainapplicantgender == "Male") / nrow(xx)
    treatments$maleApplicant_granted[i] <- 
      sum(rr$Mainapplicantgender[xx$panelchoice] == "Male") / Nfunded
    treatments$maleApplicant_declined[i] <- 
      sum(rr$Mainapplicantgender[!xx$panelchoice] == "Male") / (nrow(xx)-Nfunded)
    
    if (treatments$counterfactual[i]) {
      treatments$maleApplicant_setDiff[i] <- 
        sum(rr$Mainapplicantgender[setDiff] == "Male") / length(setDiff)
    }
    #setTxtProgressBar(pb, i)
  }
  
  
  return(treatments[i,])
}
#close(pb)




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Running batteries

# Let's run the battery. If debug is set to TRUE, then runs are executed 
# sequentially, allowing for inspection. If FALSE, they're run in parallel.
debug = FALSE

compiler::enableJIT(1)
if (debug) { # Sequential execution ____________________________________________
  for (i in 1:nrow(treatments)) {
    print(paste("running simulation", i, "of", nrow(treatments)))
    treatments[i,] <- sim(treatments, a, s, i)
  }
} else { # Parallel execution___________________________________________________
  #registerDoParallel(makeCluster(detectCores() - 2))
  
  # We run 100 batches, each of which executes N="iterations" runs per call and
  # per condition.
  for (batch in (1:nBatches)) {
    print(paste("running batch", batch, "of", nBatches))
    
    # Initializing a new batch as a "clean" new copy of "treatments".
    # We assign new random seeds.
    set.seed(batch)
    treatments <- batchBlueprint
    treatments$seed <- sample(
      x = -99999999:99999999, 
      size = nrow(treatments), 
      replace = FALSE
    )
    
    # We only need to run "real" regular and novelty-dedicated calls once. 
    # Therefore, we remove regular and novelty-dedicated from the treatment
    # set of batches 2-100.
    if (batch > 1) treatments <- subset(
      treatments,
      treatments$intervention %in% interventions[3:5]
    )
    
    # Setting up the parallel execution:
    handlers(global = TRUE)
    handlers("txtprogressbar") # Use the text progress bar handler
    
    #plan(multisession, workers = 10)
    plan(multisession)
    #parallel::detectCores()
    
    with_progress({
      progress <- progressor(along = 1:nrow(treatments))
      
      results <- future_map(1:nrow(treatments), \(i) {
        progress()
        sim(treatments, a, s, i)
      })
      
      treatments <- dplyr::bind_rows(results)
    })
    
    # Writing to file
    save(
      treatments, 
      #file = "./counterfactuals/output_20241210_a.RData"
      file = paste0(
        "./counterfactuals/output_", 
        batchDate,
        "_",
        batch,
        ".RData"
      )
    )
  }
}
compiler::enableJIT(0)




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Collating simulation batches into one file. 

# Loading and stacking the 100 data batches from the simulation runs.
# We load them one by one. The resulting complete dataframe will be called "r"
# and written to file.
pb <- txtProgressBar(min = 0, max = 100, style = 3) # setting up a progress bar
for (batch in 1:100) {
  setTxtProgressBar(pb, batch) # updating the progress bar 
  
  # Loading batch number "batch" from the "batchDate" set of runs
  load(paste0("./counterfactuals/output_", batchDate, "_", batch, ".RData"))
  
  # Stiching it to the previously loaded batches:
  ifelse (
    batch == 1,
    r <- treatments,
    r <- rbind(r, treatments)
  )
  
  rm(treatments)
}


save(
  r, 
  noveltyIndicators,
  interventions,
  file = paste0("./counterfactuals/output_", batchDate,"_complete.RData")
)








################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Exporting to other formats.


if (FALSE) {
  #write.csv2(a, file = "./counterfactuals/applications.csv")
  #write.csv2(treatments, file = "./counterfactuals/callSimulations.csv")
  #test <- read.csv2(file = "./counterfactuals/applications.csv")
  # Exporting to csv____________________________________________________________
  a <- subset(
    a, 
    select = c(
      "Applicationsourcerecordid",
      "Callid",
      "Grantedamount",
      "Appliedamount",
      "Projectgrantedamount",
      "Callsourcerecordid" ,
      "Callname",
      "Mainapplicantage",
      "Mainapplicantgender",
      "ApplicationYear",
      "malePI",
      "femalePI" ,
      "youngPI" ,
      "fundingOutcome",
      "granted" ,
      "Funding_type",
      "calltype" ,
      "competition",
      "NapplicationsInCall" ,
      "NgrantedInCall" ,
      "avgAppliedAmountInCall"  ,
      "avgAppliedAmountInCalltypeYear" ,
      "Nreviewers",
      "meanScore",
      "medianScore",
      "Appliedamount_corrected",
      "Appliedamount_corrected_log",
      "Appliedamount_corrected_log_sqr",
      "synergy",
      "regular",
      noveltyIndicators
    )
  )
  write.csv(a, file = "./counterfactuals/applications.csv")
  write.csv(treatments, file = "./counterfactuals/callSimulations.csv")
  
  # And for Stata_______________________________________________________________
  a <- subset(
    a, select = -c(
      Timedimensiondate, 
      Timedimensiondateexplanation, 
      Applicationdate, 
      Projectstartdate, 
      Projectenddate, 
      Granteddate
    )
  )
  a[a == ""] <- NA
  
  foreign::write.dta(a, "./counterfactuals/applications.dta")
  foreign::write.dta(treatments, "./counterfactuals/callSimulations.dta")
}



