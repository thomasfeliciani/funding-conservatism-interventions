# Runs in R 4.4.1.
#
# This script runs the plots to compare real and simulated inverventions.


# Cleaning environment and loading libraries.
rm(list = ls())

library("ggplot2")
library("ggpattern") # for pattern-filling of bar plots
library("gridExtra")
#source("./utils.r")

# Loading results dataframe. The variable "batchDate" identifies the desired
# data batches.
#batchDate = "20241219" # format(Sys.Date(), "%Y%m%d")
#batchDate = "20250121" # format(Sys.Date(), "%Y%m%d")
batchDate = "20250123" # format(Sys.Date(), "%Y%m%d")

load(paste0("./counterfactuals/output_", batchDate,"_complete.RData"))

noveltyIndicators <- c(
  "novelty_dic_minMeSHlag_0days",
  "novelty_new_MeSH_pairs_dic",
  "Ngrams_dic_2",
  "shibayama_title",
  "shibayama_abs",
  "shibayama_avg", ########
  "Ngrams_dic_1"
  
)
interventions <- c(
  "regular (baseline)", # This must be the first item of this vector.
  "novelty-dedicated", # This must be the second item of this vector
  "golden tickets",
  "lottery (if ties)",
  "lottery (if fundable)"
)
interventionsLabels <- c(
  "Regular (baseline)", # This must be the first item of this vector.
  "Novelty-Dedicated", # This must be the second item of this vector
  "* Golden Tickets",
  "* Tie-Breaking Lotteries",
  "* Fundable-Pool Lotteries"
)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Inspecting and cleaning results


# Checking random seeds ________________________________________________________
# Memory check: weight of the results dataframe
print(format(object.size(r), units = "auto"))

# Checking that runs have unique random seeds.
length(unique(r$seed)) == nrow(r) # Some seeds are repeated
#sum(table(r$seed) > 1) # Counting duplicate seeds.

# Some runs were given the same random seed. That means I need to ensure that,
# at least, they were run for different conditions.
dupl <- unique(r$seed[duplicated(r$seed)])
tocheck <- subset(r, r$seed %in% dupl)
#
# Looping through all duplicated seeds to ensure the runs that share the same
# seed are in fact different. I takes quite a bit of time, so I ran it once and
# then I wrapped it into a "if (FALSE)" statement to effectively switch if off.
# This allows this script to be executed fast and efficienlty, but it requires
# me to manually run it every time I produce new simulation batches.
if (FALSE) { 
  
  library("furrr")
  library("progressr")
  
  handlers("txtprogressbar")
  plan(multisession)
  
  with_progress({
    progress <- progressor(along = 1:length(dupl))
    
    problems <- future_map_lgl(1:length(dupl), \(i) {
      progress()
      x <- subset(r, r$seed == dupl[i])[,c(
        "call", 
        "counterfactual",
        "intervention", 
        "increasedSubmissions",
        "loweredQuality"
      )]
      return(any(duplicated(x)))
    })
  })
  
  any(problems) # Are there any problems?
  
}

rm(dupl, tocheck) # Clearing objects that are not needed any longer.



# Clearning variables___________________________________________________________
#
# Renaming the interventions
r$intervention[r$intervention == "regular call"] <- "regular (baseline)"
r$intervention[r$intervention=="novelty-dedicated call"] <- "novelty-dedicated"

r$intervention <- factor(
  x = r$intervention,
  levels = interventions,
  #labels = interventions
  labels = interventionsLabels
)



# Calculating the proportion of granted applications that make up the diff_set
which(r$setDiff > r$nFunded)
r$propSetDiff <- r$setDiff / r$nFunded
summary(r$propSetDiff)

# Real calls have an empty difference set (obv). Therefore, their outcome
# measures (noveltyIndicators) calculated on the difference set, naturally
# missing, ought to be replaced with the outcome measure of their granted set.
for (variable in noveltyIndicators) {
  r[!r$counterfactual, paste0(variable, "_setDiff")] <- 
    r[!r$counterfactual, paste0(variable, "_granted")] 
}

# Checking how many calls we could not run counterfactuals for -- because of
# insufficient applications (N < 2), insufficient funding rate (rate = 0) or
# insufficient information on reviewer grades (N < 2).
table(r$valid)

# And how many invalid entries we have among counterfactuals and real calls
table(valid = r$valid, counterfactual = r$counterfactual)

# And how many counterfactuals we have for each type of call:
table(valid = r$valid, novelty_dedicated = r$synergy)

# How many calls we have:
table(
  valid = r$valid[!r$counterfactual], 
  novelty_dedicated = r$synergy[!r$counterfactual]
)
#(treatments$calltype[!treatments$counterfactual])

# And how many applications per call:
summary(r$nApplications[!r$counterfactual & r$valid])


# Checking that we have exactly the same number of counterfactual calls for each
# "valid" call in our set.
r$call[r$counterfactual] |> table() |> table()


# Removing non-valid counterfactual simulations.
# In other words, we keep rows of "treatments" that either correspond to real
# calls ("!counterfactual"), or they are counterfactual but marked as "valid"
r <- subset(
  r, 
  r$valid | !r$counterfactual
)

rBackup <- r

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Figure 2 - comparing interventions


################################################################################
# Starting with counterfactuals of regular calls

# Removing runs from robustness checks
r <- rBackup
r <- subset(
  r,
  r$increasedSubmissions == 0 &    # higher submission rate
    r$loweredQuality == 0 &        # lower quality submissions
    (!r$counterfactual | (!r$synergy & r$counterfactual))# adjusted incentives
    #(!r$counterfactual | (r$synergy & r$counterfactual))# 
)

dff <- expand.grid(
  intervention = interventionsLabels, 
  variable = c("vintage", "recombination")
)

dff$upper <- dff$lower <- dff$se_diff <- dff$sd_diff <- 
  dff$mean_diff <- dff$n <- NA

for (i in 1:nrow(dff)) {
  
  # Selecting the runs we need _________________________________________________
  if (dff$intervention[i] == "Regular (baseline)") {
    rr <- subset(r, !r$counterfactual & !r$synergy)
  }
  if (dff$intervention[i] == "Novelty-Dedicated") {
    rr <- subset(r, !r$counterfactual & r$synergy)
  }
  if ( # If counterfactual:
    dff$intervention[i] != "Regular (baseline)" & 
    dff$intervention[i] != "Novelty-Dedicated"
  ) {
    rr <- subset(
      r, 
      r$counterfactual & !r$synergy & r$intervention == dff$intervention[i])
      #r$counterfactual & r$synergy & r$intervention == dff$intervention[i])
  }
  
  # Selecting correct variable _________________________________________________
  if (dff$variable[i] == "vintage") {
    differences <- rr$novelty_dic_minMeSHlag_0days_granted -
      rr$novelty_dic_minMeSHlag_0days_declined
    
    dff$mean_nov_funded[i] <- mean(
      rr$novelty_dic_minMeSHlag_0days_granted,
      na.rm = TRUE
    )
  }
  if (dff$variable[i] == "recombination") {
    differences <- rr$shibayama_title_granted -
      rr$shibayama_title_declined
    
    dff$mean_nov_funded[i] <- mean(
      rr$shibayama_title_granted,
      na.rm = TRUE
    )
  }
  
  # Calculating what we need ___________________________________________________
  dff$n[i] <- length(differences[!is.na(differences)])
  dff$mean_diff[i] <- mean(differences, na.rm = TRUE)
  dff$sd_diff[i] <- sd(differences, na.rm = TRUE)
  dff$se_diff[i] <- dff$sd_diff[i] / sqrt(dff$n[i])
  
  # 95% CI:
  t_value <- qt(0.975, df = dff$n[i] - 1)
  
  # Calculate CI
  dff$lower[i] <- dff$mean_diff[i] - t_value * dff$se_diff[i]
  dff$upper[i] <- dff$mean_diff[i] + t_value * dff$se_diff[i]
  
  # Adding a variable to distinguish between real and counterfactual
  # interventions.
  dff$counterfactual <- "Actual funding decisions"
  dff$counterfactual[
    dff$intervention %in% interventionsLabels[3:length(interventionsLabels)]
  ] <- "Counterfactual funding decisions\n(simulation of Regular panels)"
}

# And reversing factor levels for plotting things in the right order:
dff$intervention <- forcats::fct_rev(dff$intervention)

dff$variable <- factor(
  x = dff$variable,
  levels = c("vintage", "recombination"),
  labels = c("New Vintage", "Creative Recombination")
)



Figure_2 <-  ggplot(
  data = dff, 
  aes(x = mean_diff, y = intervention, xmin = lower, xmax = upper)
) +
  geom_rect(
    aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), 
    fill = "black", 
    alpha = 0.025
  ) +
  geom_col(fill = "gray55", width = 0.5) +
  geom_errorbar(width = 0.1) +
  labs(
    title = NULL,
    subtitle = NULL,
    pattern = "",
    x = NULL, 
    y = NULL,
    caption = paste0(
      "* From counterfactual funding decisions based\n",
      "   on simulations of Regular panels"
    )
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  facet_grid(
    ~variable,
    scales = "free"
  ) +
  geom_text(
    aes(x = mean_diff, y = intervention, label = round(mean_diff, digits = 3)),
    hjust = 0, # Adjust position horizontally
    vjust = -2,    # Adjust position vertically
    color = "black", # Text color
    size = 2      # Text size
  ) +
  scale_x_continuous(
    expand = c(0, 0.005),
    breaks = c(0),
    labels = c("Δ(Funded - Declined) = 0")
  ) +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray93", linewidth = 0.5),
    panel.background = element_rect(color = NA, fill = "gray97"),
    strip.background = element_blank(),
    strip.text = element_text(angle = 0, hjust = 0.2),
    axis.text.x = element_text(angle = 10, hjust = 1),
    #legend.position = "bottom"
    plot.caption = element_text(hjust = 0, vjust = 0, face = "italic") 
  )
print(Figure_2)

ggsave(
  #"./outputGraphics/Figure_2_synergyCounterfactuals.png",
  "./outputGraphics/Figure_2.png",
  plot = Figure_2,
  width = 2000,
  height = 960,
  units = "px",
  bg = "white",
  dpi = 300
)






################################################################################
# Robustness: adjusted applicant incentives
# Here we look at counterfactuals of novelty-dedicated calls

# Removing runs from robustness checks
r <- rBackup
r <- subset(
  r,
  r$increasedSubmissions == 0 &    # higher submission rate
    r$loweredQuality == 0 &        # lower quality submissions
    #(!r$counterfactual | (!r$synergy & r$counterfactual))# adjusted incentives
    (!r$counterfactual | (r$synergy & r$counterfactual))
)

dff <- expand.grid(
  intervention = interventionsLabels, 
  variable = c("vintage", "recombination")
)

dff$upper <- dff$lower <- dff$se_diff <- dff$sd_diff <- 
  dff$mean_diff <- dff$n <- NA

for (i in 1:nrow(dff)) {
  
  # Selecting the runs we need _________________________________________________
  if (dff$intervention[i] == "Regular (baseline)") {
    rr <- subset(r, !r$counterfactual & !r$synergy)
  }
  if (dff$intervention[i] == "Novelty-Dedicated") {
    rr <- subset(r, !r$counterfactual & r$synergy)
  }
  if ( # If counterfactual:
    dff$intervention[i] != "Regular (baseline)" & 
    dff$intervention[i] != "Novelty-Dedicated"
  ) {
    rr <- subset(
      r, 
      #r$counterfactual & !r$synergy & r$intervention == dff$intervention[i])
      r$counterfactual & r$synergy & r$intervention == dff$intervention[i])
  }
  
  # Selecting correct variable _________________________________________________
  if (dff$variable[i] == "vintage") {
    differences <- rr$novelty_dic_minMeSHlag_0days_granted -
      rr$novelty_dic_minMeSHlag_0days_declined
    
    dff$mean_nov_funded[i] <- mean(
      rr$novelty_dic_minMeSHlag_0days_granted,
      na.rm = TRUE
    )
  }
  if (dff$variable[i] == "recombination") {
    differences <- rr$shibayama_title_granted -
      rr$shibayama_title_declined
    
    dff$mean_nov_funded[i] <- mean(
      rr$shibayama_title_granted,
      na.rm = TRUE
    )
  }
  
  # Calculating what we need ___________________________________________________
  dff$n[i] <- length(differences[!is.na(differences)])
  dff$mean_diff[i] <- mean(differences, na.rm = TRUE)
  dff$sd_diff[i] <- sd(differences, na.rm = TRUE)
  dff$se_diff[i] <- dff$sd_diff[i] / sqrt(dff$n[i])
  
  # 95% CI:
  t_value <- qt(0.975, df = dff$n[i] - 1)
  
  # Calculate CI
  dff$lower[i] <- dff$mean_diff[i] - t_value * dff$se_diff[i]
  dff$upper[i] <- dff$mean_diff[i] + t_value * dff$se_diff[i]
  
  # Adding a variable to distinguish between real and counterfactual
  # interventions.
  dff$counterfactual <- "Actual funding decisions"
  dff$counterfactual[
    dff$intervention %in% interventionsLabels[3:length(interventionsLabels)]
  ] <- "Counterfactual funding decisions\n(simulation of Novelty-Dedicated panels)"
}

# And reversing factor levels for plotting things in the right order:
dff$intervention <- forcats::fct_rev(dff$intervention)

dff$variable <- factor(
  x = dff$variable,
  levels = c("vintage", "recombination"),
  labels = c("New Vintage", "Creative Recombination")
)


Figure_3 <-  ggplot(
  data = dff, 
  aes(x = mean_diff, y = intervention, xmin = lower, xmax = upper)
) +
  geom_rect(
    aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), 
    fill = "black", 
    alpha = 0.025
  ) +
  geom_col(fill = "gray55", width = 0.5) +
  geom_errorbar(width = 0.1) +
  labs(
    title = NULL,
    subtitle = NULL,
    pattern = "",
    x = NULL, 
    y = NULL,
    caption = paste0(
      "* From counterfactual funding decisions based\n",
      "   on simulations of Novelty-Dedicated panels"
    )
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  facet_grid(
    ~variable,
    scales = "free"
  ) +
  geom_text(
    aes(x = mean_diff, y = intervention, label = round(mean_diff, digits = 3)),
    hjust = 0, # Adjust position horizontally
    vjust = -2,    # Adjust position vertically
    color = "black", # Text color
    size = 2      # Text size
  ) +
  scale_x_continuous(
    expand = c(0, 0.005),
    breaks = c(0),
    labels = c("Δ(Funded - Declined) = 0")
  ) +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray93", linewidth = 0.5),
    panel.background = element_rect(color = NA, fill = "gray97"),
    strip.background = element_blank(),
    strip.text = element_text(angle = 0, hjust = 0.2),
    axis.text.x = element_text(angle = 10, hjust = 1),
    #legend.position = "bottom"
    plot.caption = element_text(hjust = 0, vjust = 0, face = "italic") 
  )
print(Figure_3)


ggsave(
  #"./outputGraphics/Figure_2_synergyCounterfactuals.png",
  "./outputGraphics/Figure_3.png",
  plot = Figure_3,
  width = 2000,
  height = 960,
  units = "px",
  bg = "white",
  dpi = 300
)



# Checking if these results make sense.
# Let's take all 13 (real) novelty-dedicated calls.
#t <- r[!r$counterfactual & !r$synergy,]
#mean(t$novelty_dic_minMeSHlag_0days_declined, na.rm = TRUE)
#mean(t$novelty_dic_minMeSHlag_0days_granted, na.rm = TRUE)
#mean(t$novelty_dic_minMeSHlag_0days_granted, na.rm = TRUE) - mean(t$novelty_dic_minMeSHlag_0days_declined, na.rm = TRUE)
#
#mean(t$shibayama_title_declined, na.rm = TRUE)
#mean(t$shibayama_title_granted, na.rm = TRUE)
#mean(t$shibayama_title_granted, na.rm = TRUE) - mean(t$shibayama_title_declined, na.rm = TRUE)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Another angle to look at these results.
if (FALSE) {
r2 <- subset(
    r, 
    !(r$counterfactual & r$synergy)
    #!(treatments$counterfactual & !treatments$synergy)
  )
r2 <- reshape2::melt(
  data = r2,
  measure.vars = c(
    "novelty_dic_minMeSHlag_0days_granted", 
    "shibayama_title_granted"
  )
)
r2$intervention <- factor(
  x = r2$intervention,
  levels = rev(interventions),
  labels = rev(interventions),
)

Figure_2_old <- ggplot(
  data = r2,
  aes(x = value, y = intervention)
) +
  geom_violin(color = "gray80", fill = "gray80") +
  geom_boxplot(width = 0.3, alpha = 0.5) + 
  facet_grid(
    ~variable,
    labeller = labeller(variable = as_labeller(c(
      "novelty_dic_minMeSHlag_0days_granted" = "vintage",
      "shibayama_title_granted" = "recombination"
    )))
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal()
print(Figure_2_old)

if (FALSE) ggsave(
  #"./outputGraphics/Figure_2_old_synergyCounterfactuals.png",
  "./outputGraphics/Figure_2_old.png",
  plot = Figure_2_old,
  width = 2000,
  height = 950,
  units = "px",
  bg = "white",
  dpi = 300
)

}


################################################################################
# Robustness: counterfactual regular panels with increased submissions
#
r <- rBackup
r <- subset(
  r,
  #(!r$counterfactual | r$increasedSubmissions == 1) &  #########################
  (!r$counterfactual | r$increasedSubmissions == 0.2) &  #########################
    r$loweredQuality == 0 &
    (!r$counterfactual | (!r$synergy & r$counterfactual))
)

dff <- expand.grid(
  intervention = interventionsLabels, 
  variable = c("vintage", "recombination")
)

dff$upper <- dff$lower <- dff$se_diff <- dff$sd_diff <- 
  dff$mean_diff <- dff$n <- NA

for (i in 1:nrow(dff)) {
  
  # Selecting the runs we need _________________________________________________
  if (dff$intervention[i] == "Regular (baseline)") {
    rr <- subset(r, !r$counterfactual & !r$synergy)
  }
  if (dff$intervention[i] == "Novelty-Dedicated") {
    rr <- subset(r, !r$counterfactual & r$synergy)
  }
  if ( # If counterfactual:
    dff$intervention[i] != "Regular (baseline)" & 
    dff$intervention[i] != "Novelty-Dedicated"
  ) {
    rr <- subset(
      r, 
      r$counterfactual & !r$synergy & r$intervention == dff$intervention[i])
    #r$counterfactual & r$synergy & r$intervention == dff$intervention[i])
  }
  
  # Selecting correct variable _________________________________________________
  if (dff$variable[i] == "vintage") {
    differences <- rr$novelty_dic_minMeSHlag_0days_granted -
      rr$novelty_dic_minMeSHlag_0days_declined
    
    dff$mean_nov_funded[i] <- mean(
      rr$novelty_dic_minMeSHlag_0days_granted,
      na.rm = TRUE
    )
  }
  if (dff$variable[i] == "recombination") {
    differences <- rr$shibayama_title_granted -
      rr$shibayama_title_declined
    
    dff$mean_nov_funded[i] <- mean(
      rr$shibayama_title_granted,
      na.rm = TRUE
    )
  }
  
  # Calculating what we need ___________________________________________________
  dff$n[i] <- length(differences[!is.na(differences)])
  dff$mean_diff[i] <- mean(differences, na.rm = TRUE)
  dff$sd_diff[i] <- sd(differences, na.rm = TRUE)
  dff$se_diff[i] <- dff$sd_diff[i] / sqrt(dff$n[i])
  
  # 95% CI:
  t_value <- qt(0.975, df = dff$n[i] - 1)
  
  # Calculate CI
  dff$lower[i] <- dff$mean_diff[i] - t_value * dff$se_diff[i]
  dff$upper[i] <- dff$mean_diff[i] + t_value * dff$se_diff[i]
  
  # Adding a variable to distinguish between real and counterfactual
  # interventions.
  dff$counterfactual <- "Actual funding decisions"
  dff$counterfactual[
    dff$intervention %in% interventionsLabels[3:length(interventionsLabels)]
  ] <- paste0(
    "Counterfactual funding decisions\n",
    "with doubled submission rate\n",
    "(simulation of Regular panels)"
  )
}

# And reversing factor levels for plotting things in the right order:
dff$intervention <- forcats::fct_rev(dff$intervention)

dff$variable <- factor(
  x = dff$variable,
  levels = c("vintage", "recombination"),
  labels = c("New Vintage", "Creative Recombination")
)

Figure_2_reg_increasedSubmissions <-  ggplot(
  data = dff, 
  aes(x = mean_diff, y = intervention, xmin = lower, xmax = upper)
) +
  geom_rect(
    aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), 
    fill = "black", 
    alpha = 0.025
  ) +
  geom_col(fill = "gray55", width = 0.5) +
  geom_errorbar(width = 0.1) +
  labs(
    title = NULL,
    subtitle = NULL,
    pattern = "",
    x = NULL, 
    y = NULL,
    caption = paste0(
      "* From counterfactual funding decisions based\n",
      "   on simulations of Regular panels\n",
      "   assuming a 20% higher submission rate"
    )
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  facet_grid(
    ~variable,
    scales = "free"
  ) +
  geom_text(
    aes(x = mean_diff, y = intervention, label = round(mean_diff, digits = 3)),
    hjust = 0, # Adjust position horizontally
    vjust = -2,    # Adjust position vertically
    color = "black", # Text color
    size = 2      # Text size
  ) +
  scale_x_continuous(
    expand = c(0, 0.005),
    breaks = c(0),
    labels = c("Δ(Funded - Declined) = 0")
  ) +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray93", linewidth = 0.5),
    panel.background = element_rect(color = NA, fill = "gray97"),
    strip.background = element_blank(),
    strip.text = element_text(angle = 0, hjust = 0.2),
    axis.text.x = element_text(angle = 10, hjust = 1),
    #legend.position = "bottom"
    plot.caption = element_text(hjust = 0, vjust = 0, face = "italic") 
  )
print(Figure_2_reg_increasedSubmissions)

ggsave(
  #"./outputGraphics/Figure_2_synergyCounterfactuals.png",
  "./outputGraphics/Figure_2_reg_increasedSubmissions.png",
  plot = Figure_2_reg_increasedSubmissions,
  width = 2000,
  height = 950,
  units = "px",
  bg = "white",
  dpi = 300
)



################################################################################
# Robustness: counterfactual novelty-dedicated panels with increased submissions.
#
r <- rBackup
r <- subset(
  r,
  #(!r$counterfactual | r$increasedSubmissions == 1) &  #######################
  (!r$counterfactual | r$increasedSubmissions == 0.2) &  ######################
  #r$increasedSubmissions != 0 &
    r$loweredQuality == 0 &
    (!r$counterfactual | (r$synergy & r$counterfactual))
)

dff <- expand.grid(
  intervention = interventionsLabels, 
  variable = c("vintage", "recombination")
)

dff$upper <- dff$lower <- dff$se_diff <- dff$sd_diff <- 
  dff$mean_diff <- dff$n <- NA

for (i in 1:nrow(dff)) {
  
  # Selecting the runs we need _________________________________________________
  if (dff$intervention[i] == "Regular (baseline)") {
    rr <- subset(r, !r$counterfactual & !r$synergy)
  }
  if (dff$intervention[i] == "Novelty-Dedicated") {
    rr <- subset(r, !r$counterfactual & r$synergy)
  }
  if ( # If counterfactual:
    dff$intervention[i] != "Regular (baseline)" & 
    dff$intervention[i] != "Novelty-Dedicated"
  ) {
    rr <- subset(
      r, 
      r$counterfactual & r$synergy & r$intervention == dff$intervention[i])
    #r$counterfactual & r$synergy & r$intervention == dff$intervention[i])
  }
  
  # Selecting correct variable _________________________________________________
  if (dff$variable[i] == "vintage") {
    differences <- rr$novelty_dic_minMeSHlag_0days_granted -
      rr$novelty_dic_minMeSHlag_0days_declined
    
    dff$mean_nov_funded[i] <- mean(
      rr$novelty_dic_minMeSHlag_0days_granted,
      na.rm = TRUE
    )
  }
  if (dff$variable[i] == "recombination") {
    differences <- rr$shibayama_title_granted -
      rr$shibayama_title_declined
    
    dff$mean_nov_funded[i] <- mean(
      rr$shibayama_title_granted,
      na.rm = TRUE
    )
  }
  
  # Calculating what we need ___________________________________________________
  dff$n[i] <- length(differences[!is.na(differences)])
  dff$mean_diff[i] <- mean(differences, na.rm = TRUE)
  dff$sd_diff[i] <- sd(differences, na.rm = TRUE)
  dff$se_diff[i] <- dff$sd_diff[i] / sqrt(dff$n[i])
  
  # 95% CI:
  t_value <- qt(0.975, df = dff$n[i] - 1)
  
  # Calculate CI
  dff$lower[i] <- dff$mean_diff[i] - t_value * dff$se_diff[i]
  dff$upper[i] <- dff$mean_diff[i] + t_value * dff$se_diff[i]
  
  # Adding a variable to distinguish between real and counterfactual
  # interventions.
  dff$counterfactual <- "Actual funding decisions"
  dff$counterfactual[
    dff$intervention %in% interventionsLabels[3:length(interventionsLabels)]
  ] <- paste0(
    "Counterfactual funding decisions\n",
    "with doubled submission rate\n",
    "(simulation of Novelty-Dedicated panels)"
  )
}

# And reversing factor levels for plotting things in the right order:
dff$intervention <- forcats::fct_rev(dff$intervention)

dff$variable <- factor(
  x = dff$variable,
  levels = c("vintage", "recombination"),
  labels = c("New Vintage", "Creative Recombination")
)

Figure_3_novded_increasedSubmissions <-  ggplot(
  data = dff, 
  aes(x = mean_diff, y = intervention, xmin = lower, xmax = upper)
) +
  geom_rect(
    aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), 
    fill = "black", 
    alpha = 0.025
  ) +
  geom_col(fill = "gray55", width = 0.5) +
  geom_errorbar(width = 0.1) +
  labs(
    title = NULL,
    subtitle = NULL,
    pattern = "",
    x = NULL, 
    y = NULL,
    caption = paste0(
      "* From counterfactual funding decisions based\n",
      "   on simulations of Novelty-Dedicated panels\n",
      "   assuming a 20% higher submission rate"
    )
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  facet_grid(
    ~variable,
    scales = "free"
  ) +
  geom_text(
    aes(x = mean_diff, y = intervention, label = round(mean_diff, digits = 3)),
    hjust = 0, # Adjust position horizontally
    vjust = -2,    # Adjust position vertically
    color = "black", # Text color
    size = 2      # Text size
  ) +
  scale_x_continuous(
    expand = c(0, 0.005),
    breaks = c(0),
    labels = c("Δ(Funded - Declined) = 0")
  ) +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray93", linewidth = 0.5),
    panel.background = element_rect(color = NA, fill = "gray97"),
    strip.background = element_blank(),
    strip.text = element_text(angle = 0, hjust = 0.2),
    axis.text.x = element_text(angle = 10, hjust = 1),
    #legend.position = "bottom"
    plot.caption = element_text(hjust = 0, vjust = 0, face = "italic") 
  )
print(Figure_3_novded_increasedSubmissions)

ggsave(
  #"./outputGraphics/Figure_2_synergyCounterfactuals.png",
  "./outputGraphics/Figure_3_novded_increasedSubmissions.png",
  plot = Figure_3_novded_increasedSubmissions,
  width = 2000,
  height = 950,
  units = "px",
  bg = "white",
  dpi = 300
)




################################################################################
# Robustness: counterfactual regular panels with lowered quality
#
r <- rBackup
r <- subset(
  r,
  r$increasedSubmissions == 0 &
    #(!r$counterfactual | r$loweredQuality == 1) &
    (!r$counterfactual | r$loweredQuality == 0.5) &
    (!r$counterfactual | (!r$synergy & r$counterfactual))
)

dff <- expand.grid(
  intervention = interventionsLabels, 
  variable = c("vintage", "recombination")
)

dff$upper <- dff$lower <- dff$se_diff <- dff$sd_diff <- 
  dff$mean_diff <- dff$n <- NA

for (i in 1:nrow(dff)) {
  
  # Selecting the runs we need _________________________________________________
  if (dff$intervention[i] == "Regular (baseline)") {
    rr <- subset(r, !r$counterfactual & !r$synergy)
  }
  if (dff$intervention[i] == "Novelty-Dedicated") {
    rr <- subset(r, !r$counterfactual & r$synergy)
  }
  if ( # If counterfactual:
    dff$intervention[i] != "Regular (baseline)" & 
    dff$intervention[i] != "Novelty-Dedicated"
  ) {
    rr <- subset(
      r, 
      r$counterfactual & !r$synergy & r$intervention == dff$intervention[i])
    #r$counterfactual & r$synergy & r$intervention == dff$intervention[i])
  }
  
  # Selecting correct variable _________________________________________________
  if (dff$variable[i] == "vintage") {
    differences <- rr$novelty_dic_minMeSHlag_0days_granted -
      rr$novelty_dic_minMeSHlag_0days_declined
    
    dff$mean_nov_funded[i] <- mean(
      rr$novelty_dic_minMeSHlag_0days_granted,
      na.rm = TRUE
    )
  }
  if (dff$variable[i] == "recombination") {
    differences <- rr$shibayama_title_granted -
      rr$shibayama_title_declined
    
    dff$mean_nov_funded[i] <- mean(
      rr$shibayama_title_granted,
      na.rm = TRUE
    )
  }
  
  # Calculating what we need ___________________________________________________
  dff$n[i] <- length(differences[!is.na(differences)])
  dff$mean_diff[i] <- mean(differences, na.rm = TRUE)
  dff$sd_diff[i] <- sd(differences, na.rm = TRUE)
  dff$se_diff[i] <- dff$sd_diff[i] / sqrt(dff$n[i])
  
  # 95% CI:
  t_value <- qt(0.975, df = dff$n[i] - 1)
  
  # Calculate CI
  dff$lower[i] <- dff$mean_diff[i] - t_value * dff$se_diff[i]
  dff$upper[i] <- dff$mean_diff[i] + t_value * dff$se_diff[i]
  
  #  # Adding a variable to distinguish between real and counterfactual
  # interventions.
  
  dff$counterfactual <- "Actual funding decisions"
  dff$counterfactual[
    dff$intervention %in% interventionsLabels[3:length(interventionsLabels)]
  ] <- paste0(
    "Counterfactual funding decisions\n",
    "with lower-quality proposals\n",
    "(simulation of Regular panels)"
  )
}

# And reversing factor levels for plotting things in the right order:
dff$intervention <- forcats::fct_rev(dff$intervention)

dff$variable <- factor(
  x = dff$variable,
  levels = c("vintage", "recombination"),
  labels = c("New Vintage", "Creative Recombination")
)

Figure_2_reg_loweredQuality <-  ggplot(
  data = dff, 
  aes(x = mean_diff, y = intervention, xmin = lower, xmax = upper)
) +
  geom_rect(
    aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), 
    fill = "black", 
    alpha = 0.025
  ) +
  geom_col(fill = "gray55", width = 0.5) +
  geom_errorbar(width = 0.1) +
  labs(
    title = NULL,
    subtitle = NULL,
    pattern = "",
    x = NULL, 
    y = NULL,
    caption = paste0(
      "* From counterfactual funding decisions based\n",
      "   on simulations of Regular panels\n",
      "   assuming lower-quality proposals"
    )
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  facet_grid(
    ~variable,
    scales = "free"
  ) +
  geom_text(
    aes(x = mean_diff, y = intervention, label = round(mean_diff, digits = 3)),
    hjust = 0, # Adjust position horizontally
    vjust = -2,    # Adjust position vertically
    color = "black", # Text color
    size = 2      # Text size
  ) +
  scale_x_continuous(
    expand = c(0, 0.005),
    breaks = c(0),
    labels = c("Δ(Funded - Declined) = 0")
  ) +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray93", linewidth = 0.5),
    panel.background = element_rect(color = NA, fill = "gray97"),
    strip.background = element_blank(),
    strip.text = element_text(angle = 0, hjust = 0.2),
    axis.text.x = element_text(angle = 10, hjust = 1),
    #legend.position = "bottom"
    plot.caption = element_text(hjust = 0, vjust = 0, face = "italic") 
  )
print(Figure_2_reg_loweredQuality)

ggsave(
  #"./outputGraphics/Figure_2_synergyCounterfactuals.png",
  "./outputGraphics/Figure_2_reg_loweredQuality.png",
  plot = Figure_2_reg_loweredQuality,
  width = 2000,
  height = 950,
  units = "px",
  bg = "white",
  dpi = 300
)



################################################################################
# Robustness: counterfactual novelty-dedicated panels with lowered quality
#
r <- rBackup
r <- subset(
  r,
  r$increasedSubmissions == 0 &
    #(!r$counterfactual | r$loweredQuality == 1) &
    (!r$counterfactual | r$loweredQuality == 0.5) &
    (!r$counterfactual | (r$synergy & r$counterfactual))
)

dff <- expand.grid(
  intervention = interventionsLabels, 
  variable = c("vintage", "recombination")
)

dff$upper <- dff$lower <- dff$se_diff <- dff$sd_diff <- 
  dff$mean_diff <- dff$n <- NA

for (i in 1:nrow(dff)) {
  
  # Selecting the runs we need _________________________________________________
  if (dff$intervention[i] == "Regular (baseline)") {
    rr <- subset(r, !r$counterfactual & !r$synergy)
  }
  if (dff$intervention[i] == "Novelty-Dedicated") {
    rr <- subset(r, !r$counterfactual & r$synergy)
  }
  if ( # If counterfactual:
    dff$intervention[i] != "Regular (baseline)" & 
    dff$intervention[i] != "Novelty-Dedicated"
  ) {
    rr <- subset(
      r, 
      r$counterfactual & r$synergy & r$intervention == dff$intervention[i])
    #r$counterfactual & r$synergy & r$intervention == dff$intervention[i])
  }
  
  # Selecting correct variable _________________________________________________
  if (dff$variable[i] == "vintage") {
    differences <- rr$novelty_dic_minMeSHlag_0days_granted -
      rr$novelty_dic_minMeSHlag_0days_declined
    
    dff$mean_nov_funded[i] <- mean(
      rr$novelty_dic_minMeSHlag_0days_granted,
      na.rm = TRUE
    )
  }
  if (dff$variable[i] == "recombination") {
    differences <- rr$shibayama_title_granted -
      rr$shibayama_title_declined
    
    dff$mean_nov_funded[i] <- mean(
      rr$shibayama_title_granted,
      na.rm = TRUE
    )
  }
  
  # Calculating what we need ___________________________________________________
  dff$n[i] <- length(differences[!is.na(differences)])
  dff$mean_diff[i] <- mean(differences, na.rm = TRUE)
  dff$sd_diff[i] <- sd(differences, na.rm = TRUE)
  dff$se_diff[i] <- dff$sd_diff[i] / sqrt(dff$n[i])
  
  # 95% CI:
  t_value <- qt(0.975, df = dff$n[i] - 1)
  
  # Calculate CI
  dff$lower[i] <- dff$mean_diff[i] - t_value * dff$se_diff[i]
  dff$upper[i] <- dff$mean_diff[i] + t_value * dff$se_diff[i]
  
  #  # Adding a variable to distinguish between real and counterfactual
  # interventions.
  
  dff$counterfactual <- "Actual funding decisions"
  dff$counterfactual[
    dff$intervention %in% interventionsLabels[3:length(interventionsLabels)]
  ] <- paste0(
    "Counterfactual funding decisions\n",
    "with lower-quality proposals\n",
    "(simulation of Novelty-Dedicated panels)"
  )
}

# And reversing factor levels for plotting things in the right order:
dff$intervention <- forcats::fct_rev(dff$intervention)

dff$variable <- factor(
  x = dff$variable,
  levels = c("vintage", "recombination"),
  labels = c("New Vintage", "Creative Recombination")
)

Figure_3_novded_loweredQuality <-  ggplot(
  data = dff, 
  aes(x = mean_diff, y = intervention, xmin = lower, xmax = upper)
) +
  geom_rect(
    aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), 
    fill = "black", 
    alpha = 0.025
  ) +
  geom_col(fill = "gray55", width = 0.5) +
  geom_errorbar(width = 0.1) +
  labs(
    title = NULL,
    subtitle = NULL,
    pattern = "",
    x = NULL, 
    y = NULL,
    caption = paste0(
      "* From counterfactual funding decisions based\n",
      "   on simulations of Novelt-Dedicated panels\n",
      "   assuming lower-quality proposals"
    )
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  facet_grid(
    ~variable,
    scales = "free"
  ) +
  geom_text(
    aes(x = mean_diff, y = intervention, label = round(mean_diff, digits = 3)),
    hjust = 0, # Adjust position horizontally
    vjust = -2,    # Adjust position vertically
    color = "black", # Text color
    size = 2      # Text size
  ) +
  scale_x_continuous(
    expand = c(0, 0.005),
    breaks = c(0),
    labels = c("Δ(Funded - Declined) = 0")
  ) +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray93", linewidth = 0.5),
    panel.background = element_rect(color = NA, fill = "gray97"),
    strip.background = element_blank(),
    strip.text = element_text(angle = 0, hjust = 0.2),
    axis.text.x = element_text(angle = 10, hjust = 1),
    #legend.position = "bottom"
    plot.caption = element_text(hjust = 0, vjust = 0, face = "italic") 
  )
print(Figure_3_novded_loweredQuality)

ggsave(
  #"./outputGraphics/Figure_2_synergyCounterfactuals.png",
  "./outputGraphics/Figure_3_novded_loweredQuality.png",
  plot = Figure_3_novded_loweredQuality,
  width = 2000,
  height = 950,
  units = "px",
  bg = "white",
  dpi = 300
)




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Robustness to alternative metrics
#
# Here we replicate Figure 2 and 3 using alternative metrics of new vintage
# and creative recombination.
# Starting with Figure 2:


# Removing runs from robustness checks
r <- rBackup
r <- subset(
  r,
  r$increasedSubmissions == 0 &    # higher submission rate
    r$loweredQuality == 0 &        # lower quality submissions
    (!r$counterfactual | (!r$synergy & r$counterfactual)) # Regular count. only
)

dff <- expand.grid(
  intervention = interventionsLabels, 
  variable = c(
    "New Vintage\nbased on new MeSH\n(baseline)", 
    "Creative Recombination\nbased on references titles\n(baseline)",
    "New Vintage\nbased on new N-grams",
    "Creative Recombination\nbased on references\ntitles and abstracts",
    "Hybrid measure\nbased on new MeSH pairs"
  )
)

dff$upper <- dff$lower <- dff$se_diff <- dff$sd_diff <- 
  dff$mean_diff <- dff$n <- NA

for (i in 1:nrow(dff)) {
  
  # Selecting the runs we need _________________________________________________
  if (dff$intervention[i] == "Regular (baseline)") {
    rr <- subset(r, !r$counterfactual & !r$synergy)
  }
  if (dff$intervention[i] == "Novelty-Dedicated") {
    rr <- subset(r, !r$counterfactual & r$synergy)
  }
  if ( # If counterfactual:
    dff$intervention[i] != "Regular (baseline)" & 
    dff$intervention[i] != "Novelty-Dedicated"
  ) {
    rr <- subset(
      r, 
      r$counterfactual & !r$synergy & r$intervention == dff$intervention[i])
    #r$counterfactual & r$synergy & r$intervention == dff$intervention[i])
  }
  
  # Selecting correct variable _________________________________________________
  if (dff$variable[i] == "New Vintage\nbased on new MeSH\n(baseline)") {
    differences <- rr$novelty_dic_minMeSHlag_0days_granted -
      rr$novelty_dic_minMeSHlag_0days_declined
    dff$mean_nov_funded[i] <- mean(
      rr$novelty_dic_minMeSHlag_0days_granted,
      na.rm = TRUE
    )
  }
  if (dff$variable[i] == 
      "Creative Recombination\nbased on references titles\n(baseline)") {
    differences <- rr$shibayama_title_granted -
      rr$shibayama_title_declined
    dff$mean_nov_funded[i] <- mean(
      rr$shibayama_title_granted,
      na.rm = TRUE
    )
  }
  # Now the alternative measures:
  if (dff$variable[i] == "New Vintage\nbased on new N-grams") {
    differences <- rr$Ngrams_dic_2_granted -
      rr$Ngrams_dic_2_declined
    dff$mean_nov_funded[i] <- mean(
      rr$Ngrams_dic_2_granted,
      na.rm = TRUE
    )
  }
  if (dff$variable[i] == 
      "Creative Recombination\nbased on references\ntitles and abstracts") {
    differences <- rr$shibayama_avg_granted -
      rr$shibayama_avg_declined
    dff$mean_nov_funded[i] <- mean(
      rr$shibayama_avg_granted,
      na.rm = TRUE
    )
  }
  if (dff$variable[i] == "Hybrid measure\nbased on new MeSH pairs") {
    differences <- rr$novelty_new_MeSH_pairs_dic_granted -
      rr$novelty_new_MeSH_pairs_dic_declined
    dff$mean_nov_funded[i] <- mean(
      rr$novelty_new_MeSH_pairs_dic_granted,
      na.rm = TRUE
    )
  }
  
  # Calculating what we need ___________________________________________________
  dff$n[i] <- length(differences[!is.na(differences)])
  dff$mean_diff[i] <- mean(differences, na.rm = TRUE)
  dff$sd_diff[i] <- sd(differences, na.rm = TRUE)
  dff$se_diff[i] <- dff$sd_diff[i] / sqrt(dff$n[i])
  
  # 95% CI:
  t_value <- qt(0.975, df = dff$n[i] - 1)
  
  # Calculate CI
  dff$lower[i] <- dff$mean_diff[i] - t_value * dff$se_diff[i]
  dff$upper[i] <- dff$mean_diff[i] + t_value * dff$se_diff[i]
  
  # Adding a variable to distinguish between real and counterfactual
  # interventions.
  dff$counterfactual <- "Actual funding decisions"
  dff$counterfactual[
    dff$intervention %in% interventionsLabels[3:length(interventionsLabels)]
  ] <- "Counterfactual funding decisions\n(simulation of Regular panels)"
}

# And reversing factor levels for plotting things in the right order:
dff$intervention <- forcats::fct_rev(dff$intervention)

#dff$variable <- factor(
#  x = dff$variable,
#  levels = c("vintage", "recombination"),
#  labels = c("New Vintage", "Creative Recombination")
#)



Figure_2_allMetrics <- ggplot(
  data = dff, 
  aes(x = mean_diff, y = intervention, xmin = lower, xmax = upper)
) +
  geom_rect(
    aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), 
    fill = "black", 
    alpha = 0.025
  ) +
  geom_col(fill = "gray55", width = 0.5) +
  geom_errorbar(width = 0.1) +
  labs(
    title = NULL,
    subtitle = NULL,
    pattern = "",
    x = NULL, 
    y = NULL
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  facet_wrap(~variable, ncol = 2, scales = "free") +
  geom_text(
    aes(x = mean_diff, y = intervention, label = round(mean_diff, digits = 3)),
    hjust = 0,
    vjust = -1.1,
    color = "black",
    size = 2
  ) +
  scale_x_continuous(
    expand = c(0, 0.005),
    breaks = c(0),
    labels = c("Δ(Funded - Declined) = 0")
  ) +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray93", linewidth = 0.5),
    panel.background = element_rect(color = NA, fill = "gray97"),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 8, hjust = 1),
    #legend.position = "bottom"
    #legend.position = c(0.5, 0), 
    #legend.justification = c(0, 0),
    #legend.key.spacing.y = unit(10, "pt")
    plot.caption = element_text(hjust = 0, vjust = 1, face = "italic")
  )

Figure_2_allMetrics <- gridExtra::grid.arrange(
  Figure_2_allMetrics, 
  bottom = grid::grid.text(
    paste0(
      "* From counterfactual funding decisions based\n",
      "   on simulations of Regular panels"
    ),
    x = 0.55, 
    y = 2, hjust = 0, 
    vjust = 0, 
    gp = grid::gpar(fontsize = 10, fontface = "italic")
  )
)

print(Figure_2_allMetrics)

ggsave(
  #"./outputGraphics/Figure_2_synergyCounterfactuals.png",
  "./outputGraphics/Figure_2_allMetrics.png",
  plot = Figure_2_allMetrics,
  width = 2000,
  height = 2000,
  units = "px",
  bg = "white",
  dpi = 300
)


################################################################################
# Doing the same with Figure 3:


# Removing runs from robustness checks
r <- rBackup
r <- subset(
  r,
  r$increasedSubmissions == 0 &    # higher submission rate
    r$loweredQuality == 0 &        # lower quality submissions
    (!r$counterfactual | (r$synergy & r$counterfactual)) # Nov-Ded count. only
)


dff <- expand.grid(
  intervention = interventionsLabels, 
  variable = c(
    "New Vintage\nbased on new MeSH\n(baseline)", 
    "Creative Recombination\nbased on references titles\n(baseline)",
    "New Vintage\nbased on new N-grams",
    "Creative Recombination\nbased on references\ntitles and abstracts",
    "Hybrid measure\nbased on new MeSH pairs"
  )
)

dff$upper <- dff$lower <- dff$se_diff <- dff$sd_diff <- 
  dff$mean_diff <- dff$n <- NA

for (i in 1:nrow(dff)) {
  
  # Selecting the runs we need _________________________________________________
  if (dff$intervention[i] == "Regular (baseline)") {
    rr <- subset(r, !r$counterfactual & !r$synergy)
  }
  if (dff$intervention[i] == "Novelty-Dedicated") {
    rr <- subset(r, !r$counterfactual & r$synergy)
  }
  if ( # If counterfactual:
    dff$intervention[i] != "Regular (baseline)" & 
    dff$intervention[i] != "Novelty-Dedicated"
  ) {
    rr <- subset(
      r, 
      #r$counterfactual & !r$synergy & r$intervention == dff$intervention[i])
      r$counterfactual & r$synergy & r$intervention == dff$intervention[i])
  }
  
  # Selecting correct variable _________________________________________________
  if (dff$variable[i] == "New Vintage\nbased on new MeSH\n(baseline)") {
    differences <- rr$novelty_dic_minMeSHlag_0days_granted -
      rr$novelty_dic_minMeSHlag_0days_declined
    dff$mean_nov_funded[i] <- mean(
      rr$novelty_dic_minMeSHlag_0days_granted,
      na.rm = TRUE
    )
  }
  if (dff$variable[i] == 
      "Creative Recombination\nbased on references titles\n(baseline)") {
    differences <- rr$shibayama_title_granted -
      rr$shibayama_title_declined
    dff$mean_nov_funded[i] <- mean(
      rr$shibayama_title_granted,
      na.rm = TRUE
    )
  }
  # Now the alternative measures:
  if (dff$variable[i] == "New Vintage\nbased on new N-grams") {
    differences <- rr$Ngrams_dic_2_granted -
      rr$Ngrams_dic_2_declined
    dff$mean_nov_funded[i] <- mean(
      rr$Ngrams_dic_2_granted,
      na.rm = TRUE
    )
  }
  if (dff$variable[i] == 
      "Creative Recombination\nbased on references\ntitles and abstracts") {
    differences <- rr$shibayama_avg_granted -
      rr$shibayama_avg_declined
    dff$mean_nov_funded[i] <- mean(
      rr$shibayama_avg_granted,
      na.rm = TRUE
    )
  }
  if (dff$variable[i] == "Hybrid measure\nbased on new MeSH pairs") {
    differences <- rr$novelty_new_MeSH_pairs_dic_granted -
      rr$novelty_new_MeSH_pairs_dic_declined
    dff$mean_nov_funded[i] <- mean(
      rr$novelty_new_MeSH_pairs_dic_granted,
      na.rm = TRUE
    )
  }
  
  # Calculating what we need ___________________________________________________
  dff$n[i] <- length(differences[!is.na(differences)])
  dff$mean_diff[i] <- mean(differences, na.rm = TRUE)
  dff$sd_diff[i] <- sd(differences, na.rm = TRUE)
  dff$se_diff[i] <- dff$sd_diff[i] / sqrt(dff$n[i])
  
  # 95% CI:
  t_value <- qt(0.975, df = dff$n[i] - 1)
  
  # Calculate CI
  dff$lower[i] <- dff$mean_diff[i] - t_value * dff$se_diff[i]
  dff$upper[i] <- dff$mean_diff[i] + t_value * dff$se_diff[i]
  
  # Adding a variable to distinguish between real and counterfactual
  # interventions.
  dff$counterfactual <- "Actual funding decisions"
  dff$counterfactual[
    dff$intervention %in% interventionsLabels[3:length(interventionsLabels)]
  ] <- "Counterfactual funding decisions\n(simulation of Novelty-Dedicated panels)"
}

# And reversing factor levels for plotting things in the right order:
dff$intervention <- forcats::fct_rev(dff$intervention)

#dff$variable <- factor(
#  x = dff$variable,
#  levels = c("vintage", "recombination"),
#  labels = c("New Vintage", "Creative Recombination")
#)



Figure_3_allMetrics <- ggplot(
  data = dff, 
  aes(x = mean_diff, y = intervention, xmin = lower, xmax = upper)
) +
  geom_rect(
    aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), 
    fill = "black", 
    alpha = 0.025
  ) +
  geom_col(fill = "gray55", width = 0.5) +
  geom_errorbar(width = 0.1) +
  labs(
    title = NULL,
    subtitle = NULL,
    pattern = "",
    x = NULL, 
    y = NULL
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  facet_wrap(~variable, ncol = 2, scales = "free") +
  geom_text(
    aes(x = mean_diff, y = intervention, label = round(mean_diff, digits = 3)),
    hjust = 0,
    vjust = -1.1,
    color = "black",
    size = 2
  ) +
  scale_x_continuous(
    expand = c(0, 0.005),
    breaks = c(0),
    labels = c("Δ(Funded - Declined) = 0")
  ) +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray93", linewidth = 0.5),
    panel.background = element_rect(color = NA, fill = "gray97"),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 8, hjust = 1),
    #legend.position = "bottom"
    #legend.position = c(0.5, 0), 
    #legend.justification = c(0, 0),
    #legend.key.spacing.y = unit(10, "pt")
    plot.caption = element_text(hjust = 0, vjust = 1, face = "italic")
  )

Figure_3_allMetrics <- gridExtra::grid.arrange(
  Figure_3_allMetrics, 
  bottom = grid::grid.text(
    paste0(
      "* From counterfactual funding decisions based\n",
      "   on simulations of Regular panels"
    ),
    x = 0.55, 
    y = 2, hjust = 0, 
    vjust = 0, 
    gp = grid::gpar(fontsize = 10, fontface = "italic")
  )
)

ggsave(
  #"./outputGraphics/Figure_2_synergyCounterfactuals.png",
  "./outputGraphics/Figure_3_allMetrics.png",
  plot = Figure_3_allMetrics,
  width = 2000,
  height = 2000,
  units = "px",
  bg = "white",
  dpi = 300
)

















################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Cohen's Kappa
r <- rBackup
df <- subset( # selecting baseline
  r,
  r$increasedSubmissions == 0 &    # higher submission rate
    r$loweredQuality == 0 &        # lower quality submissions
    !r$synergy & 
    r$counterfactual
  #(!r$counterfactual | (r$synergy & r$counterfactual))# 
)

# And reversing factor levels for plotting things in the right order:
df$intervention <- forcats::fct_rev(df$intervention)

Figure_Kappa <- ggplot(
  data = df,
  aes(x = CohensKappa, y = intervention)
) +
  geom_violin(fill = "gray90", color = "gray90", scale = "width", adjust = 1.2) +
  geom_vline(xintercept = 1, color = "black", linetype = "longdash") + # K=1
  geom_vline(xintercept = 0.12, color = "black", linetype = "dotted") + # K=Folgel
  geom_boxplot(width = 0.15, color = "black", outlier.size = 0.5) +
  scale_x_continuous(breaks = c(-1, -0.5, 0, 0.12, 0.5, 1)) +
  #expand_limits(y = c(0,5)) +
  #scale_y_discrete(expand = c(1, 1)) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3) +
  #scale_y_discrete(position = "right") +
  labs(
    x = paste0(
      "Similarity between counterfactual\n",
      "and actual panel decisions\n",
      "(Cohen's Kappa)"
    ),
    y = "",
    title = ""
  ) +
  annotate(
    "text", label = "Fogelholm et al. (2012)",
    x = 0.12, y = I(3.85), size = 3, colour = "black", angle = 30, hjust = 0
  ) +
  annotate(
    "text", label = "Maximal similarity",
    x = 0.99, y = I(3.85), size = 3, colour = "black", angle = 30, hjust = 0
  ) +
  coord_cartesian(clip = 'off') +
  theme(
    plot.margin = margin(45, 50, 10, 10), # added padding at the top and right
    plot.background = element_blank(),
    legend.position = "NA",
    panel.background = element_rect(color = NULL, fill = "gray98"),
    panel.grid = element_line(color = "gray93"),
    axis.text.x = element_text(size = 7, angle = 30, hjust = 1),
    axis.ticks = element_blank(),
    plot.title.position = "plot"
  )
print(Figure_Kappa)

ggsave(
  #"./outputGraphics/Figure_2_old_synergyCounterfactuals.png",
  "./outputGraphics/Figure_Kappa.png",
  plot = Figure_Kappa,
  width = 1800,
  height = 1200,
  units = "px",
  bg = "white",
  #bg = "transparent",
  dpi = 300
)

mean(df$CohensKappa[df$intervention == "Golden Tickets"], na.rm = T)
mean(df$CohensKappa[df$intervention == "Tie-Breaking Lotteries"], na.rm = T)
mean(df$CohensKappa[df$intervention == "Fundable-Pool Lotteries"], na.rm = T)














################################################################################
# Descriptives

load("./counterfactuals/input.RData")



table(1 - as.numeric(a$fundingOutcome == "Declined"))
round(summary(1 - as.numeric(a$fundingOutcome == "Declined")), digits = 3)
round(sd(1 - as.numeric(a$fundingOutcome == "Declined")), digits = 3)
sum(!is.na(a$fundingOutcome)) # N


round(summary(a$novelty_dic_minMeSHlag_0days), digits = 3)
round(sd(a$novelty_dic_minMeSHlag_0days), digits = 3)
sum(!is.na(a$novelty_dic_minMeSHlag_0days)) # N
#

round(summary(a$shibayama_title), digits = 3)
round(sd(a$shibayama_title, na.rm = TRUE), digits = 3)
sum(!is.na(a$shibayama_title)) # N
#
table(a$synergy)
sum(!is.na(a$synergy)) # N

round(summary(a$competition), digits = 3)
round(sd(a$competition, na.rm = TRUE), digits = 3)
sum(!is.na(a$competition)) # N
#

round(summary(a$meanScore), digits = 3)
round(sd(a$meanScore, na.rm = TRUE), digits = 3)
sum(!is.na(a$meanScore)) # N
#

round(summary(a$competition), digits = 3)
round(sd(a$competition, na.rm = TRUE), digits = 3)
sum(!is.na(a$competition)) # N
#

table(a$malePI)
round(summary(as.numeric(a$malePI)), digits = 3)
round(sd(as.numeric(a$malePI), na.rm = TRUE), digits = 3)
sum(!is.na(a$malePI)) # N

round(summary(as.numeric(a$ApplicationYear)), digits = 3)
round(sd(as.numeric(a$ApplicationYear), na.rm = TRUE), digits = 3)
sum(!is.na(a$ApplicationYear)) # N

table(a$youngPI)
round(summary(as.numeric(a$youngPI)), digits = 3)
round(sd(as.numeric(a$youngPI), na.rm = TRUE), digits = 3)
sum(!is.na(a$youngPI)) # N


round(summary(a$Appliedamount), digits = 3)
round(sd(a$Appliedamount, na.rm = TRUE), digits = 3)
sum(!is.na(a$Appliedamount)) # N
#



round(summary(a$Appliedamount_corrected_log), digits = 3)
round(sd(a$Appliedamount_corrected_log, na.rm = TRUE), digits = 3)
sum(!is.na(a$Appliedamount_corrected_log)) # N
#



















# Older versions 
r <- treatments

r$treatment <- NA
r$treatmentType <- NA
r$treatmentLabel <- NA

for (i in 1:nrow(treatments)) {
  
  # Real (i.e. non counterfactual) calls other than Synergy are our baseline:
  if (
    r$calltype[i] == "other calls" &
    r$counterfactual[i] == FALSE 
  ) {
    r$treatment[i] <- "baseline\n(regular calls)"
    r$treatmentType[i] <- "baseline\n(regular calls)"
    r$treatmentLabel[i] <- ""
  }
  
  # Real (i.e. non counterfactual) synergy calls are "novelty-dedicated"
  if (
    r$calltype[i] == "Synergy calls" &
    r$counterfactual[i] == FALSE 
  ) {
    r$treatment[i] <- "novelty-dedicated calls"
    r$treatmentType[i] <- "novelty-dedicated calls"
    r$treatmentLabel[i] <- ""
  }
  
  # If lottery treatment, then we only keep lotteries with aggregation by mean.
  # All other lottery treatments will remain "NA".
  if (
    r$calltype[i] == "other calls" &
    r$randomization[i] == "lottery of fundable proposals" &
    r$aggregationRule[i] == "mean"
  ) {
    r$treatment[i] <- "funding lottery"
    r$treatmentType[i] <- "funding lottery"
    r$treatmentLabel[i] <- ""
  }
  
  # For all other non-lottery counterfactual treatments, the treatment is merely
  # their aggregation rule.
  if (
    r$calltype[i] == "other calls" &
    r$randomization[i] == "no randomization" &
    r$counterfactual[i] == TRUE &
    r$aggregationRule[i] == "mean"
  ) {
    r$treatment[i] <- "aggregation by mean"
    r$treatmentType[i] <- "aggregation rule"
    r$treatmentLabel[i] <- "aggregation by mean"
  }
  if (
    r$calltype[i] == "other calls" &
    r$randomization[i] == "no randomization" &
    r$counterfactual[i] == TRUE &
    r$aggregationRule[i] == "median"
  ) {
    r$treatment[i] <- "aggregation by median"
    r$treatmentType[i] <- "aggregation rule"
    r$treatmentLabel[i] <- "aggregation by median"
  }
  if (
    r$calltype[i] == "other calls" &
    r$randomization[i] == "no randomization" &
    r$counterfactual[i] == TRUE &
    r$aggregationRule[i] == "best score"
  ) {
    r$treatment[i] <- "aggregation by best score"
    r$treatmentType[i] <- "aggregation rule"
    r$treatmentLabel[i] <- "aggregation by best score"
  }
  if (
    r$calltype[i] == "other calls" &
    r$randomization[i] == "no randomization" &
    r$counterfactual[i] == TRUE &
    r$aggregationRule[i] == "goldenTickets"
  ) {
    r$treatment[i] <- "golden tickets"
    r$treatmentType[i] <- "golden tickets"
    r$treatmentLabel[i] <- ""
  }
  
}
table(r$treatment)


# Revmoving treatments that we don't use for the ENPOSS presentation
r <- r[!is.na(r$treatment),]

# Removing non-valid treatments
r <- r[r$valid,]

# Now, recoding the variable treatment, treatmentType and treatmentLables
treatmentLevels <- c(
  "baseline\n(regular calls)",
  "novelty-dedicated calls",
  "aggregation by mean",
  "aggregation by median",
  "aggregation by best score",
  "golden tickets",
  "funding lottery"
)
r$treatment <- factor(
  x = r$treatment,
  levels = rev(treatmentLevels),
  labels = rev(treatmentLevels)
)

treatmentTypeLevels <- c(
  "baseline\n(regular calls)",
  "novelty-dedicated calls",
  "aggregation rule",
  "golden tickets",
  "funding lottery"
)
r$treatmentType <- factor(
  x = r$treatmentType,
  levels = treatmentTypeLevels,#rev(treatmentLevels),
  labels = treatmentTypeLevels#rev(treatmentLevels)
)

treatmentLabelLevels <- c(
  "aggregation by mean",
  "aggregation by median",
  "aggregation by best score",
  ""
)
r$treatmentLabel <- factor(
  x = r$treatmentLabel,
  levels = rev(treatmentLabelLevels),#rev(treatmentLevels),
  labels = rev(treatmentLabelLevels)#rev(treatmentLevels)
)



# Descriptives
table(r$treatment[!r$counterfactual]) # How many actual calls

sum(r$nApplications[!r$counterfactual], na.rm = TRUE)

sum(r$nReviews[!r$counterfactual], na.rm = TRUE)
#



############## Fig 2

r1 <- subset(
  r, 
  r$treatment %in% c("baseline\n(regular calls)", "novelty-dedicated calls")
)
#levels(r1$treatment)[levels(r1$treatment) == "baseline"] <- 
#  "baseline\n(regular calls)"


# Formatting
r1 <- reshape2::melt(
  data = r,
  id.vars = c(
    "call",
    "aggregationRule",
    "randomization",
    "Callname",
    "Applicationyear_new",
    "Funding_type",
    "counterfactual",
    "valid",
    "nApplications",
    "nFunded",
    "setDiff",
    "CohensKappa",
    "calltype",
    "propSetDiff",
    "treatment",
    "treatmentType",
    "treatmentLabel"
  ),
  measure.vars = c(
    "shibayama_title_granted",
    "novelty_dic_minMeSHlag_0days_granted"
  )
)
r1$variable <- as.character(r1$variable)
r1$variable[r1$variable == "novelty_dic_minMeSHlag_0days_granted"] <- 
  "vintage"
r1$variable[r1$variable == "shibayama_title_granted"] <- 
  "recombination"


r1$variable <- factor(r1$variable, levels = rev(levels(factor(r1$variable))))

Figure_2 <- ggplot(
  data = r1, 
  aes(x = value, y = treatmentLabel, fill = treatment)
) +
  geom_violin(color = "NA", fill = "gray70", alpha = 0.5) +
  geom_boxplot(width = 0.2, color = "black", outlier.size = 0.5) +
  #stat_summary(fun = mean, geom = "point", shape = 5, size = 1.3) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3) +
  facet_grid(
    cols = vars(as.factor(variable)),
    rows = vars(treatmentType),
    scales = "free",
    switch = "y"
  ) +
  #coord_flip() +
  ylab("") + 
  xlab("\nlow novelty     ↔     high novelty") + 
  scale_fill_manual(values = c(rep("white", times = 5), "#99b8de", "#0e4e99")) +
#scale_y_discrete(position = "right", labels = treatmentLabelLevels) +
  scale_y_discrete(position = "right") +
  scale_x_continuous(position = "bottom") +
  #scale_fill_manual(values = c("#e5e5e5", "#0e4e99")) + #"#243c58"
  theme(
    legend.position = "NA",
    #strip.placement = "outside",
    strip.background = element_rect(color = "#00000000", fill = "#00000000"),
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    panel.background = element_rect(color = NULL, fill = "gray98"),
    panel.grid = element_line(color = "gray93"),
    axis.text.x = element_text(size = 7, angle = 30, hjust = 1),
    axis.ticks = element_blank()#,
    #plot.background = element_blank(),
  )
print(Figure_2)

ggsave(
  "./outputGraphics/Figure_2.png",
  plot = Figure_2,
  width = 2000,
  height = 1300,
  units = "px",
  dpi = 300
)



################
# Now the same for all measures (appendix figure a2)
r$vintageMesh <- r$novelty_dic_minMeSHlag_0days_granted # keep this as integer
r$recombinationTitle <- r$shibayama_title_granted
r$vintageNgrams <- r$Ngrams_dic_2_granted
r$recombinationAbstract <- r$shibayama_abs_granted
r$hybrid <- r$novelty_new_MeSH_pairs_dic_granted

novMeasures <- c(
  "vintageMesh",
  "recombinationTitle",
  "vintageNgrams",
  "recombinationAbstract",
  "hybrid"
)

r1 <- reshape2::melt(
  data = r,
  id.vars = c(
    "call",
    "aggregationRule",
    "randomization",
    "Callname",
    "Applicationyear_new",
    "Funding_type",
    "counterfactual",
    "valid",
    "nApplications",
    "nFunded",
    "setDiff",
    "CohensKappa",
    "calltype",
    "propSetDiff",
    "treatment",
    "treatmentType",
    "treatmentLabel"
  ),
  measure.vars = novMeasures
)
r1$variable <- as.character(r1$variable)

r1$variable <- factor(
  x = r1$variable,
  levels = unique(r1$variable),
  labels = c(
    "vintage - MeSH",
    "recombination - title",
    "vintage - N-grams",
    "recombination - abstract",
    "hybrid - MeSH term pairs"
  )
)

#(r1$treatmentLabel)




Figure_a2 <- ggplot(
  data = r1, 
  aes(x = value, y = treatmentLabel, fill = treatment)
) +
  geom_violin(color = "NA", fill = "gray70", alpha = 0.5) +
  geom_boxplot(width = 0.2, color = "black", outlier.size = 0.5) +
  #stat_summary(fun = mean, geom = "point", shape = 5, size = 1.3) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3) +
  facet_grid(
    cols = vars(variable),
    rows = vars(treatmentType),
    scales = "free",
    switch = "y"
  ) +
  #coord_flip() +
  ylab("") + 
  xlab("\nlow novelty     ↔     high novelty") + 
  scale_fill_manual(values = c(rep("white", times = 5), "#99b8de", "#0e4e99")) +
  #scale_y_discrete(position = "right", labels = treatmentLabelLevels) +
  scale_y_discrete(position = "right") +
  scale_x_continuous(position = "bottom") +
  #scale_fill_manual(values = c("#e5e5e5", "#0e4e99")) + #"#243c58"
  theme(
    legend.position = "NA",
    #strip.placement = "outside",
    strip.background = element_rect(color = "#00000000", fill = "#00000000"),
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 7),
    strip.text.x = element_text(size = 7),#color = "red"),
    panel.background = element_rect(color = NULL, fill = "gray98"),
    panel.grid = element_line(color = "gray93"),
    axis.text.x = element_text(size = 7, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 7),
    axis.ticks = element_blank()#,
    #plot.background = element_blank(),
  )
print(Figure_a2)

ggsave(
  "./outputGraphics/Figure_a2.png",
  plot = Figure_a2,
  width = 3000,
  height = 1300,
  units = "px",
  dpi = 300
)






################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Plotting the distance between real and counterfactual funding decisions:


ggplot(
  data = r[r$counterfactual,],
  aes(x = CohensKappa, y = treatmentLabel)
) +
  geom_violin(fill = "gray90", color = "gray90") +
  geom_vline(xintercept = 1, color = "#0e4e99") +
  geom_boxplot(width = 0.2, color = "black", outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3) +
  facet_grid(
    rows = vars(treatmentType),
    scales = "free_y",
    switch = "y"
  ) +
  scale_y_discrete(position = "right") +
  labs(
    x = "similarity to real panel choice\n(Cohen's Kappa)",
    y = "",
    title = ""
  ) +
  theme(
    legend.position = "NA",
    #strip.placement = "outside",
    strip.background = element_rect(color = "#00000000", fill = "#00000000"),
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    panel.background = element_rect(color = NULL, fill = "gray98"),
    panel.grid = element_line(color = "gray93"),
    axis.text.x = element_text(size = 7, angle = 30),
    axis.ticks = element_blank(),
    plot.title.position = "plot"
    #plot.title = element_text(hjust = -1)
    #plot.background = element_blank(),
  )

