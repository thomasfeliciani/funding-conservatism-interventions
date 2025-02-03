
# Clearing environment
rm(list = ls())

# Loading libraries
library("XML")
library("haven")
library("reshape2")
library("ggplot2")


# Reading and binding partial datasets if necessary
if (
  file.exists("./NIH MeSH/MeSH terms.csv") &
  !file.exists("./NIH MeSH/MeSH terms_complete.csv")
) {
  if (file.exists("./NIH MeSH/MeSH terms_2.csv") & 
      file.exists("./NIH MeSH/MeSH terms_3.csv")) {
    df1 <- read.csv2("./NIH MeSH/MeSH terms.csv")
    df2 <- read.csv2("./NIH MeSH/MeSH terms_2.csv", header = FALSE)
    df3 <- read.csv2("./NIH MeSH/MeSH terms_3.csv", header = FALSE)
    
    # Combining the data:
    names(df3) <- names(df2) <- names(df1)
    df_complete <- rbind(df1, df2, df3)
    rm(df1, df2, df3)
    
  } else {
    df_complete <- read.csv2("./NIH MeSH/MeSH terms.csv")
  }
  
  write.csv2(df_complete, "./NIH MeSH/MeSH terms_complete.csv", row.names = F)
  rm(df_complete)
}



# Currently, the output from the API is stored as a character string. Here I
# extract from it the information we need and save it in more usable formats.
df = read.csv2("./NIH MeSH/MeSH terms_complete.csv")


MeSH_terms <- MeSH_UI <- MeSH_MTIscore <- data.frame(
  "Application source record id" = df$"Application.source.record.id",
  V2 = NA
)


# Now we can read the raw output line by line:
pb = txtProgressBar(
  title = "Elaborating raw output from the MeSH on Demand API...",
  min = 0, max = nrow(MeSH_terms), initial = 0, style = 3
)
for(i in 1:nrow(MeSH_terms)) { # For every application...
  
  # ... separate the raw output into the different MeSH terms...
  s <- base::strsplit(x = df$terms[i], split = "\\\\n")[[1]]
  
  for (t in 1:(length(s) - 1)) { # ... and for every MeSH term...
    # ... this string contains info regarding the t-th assigned term:
    x = s[t + 1]
    
    # And according to API documentation, this is the content of the string:
    # [The] MeSH Unique Identifier (DUI/CUI), UMLS Concept Identifier (CUI), and
    # a MTI score.
    # (From https://ii.nlm.nih.gov/resource/MTI_help_info.html )
    #
    # Therefore, extrapolating a bit, this is what each substring of x is:
    x = base::strsplit(x = x, split = "\\|")[[1]]
    # X[1] seems to encode the format, in this case "00000000" is for free text
    Term = x[2]
    UI = x[3]
    MTIscore = x[4]
    
    # I save each component into its relevant table:
    MeSH_terms[i, t + 1] <- Term
    MeSH_UI[i, t + 1] <- UI
    MeSH_MTIscore[i, t + 1] <- MTIscore
  }
  
  setTxtProgressBar(pb, i)
}
close(pb)

rm(df, pb)

# ______________________________________________________________________________
# Next I assign dates to each MeSH term.

# Reading the XML containing the "term descriptors", i.e. the MeSH term metadata
# that also include their date. Note that these metadata are spread across two
# datafiles. I downloaded them from: 
# https://www.nlm.nih.gov/databases/download/mesh.html 
print("loading MeSH descriptors. This will take a while.")
md1 <- XML::xmlToDataFrame(
  doc = "./NIH MeSH/desc2024/desc2024.xml", 
  homogeneous = TRUE,
  collectNames = FALSE
)
md2 <- XML::xmlToDataFrame(
  doc = "./NIH MeSH/desc2024/supp2024.xml", 
  homogeneous = TRUE,
  collectNames = FALSE
)


#md1 <- md1[,c("DescriptorUI", "DescriptorName", "DateCreated")]
#md2 <- md2[,c("SupplementalRecordUI", "SupplementalRecordName", "DateCreated")]
#names(md1) <- names(md2) <- c("UI", "term", "dateCreated")
#md2 <- XML::xmlToDataFrame("./NIH MeSH/desc2024/supp2024.xml")
#md2 <- XML::xmlToList("./NIH MeSH/desc2024/supp2024.xml")


# Initializing a new table where we'll store the dates
MeSH_date <- MeSH_UI

# And then we loop through each MeSH term in our data and look up their dates in
# the metadata.
pb = txtProgressBar(
  title = "Extracting the age of MeSH terms.",
  min = 0, max = nrow(MeSH_UI), initial = 0, style = 3
)
for (i in 1:nrow(MeSH_UI)) { # For each application...
  for (t in 2:ncol(MeSH_UI)) { # ...and for each MeSH term...
    
    #print(c(i, t))
    # ... the date can be found in the md dataframe, at the row corresponding to
    # the MeSH term with the same MeSH Unique Identifier.
    # Note that it makes a difference whether we use the "DateCreated" or
    # the "DateEstablished". For our purposes we need df$DateCreated.
    UI <- MeSH_UI[i,t]
    if(is.na(UI)) next
    
    # Some MeSH terms are "descriptors" and some are "supplemental record".
    # Based on that, their date will be stored in a different table. This ifelse
    # statement figures that out based on the MeSH id and looks up the date
    # in the appropriate table.
    ifelse(
      substr(x = MeSH_UI[i,t], start = 1, stop = 1) == "D",
      date <- md1$DateCreated[which(md1$DescriptorUI == UI)],
      date <- md2$DateCreated[which(md2$SupplementalRecordUI == UI)]
      
    )
    
    # Updating the date dataframe 
    ifelse(
      length(date) == 0,
      MeSH_date[i,t] <- NA,
      MeSH_date[i,t] <- date
    )
    
    #MeSH_date[i,t] <- md$DateCreated[which(md$DescriptorUI == DUI)]
    #MeSH_date[i,t] <- md$DateEstablished[which(md$DescriptorUI == UI)]
  }
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb)


# Time to save our output.
write.csv2(MeSH_UI, "./NIH MeSH/output/MeSH_UI.csv", row.names = FALSE)
write.csv2(MeSH_terms, "./NIH MeSH/output/MeSH_terms.csv", row.names = FALSE)
write.csv2(MeSH_MTIscore, "./NIH MeSH/output/MeSH_MTIscore.csv", row.names = F)
write.csv2(MeSH_date, "./NIH MeSH/output/MeSH_date.csv", row.names = FALSE)


# Let's have a quick look at what we have.
#
# Checking number of unique MeSH terms
test <- c()
for (c in 2:ncol(MeSH_terms)) test <- c(test, MeSH_terms[,c])
test <- test[!is.na(test)]
length(unique(test))
#
# Checking distribution of dates
test <- c()
for (c in 2:ncol(MeSH_date)) test <- c(test, MeSH_date[,c])
test <- test[!is.na(test)]
test <- as.numeric(test)
hist(test, breaks = 100)



# Applications with these IDs were probably returned invalid output from the
# API.
torepair <- MeSH_date$Application.source.record.id[which(is.na(MeSH_date$V2))]
print("MeSH terms generation may have failed for applications with IDs:")
print(torepair)
#View(df[df$Application.source.record.id %in% torepair,])
#
# Originally I found six of these ids to have had problems -- I paste them here:
# 1188507 1191337 1195163 1404954 1407427 1671427
# I then wrote a new python script, "MeSH - data repair.py", to query the NIH
# MeSH-on-Demand API again to try and fix the MeSH terms for these applications.
# It seems to have worked for all but one of these applications. Currenlty,
# only the application with id 1195163 has MeSH terms for which the date could
# not be computed.
# I can only attribute this issue with application id 1195163 to some
# inconsistency/discrepancy between the MeSH UI assigned by the MeSH-on-Demand
# API and those present in the MeSH descriptor files. I will put aside the 
# issue and simply mark the missing MeSH date as missing (NA).


#_______________________________________________________________________________
#
# Dates of MeSH terms
# Calculating summary stats about the time lag between applications and the 
# creation of their MeSH terms.
# 
# I start with parsing the date of MeSH terms and the date of application 
# submissions. So:
#
# Clearing environment so we start afresh:
rm(list = ls())
gc()

# Loading the freshly brewed MeSH term dates table:
md <- read.csv2("./NIH MeSH/output/MeSH_date.csv")

# Formatting all data columns of md into "Date" format:
for (c in 2:ncol(md)) md[,c] <- as.Date(as.character(md[,c]), format = "%Y%m%d")


# Also loading the application data:
a <- as.data.frame(haven::read_dta(
  "./NNF Project 20231228/2.DATA/original_source_imports/applications.dta"
))

# Dates in the dataframe a are already formatted correctly, have no missings nor
# "Epoch = 0"-type errors:
class(a$Applicationdate)
table(is.na(a$Applicationdate))
min(a$Applicationdate, na.rm = TRUE)
# ... Quite convenient!


# Some applications appear in the parquet file (here denoted "txt") but not in
# the application dataframe "original_source_imports/applications.dta" (here
# called "a"). Our MeSH terms and corresponding MeSH dates are based on the
# parquet/txt: we could thus remove those that don't have a corresponding entry
# in the dta/a:
#md <- md[md$Application.source.record.id %in% a$Applicationsourcerecordid,]



# Now we can easily proceed to calculating the time lag between each
# application's date and the date of its MeSH terms.
mtl <- md
mtl[,2:ncol(mtl)] <- NA


pb = txtProgressBar(
  title = "Calculating time lag between date of application and of MeSH terms",
  min = 0, max = nrow(a), initial = 0, style = 3
)
for (i in 1:nrow(a)) { # For every i-th application in "a"...
  
  # Find the corresponding row in mtl (containing i-th MeSH dates), if one 
  # exists:
  if (a$Applicationsourcerecordid[i] %in% mtl$Application.source.record.id) {
    mtlIndex <- which(
      mtl$Application.source.record.id == a$Applicationsourcerecordid[i]
    )
    
    # For each MeSH term assigned to application i, calculate the difference
    # between its date and the date of application, and write it in the
    # corresponding row/column of "mtl"
    for (l in 2:ncol(md)) if (!is.na(md[mtlIndex, l])) {
      
      # This is the time lag (in days)
      mtl[mtlIndex, l] <- a$Applicationdate[i] - md[mtlIndex, l]
    }  
  }
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb)


# The dataframe mtl now contains, for each application, the time lag between
# it and each of its MeSH term. This time lag is expressed in number of days.
# Let's see what the distribution of dates looks like:
test <- c()
for (c in 2:ncol(mtl)) test <- c(test, mtl[,c])
test <- test[!is.na(test)]
test <- as.numeric(test)
hist(test, breaks = 100)
rm(test)




#_______________________________________________________________________________
#
# Measures of novelty
#
# For the first of our MeSH-based novelty metrics all we need is, for each 
# application, some descriptive statistics of the distrubtion of time lags from
# its MeSHs' dates. Let's write a table/csv to store all that we need.
output <- data.frame(
  Applicationsourcerecordid = mtl$Application.source.record.id,
  tallyMeSH = 0,
  meanMeSHlag = NA,
  minMeSHlag = NA,
  q1MeSHlag = NA,
  medianMeSHlag = NA,
  maxMeSHlag = NA
)
pb = txtProgressBar(
  title = "Calculating summary statistics on the dates of MeSH terms",
  min = 0, max = nrow(a), initial = 0, style = 3
)
for(i in 1:nrow(mtl)) { # For every application...
  x = mtl[i, 2:ncol(mtl)] # ... find the vector containing the MeSH dates...
  if(any(!is.na(x))) { # ... and if the vector is not empty...
    x <- x[!is.na(x)] # ... then drop the missings and calculate the stats.
    
    # This is what Chai & Menon (2019) call "competition for Attention – number
    # of MeSH keywords".
    output$tallyMeSH[i] <- length(x)
    
    # This is the indicator in Azoulay et al. (2011), where novelty is
    # inversely proportional to the average MeSH term age:
    output$meanMeSHlag[i] <- mean(x)
    
    # This is what Mishra & Torvik (2016) call "minimum concept age" or 
    # "individual time novelty". Note that while they use years as unit of
    # measurement, here we have days.
    output$minMeSHlag[i] <- min(x)
    
    # And these are other obvious variants:
    output$q1MeSHlag[i] <- quantile(x, probs = 0.25)
    output$medianMeSHlag[i] <- median(x)
    output$maxMeSHlag[i] <- max(x)
    
    # Note that some authors use the recency of MeSH pairs. E.g.:
    #   - Azoulay et al. (2011) also use the average recency of MeSH pairs;
    #   - Boudreau et al. (2016) define novelty as the proportion of MeSH pairs
    #     that do not appear in prior literature.
    
  }
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb)

write.csv2(output, "./NIH MeSH/output/MeSH_timelag.csv", row.names = FALSE)




# We want to use the time lag to calculate our MeSH-based novelty metrics.
# Technically, this means transforming the time lag into a measure of recency.
#
# The first step is to define the range of recency.
# Let's say we want our novelty scores to range in [0,1], where 0 means no
# novelty at all, and 1 means maximal novelty.
# *Maximal* novelty should be recorded when the time lag is null or negative:
# i.e., when the application date coincides or precedes the date of creation of
# the MeSH terms.
# By contrast, defining *minimal* novelty is somewhat more arbitrary: we need 
# to choose what is the maximum time lag that corresponds to minimal novelty: 
#    (a) infinity;
#    (b) the maximum time lag observed in the dataset;
#    (c) an arbitrarily long time lag, e.g. 10 or 20 years.
# Here I'm going for option (a). As I will show, I will do this in such a way
# that any time lag larger than 5-10 years has almost null recency.
#
# The second step is to define a timelag-recency function.
# A linear function (e.g. recency = -1 * timelag) is trivial and intuitive, but
# it may not be our best option. After all, a difference of one year is quite 
# large when we work on brand new subjects or with brand new methods. But the
# same difference matters far less when we work on subjects or methods that have
# been around already for, say, ten years.


# Let's clear the environment again.
rm(list = ls())
gc()

d <- read.csv2("./NIH MeSH/output/MeSH_timelag.csv")

# In quite a few cases, the most recent MeSH terms were created *after* the 
# proposal submission:
table(d$minMeSHlag < 0)

# In all these cases, we assume that recency is maximal (novelty = 1)
d$meanMeSHlag[d$meanMeSHlag < 0] <- 0
d$minMeSHlag[d$minMeSHlag < 0] <- 0
d$q1MeSHlag[d$q1MeSHlag < 0] <- 0
d$medianMeSHlag[d$medianMeSHlag < 0] <- 0
d$maxMeSHlag[d$maxMeSHlag < 0] <- 0

# Then we apply a transformation on the various timelag statistics to obtain
# their corresponding measures of recency -- which we can now call "novelty".
# The exponential function is the obvious approach.
lagToRec <- \(x, s = 1000) ifelse(is.na(x), NA, exp(- x / s))

# This is what the lag-recency function looks like as a function of s:
# Plotting the lag-recency function:

viz <- data.frame(
  dist = 1:100000,
  steep = lagToRec(x = 1:100000, s = 100),
  medium = lagToRec(x = 1:100000, s = 1000),
  mild = lagToRec(x = 1:100000, s = 10000)
)
viz <- reshape2::melt(viz, id.vars = c("dist"))

labs <- data.frame(
  label = c("steep (s=100)", "medium (s=1000)", "mild (s=10000)"),
  x = c(100, 1000, 10000),
  y = c(0.5, 0.5, 0.5)
)
ggplot(data = viz) +
  geom_line(aes(x = dist, y = value, linetype = variable), size = 1) +
  xlab("time lag (days)") + ylab ("recency, aka novelty") +
  scale_x_continuous(
    trans='log10', breaks = c(1, 10, 100, 1000, 10000),
    limits = c(1, 100000), expand = c(0,0)
  ) +
  #geom_label(data = labs, aes(x = x, y = y, label = label), fill = NA, label.size	= 0) +
  geom_text(data = labs, aes(x = x, y = y, label = label), angle = 293) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,1), expand = c(0,0)) +
  theme(
    legend.position = "none",
    panel.background =  element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#808080"),
    axis.text.x = element_text(hjust = 0.8)
  )
rm(viz)

# Let's see how it would transform lags in the range "zero-days to ten-years"
# into "recency":
plot(
  0:3652, 
  lagToRec(x = 0:3652, s = 1000), 
  type = "l",
  main = "s = 1000",
  xlab = "time lag (days)",
  ylab = "recency, aka novelty"
)
# ... looks about right: at ten years the recency is almost zero, and a day
# counts much more when time lags are small and counts much less when time lags
# are long.

# Let's apply the transformation and get our MeSH-recency-based novelty metrics.
d$novelty_meanMeSHlag <- lagToRec(x = d$meanMeSHlag, s = 1000) 
d$novelty_minMeSHlag <- lagToRec(x = d$minMeSHlag, s = 1000) 
d$novelty_q1MeSHlag <- lagToRec(x = d$q1MeSHlag, s = 1000) 
d$novelty_medianMeSHlag <- lagToRec(x = d$medianMeSHlag, s = 1000) 
d$novelty_maxMeSHlag <- lagToRec(x = d$maxMeSHlag, s = 1000) 



# Lastly, a simpler approach to transform time lag into recency is to
# dichotomize the time lag: we may mark 1 for new/recent MeSH terms, and 0 for
# all others. For this we need to choose a sensible threshold.
quantile(
  x = d$minMeSHlag, 
  #x = d$q1MeSHlag,
  #d$meanMeSHlag,
  probs = c(0.5, 0.25, 0.2, 0.15, 0.1, 0.01),# Quantiles that I'm looking into
  na.rm = TRUE
) / 365.2 # convert units from days into years
table(d$minMeSHlag == 0) / sum(table(d$minMeSHlag == 0)) # Relative frequencies
table(d$minMeSHlag <= 365) / sum(table(d$minMeSHlag <= 365))
table(d$minMeSHlag <= 730) / sum(table(d$minMeSHlag <= 730))
table(d$minMeSHlag <= 1095) / sum(table(d$minMeSHlag <= 1095))
#
# Alright, after a bit of exploring it seems that sensible thresholds could be 
# the bottom 15% or 20% of time lags. Or we can set a threshold based on a time
# lag e.g. 0 days (i.e. only brand new MeSH terms count as novel), or 3 years
# (i.e. all MeSH terms up to 1095 days old count as novel).
# I'll do the latter two because they seem easier to interpret.
d$novelty_dic_minMeSHlag_0days <- as.numeric(d$minMeSHlag == 0)
d$novelty_dic_minMeSHlag_3years <- as.numeric(d$minMeSHlag <= 1095)



# Time to save:
write.csv2(d, "./NIH MeSH/output/MeSH_novelty.csv", row.names = FALSE)



