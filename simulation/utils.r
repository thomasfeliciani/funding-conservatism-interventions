# This script contains some support functions used in the simulation.





# Outcome measures ______________________________________________________________
#
# Adapted from "Muser" (November 2013), Stackoverflow. Url:
# https://stackoverflow.com/questions/20224871/kendall-tau-distance-a-k-a-bubble-sort-distance-between-permutations-in-base-r/20224872#20224872
# Last accessed on November 30, 2020.
kendallTauDistance <- function(x,y){
  if(length(x) != length(y)) { stop(
    "Function kendallTauDistance was fed vectors of unequal length.")}
  if(any(is.na( c(x,y)))) { warning(
    "Function kendallTauDistance was fed vectors with some NA's.")}
  
  mergeSort <- function(x){
    if(length(x) == 1){
      inv <- 0
    } else {
      n <- length(x)
      n1 <- ceiling(n/2)
      n2 <- n - n1
      y1 <- mergeSort(x[1:n1])
      y2 <- mergeSort(x[n1 + 1:n2])
      inv <- y1$inversions + y2$inversions
      x1 <- y1$sortedVector
      x2 <- y2$sortedVector
      i1 <- 1
      i2 <- 1
      while(i1 + i2 <= n1 + n2 + 1){
        if(i2 > n2 || (i1 <= n1 && x1[i1] <= x2[i2])){
          x[i1 + i2 - 1] <- x1[i1]
          i1 <- i1 + 1
        } else {
          inv <- inv + n1 + 1 - i1
          x[i1 + i2 - 1] <- x2[i2]
          i2 <- i2 + 1
        }
      }
    }
    return (list(inversions=inv,sortedVector=x))
  }
  inversionNumber <- function(x){
    r <- mergeSort(x)
    return (r$inversions)
  }
  distance <- inversionNumber(order(x)[rank(y)])
  return(list(
    distance = distance,
    normalized = distance / (length(x) * (length(x) -1)) * 2
  ))
}


# This is a function that takes as input the raw score data for this call
# and outputs a correctly-formatted score matrix (reviewers as columns,
# applications as rows)
buildGradeMatrix <- function(x, rr, noveltyIndicators) {
  xx <- aggregate(
    x = x,
    by = Assessmentscore ~ Applicationsourcerecordid + Personsourcerecordid,
    FUN = \(x) if(length(x) > 1) mean(as.numeric(x), na.rm = TRUE) else x
  )
  xx <- reshape2::dcast(
    data = xx,
    formula = Applicationsourcerecordid ~ Personsourcerecordid,
    value.var = "Assessmentscore"#,
    #fun.aggregate = \(x) {
    #  if(length(x) > 1) mean(as.numeric(x), na.rm = TRUE) else x
    #}
  )
  scoreIndex <- 2:ncol(xx)
  
  # Transforming scores into the correct class (numeric)
  for (col in scoreIndex) xx[,col] <- as.numeric(xx[,col])
  
  # Recoding actual funding status into a dummy:
  xx <- merge(
    x = xx, 
    y = rr[,c(
      "Applicationsourcerecordid",
      "Applicationstatus",
      noveltyIndicators
    )]
  )
  xx$deliberation_c <- xx$Applicationstatus %in% c(
    "Granted", "In progress", "Withdrawn"
  )
  xx$Applicationstatus <- NULL
  
  return(xx)
}


# Miscellanea __________________________________________________________________
gini <- \(x) {
  if (any(is.na(x))) {
    x <- na.omit(x)
    warning("NAs are ignored in the calculation of the Gini coefficient.")
  }
  x <- x |> as.numeric() |> sort()
  if (length(x) <= 1) return(NA) ###
  if (all(x == 0)) return(0) ###
  n <- length(x)
  G <- sum(x * 1L:n)
  G <- 2 * G / sum(x) - (n + 1L)
  return(G / (n - 1L))
}

# Herfindahl–Hirschman index
HHI <- \(x, tolerance = sqrt(.Machine$double.eps)) {
  if(sum(x) == 100) x <- x / 100 # If percentages were provided, transform.
  
  # Checking if the numbers provided add up to a whole. Note that we need some
  # tolerance to compensate for floating point errors. By default, we base this
  # tolerance on the numeric precision of R.
  if(abs(1 - sum(x)) >= tolerance) stop(paste(
    "Can't calculate the Herfindahl–Hirschman index:\n",
    "Proportions do not add up to 1."
  ))
  
  return(sum(x ** 2))
}

truncate <- function(x, min = 0, max = 1){
  if (length(x) > 1) return(sapply(x, truncate, min = min, max = max))
  if (is.na(x)) return(NA)
  ifelse(
    x < min,
    return(min),
    ifelse(
      x > max,
      return(max),
      return(x)
    )
  )
}

normalize <- \(x) (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))

