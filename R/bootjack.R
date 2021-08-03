#' Bootstrap-jacknife of flow calibration statistics
#'
#' @param flows Required. Data frame containing the date, observed and simulated
#' flows. The variable names must be \option{date}, \option{obs}, and \option{sim},
#' respectively. The \code{date} must be a standard \R date.
#' @param GOF_stat Required. Name(s) of simulation goodness of fit statistic(s)
#' to be calculated. Currently both \code{NSE} and \code{KGE} are supported.
#' @param nSample Required. Number of samples for bootstrapping.
#' @param waterYearMonth Required. Month of beginning of water year. Default
#' is \code{10} (October). If the calendar year is required, set
#' \code{waterYearMonth = 13}.
#' @param startYear Optional. First year of data to be used. If \code{NULL}
#' then not used.
#' @param endYear Optional. Last year of data to be used. If \code{NULL} then
#' not used.
#' @param minDays Required. Minimum number of days per year with valid
#' (i.e. greater than 0) flows. Default is 100.
#' @param minYears Required. Minimum number years to be used. Default is 10.
#' @param returnSamples Optional. Default is \code{FALSE}. If \code{TRUE}, then
#' sample statistics are returned. This is primarily used for debugging/testing.
#' @param seed Optional. If \code{NULL} (the default) then no seed is specified
#' for the random number generator used for the bootstrapping. If a value is specified
#' then the bootstrapping will always use the same set of pseudo-random numbers.
#' @param bootYearFile Optional. If \code{NULL} (the default) the years used for
#' the bootstrapping are neither output nor input. If a file is specified, and it
#' it does not already exist, then the bootstrap years will be written to a .csv
#' file as a table with the dimensions of years x nSample. If a file is specified,
#' and it _does_ exist, then the years will be read in, and used for the bootstrapping.
#' @return Returns a data frame containing the goodness of fit statistic name
#' (i.e. \option{NSE} and/or \option{KGE}), and \code{seJack} = standard error of
#' jacknife, \code{seBoot} = standard error of bootstrap, \code{p05, p50, p95},
#' the 5th, 50th and 95th percentiles of the estimates, \code{score} = jackknife
#' score, \code{biasJack} = bias of jackknife, \code{biasBoot} = bias of bootstap,
#' \code{seJab} = standard error of jackknife after bootstrap.
#'
#' @author Martyn Clark and Kevin Shook
#' @seealso \code{\link{read_CAMELS}}
#' @export
#' @importFrom stats cor median quantile runif sd var
#' @import dplyr
#' @import hydroGOF
#' @importFrom stringr str_detect
#' @importFrom utils read.csv write.table
#'
#' @examples
#' NSE_stats <- bootjack(flows_1030500, "NSE")
bootjack <- function(flows,
                     GOF_stat = c("NSE", "KGE"),
                     nSample = 1000,
                     waterYearMonth = 10,
                     startYear = NULL,
                     endYear = NULL,
                     minDays = 100,
                     minYears = 10,
                     returnSamples = FALSE,
                     seed = NULL,
                     bootYearFile = NULL) {

  # check parameter values

  if (sum(str_detect(string = GOF_stat, "KGE")) > 0)
    KGE_is_present <- TRUE
  else
    KGE_is_present <- FALSE

  if (sum(str_detect(string = GOF_stat, "NSE")) > 0)
    NSE_is_present <- TRUE
  else
    NSE_is_present <- FALSE

  # turn off dplyr message
  options(dplyr.summarise.inform = F)

  # set random seed, if provided
  if (!is.null(seed))
    set.seed(seed)

  # check for files to read and write selected years
  write_bootyears <- FALSE
  read_bootyears <- FALSE
  if (!is.null(bootYearFile)) {
    if (!file.exists(bootYearFile))
      write_bootyears <- TRUE
    else
      read_bootyears <- TRUE
  }


  flows$year <- as.numeric(format(flows$date, format = "%Y"))
  flows$month <- as.numeric(format(flows$date, format = "%m"))
  flows$day <- as.numeric(format(flows$date, format = "%d"))
  iyUnique <- unique(flows$year)
  nTrials <- length(endYear)

  # define the water years
  flows$iyWater <- ifelse(flows$month >= waterYearMonth, flows$year + 1,
                          flows$year)
  iyWater <- ifelse(flows$month >= waterYearMonth, flows$year + 1, flows$year)
  nYears <- length(unique(iyWater))

  # define stats data frames
  statsJack  <- data.frame("meanSim" = NA_real_,
                           "meanObs" = NA_real_,
                           "varSim" = NA_real_,
                           "varObs" = NA_real_,
                           "rProd" = NA_real_)
  statsBoot  <- data.frame("meanSim" = NA_real_,
                           "meanObs" = NA_real_,
                           "varSim" = NA_real_,
                           "varObs" = NA_real_,
                           "rProd" = NA_real_)
  if (NSE_is_present) {
    statsJack$NSE <- NA_real_
    statsBoot$NSE <- NA_real_
  }

  if (KGE_is_present) {
    statsJack$KGE <- NA_real_
    statsBoot$KGE <- NA_real_
  }

  # define year arrays
  yearsJack  <- array(-9999, c(nYears))
  yearsBoot  <- array(-9999, c(nYears, nSample))

  #----------------------
  #  Check data validity
  #----------------------

  # get pointer to valid values
  zeroVal <- -1e-10  # allow for zero values
  ixValid <- which((flows$obs > zeroVal) & (flows$sim > zeroVal))

  # get the number of days in each year
  good_flows <- flows[ixValid,]
  valid_days <- good_flows %>% group_by(iyWater) %>%
    summarise(good_days = n_distinct(date))


  # restrict attention to the water years with sufficient data
  valid_years <- valid_days[valid_days$good_days > minDays,]

  if (!is.null(startYear)) {
    valid_years <- valid_years[valid_years$iyWater >= startYear,]
  }

  if (!is.null(endYear)) {
    valid_years <- valid_years[valid_years$iyWater <= endYear,]
  }

  nyValid <- nrow(valid_years)

  if (nyValid < minYears) {
    errorStats <- data.frame("GOF_stat" = "","seJack" = NA_real_,
                             "seBoot" = NA_real_, "p05" = NA_real_,
                             "p50" = NA_real_, "p95" = NA_real_,
                             "score" = NA_real_,
                             "biasJack" = NA_real_, "biasBoot" = NA_real_,
                             "seJab" = NA_real_)
    return(errorStats)
  }

  izUnique <- valid_years$iyWater

 # set up sampling strategies and number of samples
 samplingStrategy      <- c('jack','boot')
 n_sampling_strategies <- length(samplingStrategy)
 mSample               <- c(nyValid + 1, nSample + 1)


  if (read_bootyears)
    bootYears <- read.csv(bootYearFile, header = FALSE)


# loop through sampling strategies
  for (iStrategy in 1:n_sampling_strategies) {
      # loop through samples
    for (iSample in 1:mSample[iStrategy]) {
      # -----
      # ----- get the data subset (resample)...
      # ---------------------------------------

      # base case: use all days
      if (iSample == 1) {
        jxValid <- ixValid
        # resample years
      } else {
        if (samplingStrategy[iStrategy] == 'jack') {
          # ***** jackknife
          # save the years
          yearsJack[iSample - 1] = izUnique[iSample - 1]
          # get the desired indices
          iyIndex <-  which(iyWater[ixValid] != izUnique[iSample - 1])
          jxValid <- ixValid[iyIndex]

        } else {
          # ***** bootstrap
          # get a random selection of years
          if (!read_bootyears) {
            uRand   <- runif(nyValid) # uniform random number
            ixYear  <- floor(uRand*nyValid) + 1
            iyYear  <- izUnique[ixYear]
          } else {
            iyYear  <- bootYears[, iSample - 1 ]
          }

          # save the years
          yearsBoot[1:nyValid, iSample - 1] <- iyYear
          iyIndex <- which(iyWater[ixValid] == iyYear[1])

          for (iYear in 2:nyValid)
            iyIndex <- c(iyIndex, which(iyWater[ixValid] == iyYear[iYear]))

          jxValid <- ixValid[iyIndex]

      }  # jackknife/bootstrap
    }  # re-sampling years

    # get valid data
    qSimValid <- flows$sim[jxValid]
    qObsValid <- flows$obs[jxValid]

    # get the water year
    wyValid   = iyWater[jxValid]


    # -----
    # ----- compute efficiency scores (annual)...
    # -------------------------------------------

    # get the product moment statistics
    meanSim <- mean(qSimValid, na.rm = TRUE)
    meanObs <- mean(qObsValid, na.rm = TRUE)
    varSim <- var(qSimValid, na.rm = TRUE)
    varObs <- var(qObsValid, na.rm = TRUE)
    rProd <- cor(qSimValid, qObsValid)


    # get stats
    if (samplingStrategy[iStrategy] == 'jack') {
      statsJack[iSample,1:5] <- c(meanSim, meanObs, varSim, varObs, rProd)
    }else if (samplingStrategy[iStrategy] == 'boot') {
      statsBoot[iSample,1:5] <- c(meanSim, meanObs, varSim, varObs, rProd)
    }
    if (NSE_is_present) {
      xBeta = meanSim/meanObs
      yBeta = (meanObs - meanSim)/sqrt(varObs)
      alpha = sqrt(varSim)/sqrt(varObs)
      nse   = 2*alpha*rProd - yBeta^2 - alpha^2
      #nse <- NSE(qSimValid, qObsValid)

      # save statistics
      if (samplingStrategy[iStrategy] == 'jack')
        statsJack$NSE[iSample] <- nse

      if (samplingStrategy[iStrategy] == 'boot')
        statsBoot$NSE[iSample] <- nse
    }

    if (KGE_is_present) {
      xBeta <-  meanSim/meanObs
      yBeta <-  (meanObs - meanSim)/sqrt(varObs)
      alpha <-  sqrt(varSim)/sqrt(varObs)
      kge <- 1 - sqrt( (xBeta - 1)^2 + (alpha - 1)^2 + (rProd - 1)^2)
     # kge <- KGE(qSimValid, qObsValid)

      # save statistics
      if (samplingStrategy[iStrategy] == 'jack')
        statsJack$KGE[iSample] <- kge

      if (samplingStrategy[iStrategy] == 'boot')
        statsBoot$KGE[iSample] <- kge
    }

   }   # looping through sampling strategies
  }


 # write boot years
 if (write_bootyears & (samplingStrategy[iStrategy] == "boot")) {
   write.table(yearsBoot,
               file = bootYearFile, row.names = FALSE,
               col.names = FALSE, sep = ",")
 }

 if (returnSamples) {
   return_vals <- list(statsBoot = statsBoot, statsJack = statsJack)
   return(return_vals)
 }


# now get error stats
 errorStats <- data.frame("GOF_stat" = "",
                          "seJack" = NA_real_,
                          "seBoot" = NA_real_,
                          "p05" = NA_real_,
                          "p50" = NA_real_,
                          "p95" = NA_real_,
                          "score" = NA_real_,
                          "biasJack" = NA_real_,
                          "biasBoot" = NA_real_,
                          "seJab" = NA_real_)


 # check for missing values (< -9998)
 if (any(statsJack <= -9998))
   return(errorStats)


 # loop through stat types
 numstats <- length(GOF_stat)
 colnames <- names(statsJack)

 if (KGE_is_present)
   kge_col <- which(colnames == "KGE")

 if (NSE_is_present)
   nse_col <- which(colnames == "NSE")

 for (iPlot in 1:numstats) {
   # get the data
   if (GOF_stat[iPlot] == "NSE")
     ixPos <- nse_col

   if (GOF_stat[iPlot] == "KGE")
     ixPos <- kge_col

   # get the data
   xJack <- statsJack[, ixPos]
   xBoot <- statsBoot[, ixPos]

   # rank the data
   iSort <- order(xJack)

   # extract the score
   score  <- xJack[1]

   # extract the Jackknife and bootstrap estimates
   zJack  <- xJack[2:(nYears + 1)]
   zBoot  <- xBoot[2:(nSample + 1)]

   # get the valid samples
   ixJack <- which(zJack > -9998 & (!is.na(zJack)))
   nJack <- length(ixJack)

   # get the mean of all Jackknife samples
   jackMean <- mean(zJack, na.rm = TRUE)

   # get the jackknife estimates
   jackScore <- (nJack * score) - (nJack - 1) * jackMean
   sumSqErr  <- (nJack - 1) * sum((jackMean - zJack[ixJack])^2)
   seJack    <- sqrt(sumSqErr / nJack)  # standard error of the Jackknife estimate (==. 22)

   # get the bootstrap estimates
   ySample   <- zBoot[order(zBoot)]
   seBoot    <- sd(zBoot)  # ==. 23
#   p05       <- quantile(ySample, 0.05, na.rm = TRUE, type = 3)
#   p50       <- median(ySample, na.rm = TRUE)
#   p95       <- quantile(ySample, 0.95, na.rm = TRUE, type = 3)

    p05 <- ySample[floor(0.05 * nSample) + 1]
    p50 <- ySample[floor(0.5 * nSample) + 1]
    p95 <- ySample[floor(0.95 * nSample) + 1]


   # get the bias
   biasJack  <- (nJack - 1) * (jackMean - score)
   biasBoot  <- mean(zBoot) - score

   # **** get the standard error of the confidence intervals
   jabData   <- vector("numeric", nYears)            # JAB (Jackknife-After-Bootstrap)

   for (iYear in 2:(nYears + 1)) {
     # get the number of times iyUnique[iYear] is used in each sample
     matchYear <- vector("integer", nSample)
     for (iSample in 1:nSample) {
       ixMatch <- which(yearsBoot[,iSample] == iyUnique[iYear]) #eqs. 24,25
       nMatch <- length(ixMatch)
       matchYear[iSample] <- nMatch
     } # looping through samples

     # compute statistics where the year is excluded
     ixMissing <- which(matchYear == 0) # indices of the bootstrap samples when a given year is missing
     nMissing <- length(ixMissing)
     xSample   <- zBoot[ixMissing]
     ySample   <- xSample[order(xSample)]
     p05jack_R   <- quantile(xSample, 0.05, type = 3)
     p95jack_R   <- quantile(xSample, 0.95, type = 3)
     p05jack <- ySample[floor(0.05 * nMissing) + 1]
     p95jack <- ySample[floor(0.95 * nMissing) + 1]

     jabData[iYear - 1] <- p95jack - p05jack          # data used in the jackknife
   }  # looping through years

   # get the jackknife estimates
   jabMean   <- mean(jabData)
   sumSqErr  <- (nYears - 1)*sum((jabMean - jabData)^2)
   seJab     <- sqrt(sumSqErr/nYears) # standard error of the bootstrap estimate
   # save errors
   errorStats[iPlot,] <- c(GOF_stat[iPlot], seJack, seBoot, p05, p50, p95, score, biasJack, biasBoot, seJab)

}  # looping through plots

 errorStats$seJack <- as.numeric(errorStats$seJack)
 errorStats$seBoot <- as.numeric(errorStats$seBoot)
 errorStats$p05 <- as.numeric(errorStats$p05)
 errorStats$p50 <- as.numeric(errorStats$p50)
 errorStats$p95 <- as.numeric(errorStats$p95)
 errorStats$score <- as.numeric(errorStats$score)
 errorStats$biasJack <- as.numeric(errorStats$biasJack)
 errorStats$biasBoot <- as.numeric(errorStats$biasBoot)
 errorStats$seJab <- as.numeric(errorStats$seJab)

 return(errorStats)
}
