#' Bootstrap-jacknife of flow calibration statistics
#'
#' @param flows Required. Data frame of date, observed and simulated flows.
#' @param stats Required. Vector of goodness of fit statistics to be used.
#' @param nSampls Required. Number of samples for bootstrapping.
#' @param waterYearMonth Required. Month of beginning of water year. Default is \code{10}
#' (October). If the calendar year is required, set \code{waterYearMonth = 13}.
#' @param startYear Optional. First year of data to be used. If \code{NULL} then not used.
#' @param endYear Optional. Last year of data to be used. If \code{NULL} then not used.
#' @param minYears Required. Minimum number years to be used. Default is 10.
#'
#' @return Returns a list containing 1) \code{statsBoot} (array of bootstrap stats), and
#' 2) \code{statsJack} (array of jackknife stats).
#' @author Kevin Shook
#' @seealso \code{\link{read_hcdn}}
#' @export
#' @import dplyr
#' @import hydroGOF
#'
#' @examples \dontrun{
#' NSE_stats <- bootjack(flows)}
bootjack <- function(flows,
                     stats = "nse",
                     nSample = 1000,
                     waterYearMonth = 10,
                     startYear = NULL,
                     endYear = NULL,
                     minYears = 10) {

  flows$year <- as.numeric(format(flows$date, format = "%Y"))
  flows$month <- as.numeric(format(flows$date, format = "%m"))
  flows$day <- as.numeric(format(flows$date, format = "%d"))
  years <- unique(flows$year)
  nTrials <- length(endYear)

  # define the water years
  flows$iyWater <- ifelse(flows$month >= waterYearMonth, flows$year + 1, flows$year)
  iyWater <- ifelse(flows$month >= waterYearMonth, flows$year + 1, flows$year)
  nYears <- length(unique(iyWater))

  # get number of calibration statistics
  nTarget  <- length(stats)

  # define stats
  statsJack  <- array(-9999, c(7, nYears, nTarget))
  statsBoot  <- array(-9999, c(7, nSample, nTarget))

  # define years
  yearsJack  <- array(-9999, c(nYears, nTarget))
  yearsBoot  <- array(-9999, c(nYears, nSample, nTarget))

  #----------------------
  #  Check data validity
  #----------------------

  # get pointer to valid values
  zeroVal <- -1e-10  # allow for zero values
 # flows$ixValid <- ((flows$obs > zeroVal) & (flows$sim > zeroVal))
  ixValid <- which((flows$obs > zeroVal) & (flows$sim > zeroVal))

  # get the number of days in each year
  good_flows <- flows[flows$ixValid,]
  valid_days <- good_flows %>% group_by(iyWater) %>% summarise(good_days = n_distinct(date))


  # restrict attention to the water years with sufficient data
  valid_years <- valid_days[valid_days$good_days > 100,]

  if (!is.null(startYear)){
    valid_years <- valid_years[valid_years$iyWater >= startYear,]
  }

  if (!is.null(endYear)){
    valid_years <- valid_years[valid_years$iyWater <= endYear,]
  }

  nyValid <- nrow(valid_years)

  if (nyValid < minYears) {
      stop("Too few years of valid data")
  }

  izUnique <- valid_years$iyWater

 # set up sampling strategies and number of samples
 samplingStrategy      <- c('jack','boot')
 n_sampling_strategies <- length(samplingStrategy)
 mSample               <- c(nyValid, nSample)

  # loop through calibration targets i.e. NSE or KGE etc.
  for (iTarget in 1:nTarget) {

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
            yearsJack[iSample, iTarget] = izUnique[iSample]

            # get the desired indices
            iyIndex <-  which(iyWater[ixValid] != izUnique[iSample])
            jxValid <- ixValid[iyIndex]

          } else {
            # ***** bootstrap
            # get a random selection of years
            uRand   <- runif(nyValid)                  # uniform random number
            ixYear  <- floor(uRand*nyValid - 1e-5) + 1
            iyYear  <- izUnique[ixYear]

            # save the years
            yearsBoot[1:nyValid, iSample, iTarget] <- iyYear
            iyIndex <- which(iyWater[ixValid] == iyYear[1])

            for (iYear in 2:nyValid)
              iyIndex <- c(iyIndex, which(iyWater[ixValid] == iyYear[iYear]))

            jxValid <- ixValid[iyIndex]
          }  # jackknife/bootstrap
       }  # re-sampling years

      # get valid data
      qSimValid <- flows$sim[jxValid]
      qObsValid <- flows$obs[jxValid]

      # -----
      # ----- compute efficiency scores (annual)...
      # -------------------------------------------

      # get the product moment statistics
      meanSim <- mean(qSimValid)
      meanObs <- mean(qObsValid)
      varSim <- var(qSimValid)
      varObs <- var(qObsValid)
      rProd <- cor(qSimValid, qObsValid)

      # get stats
      nse <- NSE(qSimValid, qObsValid)
      kge <- KGE(qSimValid, qObsValid)

      # save statistics
      if (samplingStrategy[iStrategy] == 'jack')
        statsJack[,iSample,iTarget] <- c(meanSim, meanObs, varSim, varObs, rProd, nse, kge)

      if (samplingStrategy[iStrategy] == 'boot')
        statsBoot[,iSample,iTarget] <- c(meanSim, meanObs, varSim, varObs, rProd, nse, kge)

    }   # looping through sampling strategies
  } # target loop
 } # return values
 return_list <- list(statsJack = statsJack, statsBoot = statsBoot)

}
