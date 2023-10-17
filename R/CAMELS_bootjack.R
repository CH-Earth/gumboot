#' Jackknife after bootstrap for all CAMELS sites
#'
#' @description Hydrologic model simulations can be produced using input-response data
#' from the 671 catchments in the CAMELS dataset (Catchment Attributes and MEteorology
#' for Large-sample Studies). Newman et al. (2015) and Addor et al. (2017)
#' provide details on the hydrometeorological and physiographical characteristics of
#' the CAMELS catchments. The CAMELS catchments are those with minimal human disturbance
#' (i.e., minimal land use changes or disturbances, minimal water withdrawals), and are
#' hence almost exclusively smaller, headwater-type catchments (median basin size of 336 km^2^).
#' The CAMELS data used for the large-domain model simulations are publicly available at the
#' National Center for Atmospheric Research at \url{https://ral.ucar.edu/solutions/products/camels}.
#'
#' @param CAMELS_sites Required. Data frame of CAMELS sites. Must contain a field called \option{hcdn_site}. The data frame
#' \code{hcdn_conus_sites} will work. You can subset this data frame if you want to use fewer sites.
#' @param NetCDF_file Required. NetCDF file containing CAMELS modelled and gauged flows.
#' @param sim_var Required. Name of variable containing simulated flows in \code{NetCDF}.
#' @param GOF_stat Required. Name(s) of simulation goodness of fit statistic(s) to be calculated. Currently both \code{NSE} and \code{KGE} are supported.
#' @param nSample Required. Number of samples for bootstrapping.
#' @param waterYearMonth Required. Month of beginning of water year. Default is \code{10}
#' (October). If the calendar year is required, set \code{waterYearMonth = 13}.
#' @param startYear Optional. First year of data to be used. If \code{NULL} then not used.
#' @param endYear Optional. Last year of data to be used. If \code{NULL} then not used.
#' @param minDays Required. Minimum number of days per year with valid (i.e. greater than 0) flows. Default is 100.
#' @param minYears Required. Minimum number years to be used. Default is 10.
#' @param seed Optional. If \code{NULL} (the default) then no seed is specified
#' for the random number generator used for the bootstrapping. If a value is specified
#' then the bootstrapping will always use the same set of pseudo-random numbers.
#' @param bootYearFile Optional. If \code{NULL} (the default) the years used for
#' the bootstrapping are neither output nor input. If a file is specified, and it
#' it does not already exist, then the bootstrap years will be written to a .csv
#' file as a table with the dimensions of years x nSample. If a file is specified,
#' and it _does_ exist, then the years will be read in, and used for the bootstrapping.
#' @param quiet Optional. If \code{FALSE} (the default) a progress bar is displayed. If \code{TRUE},
#' it is not.
#'
#' @return Returns a data frame containing the following variables:
#' \describe{
#'  \item{\code{CAMELS_site}}{CAMELS site number}
#'  \item{\code{lat}}{CAMELS site latitude}
#'  \item{\code{lon}}{CAMELS site longitude}
#'  \item{\code{GOF_stat}}{Goodness of fit statistics (i.e. NSE or KGE)}
#'  \item{\code{seJack}}{standard error of jacknife}
#'  \item{\code{seBoot}}{standard error of bootstrap}
#'  \item{\code{p05, p50, p95}}{the 5th, 50th and 95th percentiles of the estimates}
#'  \item{\code{score}}{the jackknife score}
#'  \item{\code{biasJack}}{the bias of the jackknife}
#'  \item{\code{biasBoot}}{the bias of the bootstrap}
#'  \item{\code{seJab}}{the standard error of the jackknife after bootstrap}
#' }
#' @author Martyn Clark and Kevin Shook
#' @seealso \code{\link{read_CAMELS}}
#' @export
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @references N. Addor, A. Newman, M. Mizukami, and M. P. Clark, 2017. Catchment attributes
#' for large-sample studies. Boulder, CO: UCAR/NCAR. \doi{10.5065/D6G73C3Q}
#' @references Addor, N., Newman, A. J., Mizukami, N. and Clark, M. P.: The CAMELS data set:
#' catchment attributes and meteorology for large-sample studies, Hydrol. Earth Syst. Sci., 21,
#' 5293–5313, \doi{10.5194/hess-21-5293-2017}, 2017.
#'
#' @examples \dontrun{
#' camels <- CAMELS_bootjack(CAMELS_sites = sites, NetCDF_file = "CAMELS_flow.nc")
#' }
#'
CAMELS_bootjack <- function(CAMELS_sites = NULL,
                                NetCDF_file = NULL,
                                sim_var = "kge",
                                GOF_stat = c("NSE", "KGE"),
                                nSample = 1000,
                                waterYearMonth = 10,
                                startYear = NULL,
                                endYear = NULL,
                                minDays = 100,
                                minYears = 10,
                                seed = NULL,
                                bootYearFile=NULL,
                                quiet = FALSE) {
  # check parameters
  if (is.null(NetCDF_file)) {
    stop("NetCDF file containing flows is required")
  }

  if (is.null(CAMELS_sites)) {
    stop("CAMELS site data frame is required")
  }

  # loop through sites

  num_sites <- nrow(CAMELS_sites)
  if (!quiet)
    pb <- txtProgressBar(min = 1, max = num_sites, style = 3)

  for (i in 1:num_sites) {
    if (!quiet)
      setTxtProgressBar(pb, i)

    # get data
    CAMELS_site <- CAMELS_sites$hcdn_site[i]
    CAMELS_values <- read_CAMELS(NetCDF_file, CAMELS_site, obsName = "obs", simName = sim_var)
    # do JAB
    jab <- bootjack(CAMELS_values, GOF_stat, nSample, waterYearMonth, startYear,
                    endYear, minDays, minYears, returnSamples = FALSE, seed = seed,
                    bootYearFile = bootYearFile)
    jab$CAMELS_site <- CAMELS_site
    jab$lat <- CAMELS_sites$lat[i]
    jab$lon <- CAMELS_sites$lon[i]
    if (i == 1) {
      all_jab <- jab
    } else {
      all_jab <- rbind(all_jab, jab)
    }
    rm(jab)
    rm(CAMELS_values)
  }

  # rearrange columns
  all_jab <- all_jab[, c(11, 12, 13, 1:10)]
  return(all_jab)
}
