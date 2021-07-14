#' Reads simulated and observed values from HCDN netcdf file for a single location
#'
#' @param nc_file Required. netCDF file to read from.
#' @param hcdn Required. Location to extract data.
#' @param obsName Required. Name for variable containing observations. Default is "obs".
#' @param simName Required. Name for variable containing simulations. Default is "kge".
#'
#' @return Returns a data frame containing the date, observed and simulated flows. The name of the
#' observed flow variable is \code{obs}, the name of the simulated flow variable is \code{sim}.
#' @author Kevin Shook
#' @seealso \code{\link{conus_hcdn_bootjack}}
#' @export
#' @import ncdf4
#' @import stringr
#'
#' @examples \dontrun{
#' flows <- read_hcdn(nc_file = "results_hcdn_flow.nc", hcdn = 1030500)
#' }
read_hcdn <- function(nc_file, hcdn, obsName = "obs", simName = "kge") {

  # check parameters
  if (is.null(nc_file)) {
    stop("NetCDF file containing flows is required")
  }

  if (is.null(hcdn)) {
    stop("location parameter is required")
  }

  # define the netcdf
  timeName <- 'time'  # name of the time dimension
  hcdnName <- 'hcdn'  # name of the spatial dimension

  # open up netCDF
  nc <- nc_open(nc_file, write = FALSE)

  # get the variable attributes
  hcdn_att <- ncatt_get(nc, hcdnName)
  obs_att  <- ncatt_get(nc, obsName)

  # get times
  times <- ncvar_get(nc, timeName)
  nTime <- length(times)

  # get time units
  time_units_att <- ncatt_get(nc, timeName, "units")

  if (!time_units_att[[1]])
    stop("Time units missing")

  time_bits <- str_split_fixed(time_units_att[[2]], " ", 4)

  if (time_bits[1,1] != "days")
    stop("NetCDF file not daily - can't read")

  origin <- time_bits[1, 3]
  date <- as.Date(times, origin)

  # get hcdn values
  hcdn_values <- ncvar_get(nc, hcdnName)

  # find specified location
  hcdn_loc <- which(hcdn_values == hcdn)

  # get obs values for specified location
  start <- c(time = 1, hcdn = hcdn_loc)
  count <- c(time = nTime, hcdn = 1)
  obs <- ncvar_get(nc, obsName, start = start, count = count, raw_datavals=FALSE )

  # get simulated values
  sim <- ncvar_get(nc, simName, start = start, count = count, raw_datavals=FALSE )

  # close NetCDF
  nc_close(nc)

  output <- data.frame(date, obs, sim)

}
