#' Locations of HCDN sites in CONUS
#'
#' @description A data frame containing the locations of the USGS Hydro-Climatic
#' Data Network site for the continental US (CONUS). These are the same sites
#' used by CAMELS (Catchment Attributes and MEteorology for Large-sample Studies).
#'
#' @format A data frame with 670 rows and 3 variables:
#' \describe{
#'   \item{hcdn_site}{HCDN site number (integer)}
#'   \item{lat}{Site latitude (decimal degrees)}
#'   \item{lon}{Site longitude (decimal degrees)}
#' }
#' @source This data set is described in Lins, H. F. (2012). USGS Hydro-climatic
#' data network 2009 (HCDN-2009). U.S. Geological Survey Fact Sheet 2012-3047.
#' Retrieved from \url{https://pubs.usgs.gov/fs/2012/3047/}.
#' The data can be downloaded at
#' \url{https://water.usgs.gov/osw/hcdn-2009/HCDN-2009_Station_Info.xlsx}.

"hcdn_conus_sites"
