#' @title  Bootstrap Analyses of Hydrological Model Error
#' @docType package
#' @name bootstrappR-packag
#'
#' @description
#' bootstrappR is an R package which does jackknife after bootstrap analyses of the error in hydrological models by
#' estimating the empirical probability distributions of NSE (Nash-Sutcliffe efficiency) and KGE (Kling-Gupta efficiency)
#' estimators.
#'
#' @author \strong{Coded by:} Kevin Shook  and...
#' @author \strong{Conceptual design by:} Martyn Clark
#' @author \strong{Maintained by:} Guoqiang Tang \email{guoqiang.tang@usask.ca}
#'
#' @section Funding:
#'
#' The package was partly funded by the Global institute for Water Security (GIWS; \href{https://www.usask.ca/water/}{https://www.usask.ca/water/}) and the Global Water Futures (GWF; \href{https://gwf.usask.ca/}{https://gwf.usask.ca/}) program.
#' @references
#'  The package is described in: \cr
#'  \cite{Clark, M.P, et al. 2020. "Evaluation of global performance metrics habitually used in hydrological modelling studies: A critique and path forward"}
#'
#' @import dplyr hydroGOF ggplot2 lubridate stringr ncdf4
NULL

