#' @title  Bootstrap Analyses of Hydrological Model Error
#' @docType package
#' @name gumboot-package
#'
#' @description
#' gumboot is an R package which does jackknife after bootstrap analyses of the error in hydrological models by
#' estimating the empirical probability distributions of NSE (Nash-Sutcliffe efficiency) and KGE (Kling-Gupta efficiency)
#' estimators.
#'
#' @author \strong{Coded by:} Kevin Shook  and...
#' @author \strong{Conceptual design by:} Martyn Clark
#' @author \strong{Maintained by:} Guoqiang Tang \email{guoqiang.tang@usask.ca}
#'
#' @section Funding:
#'
#' The package was partly funded by the Global institute for Water Security (GIWS; \href{https://water.usask.ca/}{https://water.usask.ca/}) and the Global Water Futures (GWF; \href{https://gwf.usask.ca/}{https://gwf.usask.ca/}) program.
#' @references
#'  The package code is described in: \cr
#'  \cite{Clark, M.P, et al. 2020. "The abuse of popular performance metrics in hydrologic modeling"}
#'
#' @import dplyr hydroGOF ggplot2 lubridate stringr ncdf4
NULL

