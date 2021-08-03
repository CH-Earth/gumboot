#' Plots uncertainties in model error estimates
#'
#' @param JAB_stats Required. Data frame of jackknife after boot statistics for a large number
#' of model runs, as produced by \code{CAMELS_bootjack}.
#' @param fill_colour Optional. If \code{NULL} (the default), then all data series are plotted as lines.
#' If specified, e.g.\code{fill_colour = "orange"}, the plot of 2 x the Jackknife estimate of the
#' standard error will be filled with the specified colour.
#' @return Returns a \code{ggplot2} object of the plots, faceted by goodness of fit statistic, i.e. NSE/KGE.
#' The confidence interval (difference between the 95^th^ and 5^th^ quantiles, and the value of
#' 2 x the Bootstrap estimate of the standard error are plotted as lines. The values of
#' 2 x the Jackknife estimate of the standard error are plotted as filled)
#' @author Martyn Clark and Kevin Shook
#' @seealso \code{\link{CAMELS_bootjack}}
#' @export
#' @import ggplot2
#' @import reshape2

#'
#' @examples \dontrun{ p <- ggplot_estimate_uncertainties(all_stats, "orange")
#' }
ggplot_estimate_uncertainties <- function(JAB_stats, fill_colour = NULL) {

  # declare ggplot variables
  rank <- NULL
  conf_int <- NULL
  se2_jack <- NULL
  se2_boot <- NULL
  GOF_stat <- NULL
  value <- NULL
  variable <- NULL

  # get number of GOF stats
  GOF_stats <- unique(JAB_stats$GOF_stat)
  num_stats <- length(GOF_stats)

  for (i in 1:num_stats) {
    GOF <- GOF_stats[i]
    selected_stats <- JAB_stats[JAB_stats$GOF_stat == GOF,]

    # rank
    selected_stats_ranked <- selected_stats[order(selected_stats$seJab),]

    # add rank
    num_selected <- nrow(selected_stats)
    selected_stats_ranked$rank <- seq(from = 1, to = num_selected)

    if (i == 1) {
      all_stats <- selected_stats_ranked
    } else {
      all_stats <- rbind(all_stats, selected_stats_ranked)
    }
  }

  # calculate stats for plotting

  # get confidence intervals
  all_stats$conf_int <- all_stats$p95 - all_stats$p05

  # get 2 x standard error estimates from bootstrapping
  all_stats$se2_boot <- all_stats$seBoot * 2

  # get 2 x the Jackknife estimate of the standard error
  all_stats$se2_jack <- all_stats$seJack * 2

  # plot values

  if (!is.null(fill_colour)) {
    # fill with specified colour
    p1 <- ggplot() +
    geom_area(data = all_stats, aes(rank, se2_jack, fill = fill_colour)) +
    geom_line(data = all_stats, aes(rank, se2_boot, colour = "red")) +
    geom_line(data = all_stats, aes(rank, conf_int, colour = "black")) +

    xlab("Site index, ranked w.r.t. the Bootstrap estimates of the confidence intervals") +
    ylab("Uncertainty") +
    ylim(0, 0.5) +
    scale_colour_manual(name = '', values =c('black'='black','red'='red'),
                        labels = c('Confidence interval (p95 - p05)',
                                   '2 x standard error (Bootstrap)')) +
    scale_fill_identity(name = '', guide = 'legend',labels = c("2 x standard error (Jackknife)")) +
    facet_wrap(~GOF_stat, nrow = 2) +
    guides(
      color = guide_legend(order = 1),
      fill = guide_legend(order = 2)
    )
  } else {
    # line plots
    # melt data frame before plotting
    all_stats <- all_stats[, c("rank", "GOF_stat", "conf_int", "se2_boot", "se2_jack")]
    melted <- melt(all_stats, id.vars = c("rank", "GOF_stat"), measure_vars = c("conf_int", "se2_boot", "se2_jack"))
    melted$variable <- as.character(melted$variable)
    melted$variable[melted$variable == "conf_int"] <- "Confidence interval (p95 - p05)"
    melted$variable[melted$variable == "se2_boot"] <- "2 x standard error (Bootstrap)"
    melted$variable[melted$variable == "se2_jack"] <- "2 x standard error (Jackknife)"

    p1 <- ggplot(melted, aes(rank, value, colour = variable)) +
      geom_line() +
      xlab("Site index, ranked w.r.t. the Bootstrap estimates of the confidence intervals") +
      ylab("Uncertainty") +
      ylim(0, 0.5) +
      scale_colour_manual(name = '', values =c("black", "red", "orange"),
                          labels = c('Confidence interval (p95 - p05)',
                                     '2 x standard error (Bootstrap)',
                                     "2 x standard error (Jackknife)")) +
      facet_wrap(~GOF_stat, nrow = 2)

  }



 return(p1)
}
