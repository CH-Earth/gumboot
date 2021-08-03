## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(gumboot)

## -----------------------------------------------------------------------------
flows_1030500 <- flows_1030500 
head(flows_1030500)

## ---- fig.width = 5-----------------------------------------------------------
library(ggplot2)
library(reshape2)
melted <- melt(flows_1030500, id.vars = "date")
ggplot(melted, aes(date, value, colour = variable)) +
  geom_line() +
  xlab("") +
  labs(y = bquote('Daily streamflow'~(m^3/s)), x = "")

## -----------------------------------------------------------------------------
NSE_values <- bootjack(flows_1030500, GOF_stat = "NSE")
NSE_values

## -----------------------------------------------------------------------------
bootjack(flows_1030500, GOF_stat = "NSE", seed = 1)
bootjack(flows_1030500, GOF_stat = "NSE", seed = 1)

## -----------------------------------------------------------------------------
NSE_samples <- bootjack(flows_1030500, GOF_stat = "NSE", returnSamples = TRUE)
names(NSE_samples)

## ---- fig.width=5, warning=FALSE, message = FALSE, error=FALSE----------------
ggplot(NSE_samples$statsBoot, aes(NSE)) + 
  geom_histogram() +
  ggtitle("Bootstrap samples")

## ---- eval = FALSE------------------------------------------------------------
#  CAMELS_sites <- hcdn_conus_sites
#  nc_file <- "/home/kevin/data/projects/bootstrappR_test/hess2019/results_hcdn_flow.nc"
#  CAMELS_stats <- CAMELS_bootjack(CAMELS_sites, nc_file)

## ---- eval = FALSE------------------------------------------------------------
#  CAMELS_stats_cleaned <- na.omit(CAMELS_stats)
#  ggplot_estimate_uncertainties(CAMELS_stats_cleaned, fill_colour = "orange")

