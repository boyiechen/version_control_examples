#' @title Master Script

rm(list = ls()); gc()

#' @section Check Requirements
source("./code/utility/requirements.R")
source("./code/utility/preamble.R")
source("./code/utility/config.R")

#' Dependencies
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
require(matrixcalc)
library(lattice)
library(cowplot)
library(xtable)
library(openxlsx)
library(mFilter)
library(assertthat)

#' @section data cleaning
source("./code/cleaning/data_cleaning.R")

#' @section  estimation of SVAR
rm(list = ls())

# plot time series
source("./code/analysis/plot_time_series.R")

# estimate Reduced Form VAR model and find the A, B matrices
# specify main model
Robustness <- 0
source("./code/analysis/svar.R")


# estimate Impulse Response function
source("./code/analysis/estimate_IRF.R")
confidence_level = 0.95
source("./code/analysis/estimate_IRF_with_BS.R")

# plot Impulse Response function
shock_sign <- 1
source("./code/analysis/plot_IRF_with_CI_shock_sign.R")

# estimate Variance Decomposition and export table
source("./code/analysis/estimate_VD.R")

# estimate Historical Decomposition
source("./code/analysis/estimate_HD2.R")

# jewiofjweoifjo2
# fjweoifjowiefoi

