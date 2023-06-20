#' @section Robustness test

source("./code/analysis/svar.R")
# estimate Impulse Response function
source("./code/analysis/estimate_IRF.R")
confidence_level = 0.95
source("./code/analysis/estimate_IRF_with_BS.R")
# plot Impulse Response function
shock_sign <- 1
source("./code/analysis/plot_IRF_with_CI_shock_sign.R")
shock_sign <- -1
source("./code/analysis/plot_IRF_with_CI_shock_sign.R")
shock_signs <- c(-1, 1, -1, 1, 1, 1, 1)
source("./code/analysis/plot_IRF_with_CI_custom_shock_sign.R")
# Different C.I.
confidence_level = 0.90
source("./code/analysis/estimate_IRF_with_BS.R")
shock_sign <- 1
source("./code/analysis/plot_IRF_with_CI_shock_sign.R")
shock_sign <- -1
source("./code/analysis/plot_IRF_with_CI_shock_sign.R")
shock_signs <- c(-1, 1, -1, 1, 1, 1, 1)
source("./code/analysis/plot_IRF_with_CI_custom_shock_sign.R")

# estimate Variance Decomposition and export table
source("./code/analysis/estimate_VD.R")

# estimate Historical Decomposition
source("./code/analysis/estimate_HD.R")
