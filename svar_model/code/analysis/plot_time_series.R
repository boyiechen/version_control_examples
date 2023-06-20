#' @title Sketch the raw and processed time series data
#' The script provides figures for the time series data before our structural estimation

rm(list = ls())

#' Dependencies
source("./code/utility/utils.R")

#' @section Load data
path_data <- file.path(DROPBOX_PATH, "./data/cleaned_data/data.RDS")
data <- readRDS(file = path_data) 
str(data)


#' Time Serie Variables
#' yt = [R rgdp rmr, loan ccost sent hp]′
#' yt = [R lrgdp rmr, loanratio lccost sent lrhp]′

#' @title Figure 1
# Make multiple plots
fig_list <- create_multiple_time_series(data, "date", 
                                        c("R", "lrgdp", "rmr", 
                                          "loanratio", "lccost", "sent", 
                                          "lrhp"))
fig <- make_multiple_plots(fig_list,
                           ncol = 3, nrow = 3)

# Save figure
ggsave_default(fig, path = "./result/figure/fig_time_series.pdf")


#' @title Figure 2
# Make multiple plots
fig_list <- create_multiple_time_series(data, "date", 
                                        c("lrhp", 
                                          "rhp",
                                          "pir",
                                          "lhp_tw1"))
fig <- make_multiple_plots(fig_list,
                           ncol = 2, nrow = 3)

# Save figure
ggsave_default(fig, path = "./result/figure/fig_time_series2.pdf")

