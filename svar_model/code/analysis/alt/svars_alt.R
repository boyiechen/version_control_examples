#' @title This script performs replication with package `svars`

# `svars` package
# ref: https://github.com/alexanderlange53/svars
library(svars)
library("ggplot2")
library("ggfortify")
library("dplyr")
library("tidyr")

rm(list = ls())

source("./code/utility/preamble.R")
source("./code/utility/utils.R")


#' TW Housing Price data
#' @section Load data
path_data = file.path(DROPBOX_PATH, "./data/cleaned_data/data.RDS")
data_full <- readRDS(file = path_data) 
data <- data_full %>% 
  dplyr::select(R, lrgdp, rmr, loanratio, lccost, sent, lrhp) %>% 
  drop_na()
num_var <- dim(data)[2]

# Estimate VAR model
data_ts <- ts(data, start=c(1993, 7), frequency = 4)

# time-series plot
plot(data_ts)
autoplot(data_ts, facet = TRUE) + theme_bw()

# estimate reduced-form VAR
reduced.form <- vars::VAR(data_ts, lag.max = 1, ic = "AIC" )
# cholesky decomposition
structural.form <- id.chol(reduced.form)
summary(structural.form)

# modify shock signs
structural.form$B[,1] <- structural.form$B[,1]*(-1)
structural.form$B[,3] <- structural.form$B[,3]*(-1)


impulse.response <- irf(structural.form, n.ahead = 40)
plot(impulse.response, scales = 'free_y')

cores <- parallel::detectCores() - 1
wildboot.svar <- wild.boot(structural.form, n.ahead = 40, nboot = 1500, nc = cores)
mbboot.svar <- mb.boot(structural.form, n.ahead = 40, nboot = 1500, nc = cores)

plot(wildboot.svar)
plot(mbboot.svar)


fevd <- fevd(structural.form, n.ahead = 40)
fevd
plot(fevd)


#' @section Truncated HD estimation
# hist.decomp <- hd(structural.form, series = 7)
hist.decomp <- hd(structural.form, series = 7, transition = 0.26)
class(hist.decomp$hidec)
# get dataframe
df.hd <- hist.decomp$hidec %>% 
  # append date label
  as_tibble() %>% 
  bind_cols(date = time(hist.decomp$hidec)) %>% 
  # append the actual time series
  bind_cols(
    lrhp_actual = data_full %>% 
      filter(date >= "2001-09-01") %>% 
      pull(lrhp)
  ) %>% 
  # reorder and rename
  select(date, lrhp_actual,
         lrhp_demeaned = `Demeaned series  lrhp`,
         lrhp_constructed = `Constructed series  lrhp`,
         lrhp_shock_R = `Cumulative effect of  R shock on  lrhp`,
         lrhp_shock_lrgdp = `Cumulative effect of  lrgdp shock on  lrhp`,
         lrhp_shock_rmr = `Cumulative effect of  rmr shock on  lrhp`,
         lrhp_shock_loanratio = `Cumulative effect of  loanratio shock on  lrhp`,
         lrhp_shock_lccost = `Cumulative effect of  lccost shock on  lrhp`,
         lrhp_shock_sent = `Cumulative effect of  sent shock on  lrhp`,
         lrhp_shock_lrhp = `Cumulative effect of  lrhp shock on  lrhp`
         ) %>% 
  mutate_all(.funs = as.numeric)
plot(hist.decomp)
# plot(df.hd, linetype = "l")


df.hd <- df.hd %>% 
  # constructed is the sum of all shocks
  mutate(all_shocks = lrhp_shock_R + lrhp_shock_lrgdp + lrhp_shock_rmr
                      + lrhp_shock_loanratio + lrhp_shock_lccost
                      + lrhp_shock_sent + lrhp_shock_lrhp) %>% 
  # the diff between constructed and demeaned series: base projection
  mutate(base_demeaned = lrhp_demeaned - lrhp_constructed,
         base = lrhp_actual - lrhp_constructed) 

colnames(df.hd) 
df.hd %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = lrhp_actual, col = "actual")) +
  # geom_line(aes(y = lrhp_demeaned, col = "demaned")) +
  # geom_line(aes(y = lrhp_constructed, col = "costructed")) +
  # geom_line(aes(y = lrhp_shock_R, col = "R")) +
  # geom_line(aes(y = lrhp_shock_lrgdp, col = "lrgdp"))+
  # geom_line(aes(y = lrhp_shock_rmr, col = "rmr"))+
  # geom_line(aes(y = lrhp_shock_loanratio, col = "loanratio"))+
  # geom_line(aes(y = lrhp_shock_lccost, col = "lccost"))+
  # geom_line(aes(y = lrhp_shock_sent, col = "sent"))+
  # geom_line(aes(y = lrhp_shock_lrhp, col = "lrhp"))+
  # geom_line(aes(y = all_shocks, col = "all shocks")) + 
  # geom_line(aes(y = base_demeaned, col = "demeaned - all shocks"))+
  geom_line(aes(y = base, col = "actual - all shocks"))
  

# only turn on R shocks
colnames(df.hd)

create_HD_plot <- function(shock_var = "R", title_text = "") {
  legend_name <- paste0("Shock: ", shock_var)
  shock_var <- paste0("lrhp_shock_", shock_var)
  shock_var <- as.symbol(shock_var)
  fig <- df.hd %>% 
    ggplot(aes(x = date)) + 
    geom_line(aes(y = lrhp_actual, col = "Real House Prices"), linetype = "solid") +
    geom_line(aes(y = base, col = "Base Projection"), linetype = "dotted") +
    geom_line(aes(y = base + !!shock_var, col = legend_name), linetype = "dashed") +
    
    # asthetic setting
    labs(x = '',
         y = '',
         title = title_text)+
    # scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
    scale_color_manual(values = c("red", "royalblue", "darkgreen")) +
    Text_Size_Theme+
    theme(legend.position="bottom", 
          legend.direction="vertical",
          legend.title = element_blank())
  return(fig)
}
create_HD_plot("R")
create_HD_plot("lrgdp")
create_HD_plot("rmr")
create_HD_plot("loanratio")
create_HD_plot("lccost")
create_HD_plot("sent")
create_HD_plot("lrhp")

variables <- c("R", "lrgdp", "rmr", "loanratio", "lccost", "sent", "lrhp")
fig_list <- list()
for (variable in variables) {
  fig <- create_HD_plot(variable)
  fig_list <- append(fig_list, list(fig))
}
fig <- make_multiple_plots(fig_list, ncol = 2)
ggsave_default(fig, path = "./result/figure/fig_HD_alt.pdf",
               width = 60, height = 40)



 
#' @section HD Counterfactuals
# counterfactuals
counterfactuals <- cf(structural.form, series = 7)
counterfactuals$actual
counterfactuals$counter
plot(counterfactuals)

counterfactuals_truncated <- cf(structural.form, series = 7, transition = 0.26)
plot(counterfactuals_truncated)

