#' @section Historical Decomposition
#' starting from 2001Q4

rm(list = setdiff(ls(), c("VAR.P", "Robustness")))

source("./code/analysis/svar.R")

# adjust the starting date
By_with_date <- data %>%
  filter(date >= ymd("2001-10-01")) %>%
  dplyr::select(date, R, lrgdp, rmr, loanratio, lccost, sent, lrhp)

By <- By_with_date %>%
  dplyr::select(-date) %>%
  as.matrix()

# ddTheta's length determines the horizon of HD can be
# so if we want to trace back longer (have a greater length of HD horizon)
# we need to re-estimate ddTheta with full length
if(hrz < nrow(By)){
  SVAR_AB_IRF <- VAR.svarirf.AB(By, VAR.P, Amat, Bmat,    # pre-estimated A, B matrices
                                h = nrow(By),             # a longer horizon
                                CONST, 
                                SVAR_AB_est = SVAR_AB_est # estimated A, B matrices
                                )
}

# # adjust the starting date
# By_with_date <- data %>% 
#   filter(date >= ymd("2001-07-01")) %>% 
#   select(date, R, lrgdp, rmr, loanratio, lccost, sent, lrhp)
# 
# By <- By_with_date %>% 
#   select(-date) %>% 
#   as.matrix()

# estimate historical decomposition
# it is a matrix that stores each shock in the past
# note that the dimension is (available horizon, num_var^2)


SVAR_AB_HistDecomp <- VAR.svarhist.AB(By, VAR.P, Amat, Bmat, CONST)
# check dimenson
# the column of the matrix should be num_var^2
# use assertthat
data %>% drop_na() %>% nrow() == dim(SVAR_AB_HistDecomp)[1]
num_var^2 == dim(SVAR_AB_HistDecomp)[2]

#' @example Explanation of the num_var^2 columns
# If we have 5-variable model which is [mp, exp, hs, hd, hp]'
# V1 is the historical mp shocks that affected the interest rate time series
# V5 is the historical mp shocks that affected the house price time series
# V21 is the historical hp shocks that affected the interest rate time series
# V25 is the historical hp shocks that affected the house price time series

#----- Base Project 估計 -----#
# Base projection is a counterfactural time series without any shock
SVAR_AB_Hist.c0 <- VAR.baseproject(By, VAR.P, CONST)
dim(SVAR_AB_Hist.c0) == dim(By)

#' @details 
#' The actual time series (what we observed): By
#' The counterfactual time series without any shocks (base projection): c0
#' The historical shocks: SVAR_AB_HistDecomp
#' The following should hold:
#' By = c0 + (aggregated historical shocks)

# we want the following time series aligned with each other
# the actual time series variable, the base projection, the historical shocks plus the base projection

# By[, 7] # real house prices
# SVAR_AB_Hist.c0[, 7] # base projection
# SVAR_AB_HistDecomp[, c(7, 14, 21, 28, 35, 42, 49)]

# create df for plots
df.tmp <- bind_cols(time = 1:nrow(By),
                    actual = By[,7],
                    base = SVAR_AB_Hist.c0[, 7],
                    shock_1 = SVAR_AB_HistDecomp[, 7],
                    shock_2 = SVAR_AB_HistDecomp[, 14],
                    shock_3 = SVAR_AB_HistDecomp[, 21],
                    shock_4 = SVAR_AB_HistDecomp[, 28],
                    shock_5 = SVAR_AB_HistDecomp[, 35],
                    shock_6 = SVAR_AB_HistDecomp[, 42],
                    shock_7 = SVAR_AB_HistDecomp[, 49]
                    ) %>% 
  mutate(shock_1 = base + shock_1,
         shock_2 = base + shock_2,
         shock_3 = base + shock_3,
         shock_4 = base + shock_4,
         shock_5 = base + shock_5,
         shock_6 = base + shock_6,
         shock_7 = base + shock_7,
         ) %>%
  bind_cols(date = By_with_date$date)

# Plots
# p_base <- ggplot(df.tmp, aes(x = time)) +
#   geom_line(aes(y = actual, color = 'Real House Prices')) +
#   geom_line(aes(y = base, color = 'Base Projection'))

p_base <- ggplot(df.tmp, aes(x = date)) +
  geom_line(aes(y = actual, color = 'Real House Prices')) +
  geom_line(aes(y = base, color = 'Base Projection')) +
  Text_Size_Theme +
  labs(x = "time", y = "")

p1 <- p_base + 
  geom_line(aes(y = shock_1, color = 'Monetary Policy Shock'), linetype = "dashed") +
  scale_color_manual(values=c('Real House Prices' = 'royalblue',
                              'Base Projection' = 'red',
                              'Monetary Policy Shock' = 'darkgreen'))+ 
  theme(legend.position="bottom", 
      legend.direction="vertical",
      legend.title = element_blank())

p2 <- p_base + 
  geom_line(aes(y = shock_2, color = 'Real-Output Shock'), linetype = "dashed") +
  scale_color_manual(values=c('Real House Prices' = 'royalblue',
                              'Base Projection' = 'red',
                              'Real-Output Shock' = 'darkgreen'))+ 
  theme(legend.position="bottom", 
      legend.direction="vertical",
      legend.title = element_blank())

p3 <- p_base + 
  geom_line(aes(y = shock_3, color = 'Mortgage-Rate Shock'), linetype = "dashed") +
  scale_color_manual(values=c('Real House Prices' = 'royalblue',
                              'Base Projection' = 'red',
                              'Mortgage-Rate Shock' = 'darkgreen'))+ 
  theme(legend.position="bottom", 
      legend.direction="vertical",
      legend.title = element_blank())
  
p4 <- p_base + 
  geom_line(aes(y = shock_4, color = 'Credit Shock'), linetype = "dashed") +
  scale_color_manual(values=c('Real House Prices' = 'royalblue',
                              'Base Projection' = 'red',
                              'Credit Shock' = 'darkgreen'))+ 
  theme(legend.position="bottom", 
      legend.direction="vertical",
      legend.title = element_blank())
  
p5 <- p_base + 
  geom_line(aes(y = shock_5, color = 'Construction-Cost Shock'), linetype = "dashed") +
  scale_color_manual(values=c('Real House Prices' = 'royalblue',
                              'Base Projection' = 'red',
                              'Construction-Cost Shock' = 'darkgreen'))+ 
  theme(legend.position="bottom", 
      legend.direction="vertical",
      legend.title = element_blank())
  
p6 <- p_base + 
  geom_line(aes(y = shock_6, color = 'Sentiment Shock'), linetype = "dashed") +
  scale_color_manual(values=c('Real House Prices' = 'royalblue',
                              'Base Projection' = 'red',
                              'Sentiment Shock' = 'darkgreen'))+ 
  theme(legend.position="bottom", 
      legend.direction="vertical",
      legend.title = element_blank())
  
p7 <- p_base + 
  geom_line(aes(y = shock_7, color = 'House-Price Shock'), linetype = "dashed") +
  scale_color_manual(values=c('Real House Prices' = 'royalblue',
                              'Base Projection' = 'red',
                              'House-Price Shock' = 'darkgreen')) + 
  theme(legend.position="bottom", 
      legend.direction="vertical",
      legend.title = element_blank())

# Save
fig <- make_multiple_plots(list(p1, p2, p3, p4, p5, p6, p7), ncol = 2, nrow = 4)
if (VAR.P == 1 & Robustness == 0) {
  ggsave_default(fig, "./result/figure/fig_HD.pdf",
                 width = 30, height = 40)
} else if (Robustness == 1) {
  ggsave_default(fig, "./result/robustness/r1/figure/fig_HD.pdf",
                 width = 30, height = 40)
} else if (Robustness == 2) {
  ggsave_default(fig, "./result/robustness/r2/figure/fig_HD.pdf",
                 width = 30, height = 40)
} else if (Robustness == 3) {
  ggsave_default(fig, "./result/robustness/r3/figure/fig_HD.pdf",
                 width = 30, height = 40)
} else if (Robustness == 4) {
  ggsave_default(fig, "./result/robustness/r4/figure/fig_HD.pdf",
                 width = 30, height = 40)
}
