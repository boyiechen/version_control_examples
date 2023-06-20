#' @section Impulse Response Function (IRF) -- without C.I. (point estimators)

source("./code/analysis/svar.R")

# SVAR_AB_IRF <- VAR.svarirf.AB(By, VAR.P, Amat, Bmat, h = hrz, CONST, SVAR_AB_est = SVAR_AB_est)

# 5*5個圖的time series
df_IRF_plot <- matrix(NA, hrz+1, num_var^2) 
h <- 0 # h表示第幾期的IRF
for(period in SVAR_AB_IRF){
  k <- 0 # k表示把5*5的矩陣攤平到25個col的df時，要攤到第幾個columns上
  h <- h + 1 # h表示第幾期的IRF
  for(j in (1:num_var) ){
    for(i in (1:num_var) ){
      k <- k + 1 # k表示把5*5的矩陣攤平到25個col的df時，要攤到第幾個columns上
      df_IRF_plot[h,k] <- period[i,j]
    }
  }
}
# the dimension of IRF dataframe would be horizon * num_var^2
df_IRF_plot <- df_IRF_plot %>% as_tibble()
if (VAR.P == 1 & Robustness == 0) {
  saveRDS(df_IRF_plot, file.path(DROPBOX_PATH, "./data/intermediate_result/df_IRF_plot.RDS"))
} else if (Robustness == 1) {
  saveRDS(df_IRF_plot, file.path(DROPBOX_PATH, "./data/robustness/r1/df_IRF_plot.RDS"))
} else if (Robustness == 2) {
  saveRDS(df_IRF_plot, file.path(DROPBOX_PATH, "./data/robustness/r2/df_IRF_plot.RDS"))
} else if (Robustness == 3) {
  saveRDS(df_IRF_plot, file.path(DROPBOX_PATH, "./data/robustness/r3/df_IRF_plot.RDS"))
} else if (Robustness == 4) {
  saveRDS(df_IRF_plot, file.path(DROPBOX_PATH, "./data/robustness/r4/df_IRF_plot.RDS"))
}
