# rm(list = setdiff(ls(), c("confidence_level", "VAR.P", "Robustness")))
rm(list = ls())

#' Dependencies
source("./code/utility/utils.R")

# import VAR estimation functions
source("./code/utility/var_functions.R")

#' @section Preamble
inv_tol = 1e-20 #求反矩陣時做數值運算允許的最小誤差(避免singular matrix)
options(warn=-1)    # 關掉warning
options(scipen=999) #不要科學記號

#' @section Load data
path_data = "./data/cleaned_data/data.RDS"
data <- readRDS(file = path_data)

#' Apply HP filter
data <- data %>% 
  # set time series structure
  mutate(ts_lrgdp = ts(lrgdp, frequency = 4, start = c(year(date), season)),
         # prepare other variables in the SVAR vector
         # consumer price inflation
         cpinf = 100 * log(cpi / lag(cpi, 4))
  )

hp_res <- hpfilter(data$ts_lrgdp, type = "lambda", freq = 1600)

data <- data %>% 
  bind_cols(lrgdp_trend = hp_res$trend,
            output_gap = hp_res$cycle) %>% 
  mutate(lrgdp_trend = as.numeric(lrgdp_trend),
         output_gap = as.numeric(output_gap))

# plot and check
data %>% ggplot(aes(x = date))+
  geom_line(aes(y = lrgdp, col = "Real GDP")) + 
  geom_line(aes(y = lrgdp_trend, col = "trend")) 

data %>% ggplot(aes(x = date)) + 
  geom_line(aes(y = output_gap, col = "output gap"))



# Estimate SVAR
#' @section Prepare matrix form data of a 7-variable model
By <- data %>% 
  dplyr::select(gap = output_gap,
         cpinf,
         rhp,
         R) %>% 
  drop_na() %>%
  as.matrix()


#' @section Model setup
#' Parameters
#----- 模型設定 -----#
VAR.P = 1                       # 最大的落後項數
CONST = TRUE                    # 是否有常數項
Y     = VAR.Y(By, VAR.P)        # 設定 Y
X     = VAR.X(By, VAR.P)        # 設定 X
hrz = 39 # the length of response
shock_sign = -1 # control the positive/negative shock
num_var <- dim(By)[2]


#' @description Reduced Form VAR
###### 參數估計 ######
(Coef.OLS    = VAR.OLS(Y, X, CONST)                  )
(Sigma.OLS   = VAR.Sigma.OLS(Y, X, Coef.OLS, CONST)  )
(Sigma.MLE   = VAR.Sigma.MLE(Y, X, Coef.OLS, CONST))

#' @description Identification Conditions
#' A recursive identification, so the diagonal terms are 0
Amat = diag(x = 1, num_var)
for (i in 2:num_var) {
  for (j in 1:(i-1) ) {
    Amat[i,j] <- NA
  }
}

Bmat = diag(num_var)
diag(Bmat) = NA


#' @description Estimate A, B matrix and implement Cholesky decomposition
C.Prime <- chol(Sigma.OLS)
C <- t(C.Prime)

# solving system of linear equation
B0 <- diag(diag(C), ncol = num_var, nrow = num_var)
A0 <- B0 %*% solve(C)

# A, B matrices
SVAR_AB_est <- list("A0.svar" = A0, "B0.svar" = B0)


# Point estimation of IRF, denoted as ddTheta
# it is necessary to have ddTheta to have further estimations
SVAR_AB_IRF <- VAR.svarirf.AB(By, VAR.P, Amat, Bmat, h = hrz, CONST, SVAR_AB_est = SVAR_AB_est)

