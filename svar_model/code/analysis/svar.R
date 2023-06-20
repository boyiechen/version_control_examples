#' @title Structural VAR Analysis
#' The script performs the estimation of a SVAR with recursive identification (World Ordering)

rm(list = setdiff(ls(), c("confidence_level", "VAR.P", "Robustness")))

#' Dependencies
source("./code/utility/utils.R")

# import VAR estimation functions
source("./code/utility/var_functions.R")

#' @section Preamble
inv_tol = 1e-20 #求反矩陣時做數值運算允許的最小誤差(避免singular matrix)
options(warn=-1)    # 關掉warning
options(scipen=999) #不要科學記號

#' @section Load data
path_data = file.path(DROPBOX_PATH, "./data/cleaned_data/data.RDS")
data <- readRDS(file = path_data)

# ensure right data type
class(data$date) == "Date"

colnames(data)

#' @section Prepare matrix form data of a 7-variable model
if (Robustness %in% c(0,1)) {
  By <- data %>% 
    dplyr::select(R, lrgdp, rmr, loanratio, lccost, sent, lrhp) %>% 
    drop_na() %>%
    as.matrix()
  if (Robustness == 0) { VAR.P <- 1 }
  if (Robustness == 1) { VAR.P <- 4 }
} else if (Robustness == 2) {
  VAR.P <- 1
  By <- data %>% 
    dplyr::select(R, lrgdp, rmr, loanratio, lccost, sent, pir) %>% 
    drop_na() %>%
    as.matrix() 
} else if (Robustness == 3) {
  VAR.P <- 1
  By <- data %>% 
    dplyr::select(R, lrgdp, rmr, loanratio, lccost, sent, lhp_tw1) %>% 
    drop_na() %>%
    as.matrix() 
} else if (Robustness == 4) {
  VAR.P <- 1
  By <- data %>% 
    dplyr::select(R, lrgdp, rmr, loanratio, lccost, sent, lhp_tpe) %>% 
    drop_na() %>%
    as.matrix() 
}


#' @section Model setup
#' Parameters
#----- 模型設定 -----#
# VAR.P = 1                       # 最大的落後項數
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

Amat = diag(num_var)
for (i in 2:num_var) {
  for (j in 1:(i-1) ) {
    Amat[i,j] <- NA
  }
}

#' it may be abstract to understand how we initialize A matrix
#' here is an example when num_var = 5
#' For a 5-variable model
# Amat = diag(5)
# Amat[2,1]  = NA; 
# Amat[3,1]  = NA; Amat[3,2]  = NA;
# Amat[4,1]  = NA; Amat[4,2]  = NA; Amat[4,3]  = NA;
# Amat[5,1]  = NA; Amat[5,2]  = NA; Amat[5,3]  = NA; Amat[5,4]  = NA;

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
