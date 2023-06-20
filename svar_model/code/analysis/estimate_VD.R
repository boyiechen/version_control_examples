#' @section Variance Decomposition

rm(list = setdiff(ls(), c("VAR.P", "Robustness")))

# Prepare necessary matrices: By, Amat, Bmat and SVAR_AB_IRF
source("./code/analysis/svar.R")

# selected horizons
horizons <- c(1,2,4,8,12,20,32,40)

# `ddTheta` 放已經估出來的IRF 
# m表示對於第幾個變數的變異數分解（如第7個是對房價的變異數分解）
SVAR_AB_VarDecomp <- VAR.svardecomp.AB(m = 7, By, VAR.P,
                                       AMat, BMat, h=(hrz+1),
                                       Const=TRUE, ddTheta = SVAR_AB_IRF)

shock_colnames <- c("Monetary Policy Shock",
                    "Real Output Shock",
                    "User Cost Shock",
                    "Credit Shock",
                    "Construction Cost Shock",
                    "Sentiment Shock",
                    "House Price Shock")
# export the raw table
VD_TABLE <- (SVAR_AB_VarDecomp*100) %>% 
  as_tibble %>% 
  # rounded
  mutate_all(round, digits = 2)
colnames(VD_TABLE) <- shock_colnames

# export csv table
if (VAR.P == 1 & Robustness == 0) {
  write_csv(VD_TABLE, file = "./result/table/tab_VD.csv")
}

# export LaTeX table
SVAR_VD <- VD_TABLE %>% 
  # select different horizons
  slice(horizons) %>% 
  mutate(horizon = horizons) %>% 
  relocate(horizon, before = 1)
  

# convert to latex format
tab_VD <- xtable(SVAR_VD, 
                 caption= "", 
                 align=c("c","c","c","c","c","c","c", "c", "c"))

if (VAR.P == 1 & Robustness == 0) {
  path_tex <- "./result/table/tab_VD.tex"
} else if (Robustness == 1) {
  path_tex <- "./result/robustness/r1/table/tab_VD.tex"
} else if (Robustness == 2) {
  path_tex <- "./result/robustness/r2/table/tab_VD.tex"
} else if (Robustness == 3) {
  path_tex <- "./result/robustness/r3/table/tab_VD.tex"
} else if (Robustness == 4) {
  path_tex <- "./result/robustness/r4/table/tab_VD.tex"
}

print(tab_VD, file= path_tex,
        include.rownames=FALSE,
        append=F, table.placement = "h",
        caption.placement="bottom", 
        hline.after = seq(from = -1,to = nrow(tab_VD), by = 1))
