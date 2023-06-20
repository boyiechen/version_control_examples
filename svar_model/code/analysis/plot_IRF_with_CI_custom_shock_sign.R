rm(list = setdiff(ls(), c("shock_signs", "confidence_level", "VAR.P", "Robustness")))
# set specific shock signs
# only monetary policy shock and user cost shock are negative
# i.e. shock 1 & shock 3 are negative
# shock_signs <- c(-1, 1, -1, 1, 1, 1, 1)

# Load graph setting
source("./code/utility/preamble.R")
source("./code/utility/utils.R")

# Load data
# Load data
if (VAR.P == 1 & Robustness == 0) {
  df_IRF_plot <- readRDS("./data/intermediate_result/df_IRF_plot.RDS")
  df_IRF.sim <- read_rds(paste0("./data/intermediate_result/df_IRF.sim_hrz40_", confidence_level*100, "CI.rds"))
} else if (Robustness == 1) {
  df_IRF_plot <- readRDS("./data/robustness/r1/df_IRF_plot.RDS")
  df_IRF.sim <- read_rds(paste0("./data/robustness/r1/df_IRF.sim_hrz40_", confidence_level*100, 
                                "CI.rds"))
} else if (Robustness == 2) {
  df_IRF_plot <- readRDS("./data/robustness/r2/df_IRF_plot.RDS")
  df_IRF.sim <- read_rds(paste0("./data/robustness/r2/df_IRF.sim_hrz40_", confidence_level*100, 
                                "CI.rds"))
} else if (Robustness == 3) {
  df_IRF_plot <- readRDS("./data/robustness/r3/df_IRF_plot.RDS")
  df_IRF.sim <- read_rds(paste0("./data/robustness/r3/df_IRF.sim_hrz40_", confidence_level*100, 
                                "CI.rds"))
} else if (Robustness == 4) {
  df_IRF_plot <- readRDS("./data/robustness/r4/df_IRF_plot.RDS")
  df_IRF.sim <- read_rds(paste0("./data/robustness/r4/df_IRF.sim_hrz40_", confidence_level*100, 
                                "CI.rds"))
}


# 畫IRF & Bootstrap C.I.
df_IRF_plot.BS.L <- matrix(NA, nrow = hrz+1, ncol = num_var^2)
df_IRF_plot.BS.U <- matrix(NA, nrow = hrz+1, ncol = num_var^2)
df_IRF_plot.BS.Median <- matrix(NA, nrow = hrz+1, ncol = num_var^2)
df_IRF_plot.BS.Mean <- matrix(NA, nrow = hrz+1, ncol = num_var^2)
for(col in 1:num_var^2){
  for(row in 1:(hrz+1) ){
    df_IRF_plot.BS.L[row,col] <- quantile(df_IRF.sim[row,col,], probs = 0.025)
    df_IRF_plot.BS.U[row,col] <- quantile(df_IRF.sim[row,col,], probs = 0.975)
    df_IRF_plot.BS.Median[row,col] <- quantile(df_IRF.sim[row,col,], probs = 0.5)
    df_IRF_plot.BS.Mean[row,col] <- mean(df_IRF.sim[row,col,])
  }
}

df_IRF_plot.BS.L <- df_IRF_plot.BS.L %>% as_tibble()
df_IRF_plot.BS.U <- df_IRF_plot.BS.U %>% as_tibble()
df_IRF_plot.BS.Median <- df_IRF_plot.BS.Median %>% as_tibble()
df_IRF_plot.BS.Mean <- df_IRF_plot.BS.Mean %>% as_tibble()


# With shock sign
#' since the dataframe is organized as the following structure:
#' V1 V2 V3 V4 ... V(num_var^2)
#' and V1 represents the IRF of Y1_shock1
#'     V2 represents the IRF of Y1_shock2
#'     V(num_var + 1) represents the IRF of Y2_shock1
#' Thus we extend the shock_signs vector into a num_var^2 size vector to multiply with these columns
cat(">>> shock signs are: ", shock_signs, "\n")

# the combination of titles
title_list_response <- c("Overnight Rate", 
                         "Real GDP",
                         "Real Mortgage Rate",
                         "Housing Loan",
                         "Construction Price",
                         "Housing Market Sentiment",
                         "Real House Price"
                         )
title_list_shock <- c("Monetary Policy", 
                      "Real Output",
                      "User Cost",
                      "Credit",
                      "Construction Cost",
                      "Sentiment",
                      "House Price"
                      )

# if (shock_sign == (-1)) {
#   negative_label <- "Negative "
# } else if (shock_sign == 1) {
#   negative_label <- ""
# }
negative_labels <- c()
for (shock_sign in shock_signs) {
  if (shock_sign == -1) {
    negative_labels <- c(negative_labels, "negative ")
  } else if (shock_sign == 1) {
    negative_labels <- c(negative_labels, "")
  }
}

# negative_label <- ""

title_list <- c(paste0("Reponse of ", title_list_response, " to ", negative_labels[1], title_list_shock[1], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", negative_labels[2], title_list_shock[2], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", negative_labels[3], title_list_shock[3], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", negative_labels[4], title_list_shock[4], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", negative_labels[5], title_list_shock[5], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", negative_labels[6], title_list_shock[6], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", negative_labels[7], title_list_shock[7], " Shock")
                )

# making figures
ind <- 0
for(i in 1:num_var){ # i denotes the shock
  for(j in 1:num_var){ # j denotes the response variable
    ind <- ind+1
    nam <- paste("shock", i, "y", j, sep = '')
    assign(nam, bind_cols(df_IRF_plot.BS.L[ind], df_IRF_plot.BS.U[ind],
                          df_IRF_plot.BS.Median[ind], df_IRF_plot.BS.Mean[ind],
                          df_IRF_plot[ind]) * shock_signs[i]
           )
    # 改名
    evalStr <- paste0("colnames(", nam, ") <- c('Lower', 'Upper', 'Median', 'Mean', 'Actual')")
    eval(parse(text=evalStr))
    # 圖層
    evalStr <- paste0("p", ind, " <- ", "ggplot(",nam,")",
                      " + geom_hline(yintercept=0, color = 'grey')",
                      " + geom_line(aes(x = 1:nrow(", nam, "), y = Lower), linetype = 'dashed', col='red')",
                      " + geom_line(aes(x = 1:nrow(", nam, "), y = Upper), linetype = 'dashed', col='red')",
                      " + geom_line(aes(x = 1:nrow(", nam, "), y = Actual), col = 'Blue')"
                      )
    eval(parse(text=evalStr))
    
    # assign x and y labels, and theme, title
    evalStr <- paste0("p", ind, " <- ", "p", ind, 
                      " + Text_Size_Theme",
                      " + labs(x = 'Time (Season)',
                               y = '',
                               title = title_list[", ind, "])"
                      )
    eval(parse(text=evalStr))
  }
}

# Create plots
## By variables (house price is our variable of interest)
fig_hp <- make_multiple_plots(list(p7, p14, p21, p28, p35, p42, p49), ncol = 2)

if (VAR.P == 1 & Robustness == 0) {
  ggsave_default(fig_hp, paste0("./result/figure/fig_IRF_hp_", confidence_level*100, "CI.pdf"),
                 width = 30, height = 40
  )
} else if (Robustness == 1) {
  ggsave_default(fig_hp, paste0("./result/robustness/r1/figure/fig_IRF_hp_", confidence_level*100, "CI.pdf"),
                 width = 30, height = 40
  )
} else if (Robustness == 2) {
  ggsave_default(fig_hp, paste0("./result/robustness/r2/figure/fig_IRF_hp_", confidence_level*100, "CI.pdf"),
                 width = 30, height = 40
  )
} else if (Robustness == 3) {
  ggsave_default(fig_hp, paste0("./result/robustness/r3/figure/fig_IRF_hp_", confidence_level*100, "CI.pdf"),
                 width = 30, height = 40
  )
} else if (Robustness == 4) {
  ggsave_default(fig_hp, paste0("./result/robustness/r4/figure/fig_IRF_hp_", confidence_level*100, "CI.pdf"),
                 width = 30, height = 40
  )
}


