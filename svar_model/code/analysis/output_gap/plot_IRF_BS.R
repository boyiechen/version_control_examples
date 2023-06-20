rm(list = setdiff(ls(), c("shock_sign", "confidence_level", "VAR.P", "Robustness")))

# Load graph setting
source("./code/utility/preamble.R")
num_var <- 4
confidence_level = 0.95
source("./code/utility/utils.R")

# Load data
df_IRF_plot <- readRDS("./data/intermediate_result/df_IRF_plot_outputgap.RDS")
df_IRF.sim <- read_rds(paste0("./data/intermediate_result/df_IRF_outputgap.sim_hrz40_", confidence_level*100, "CI.rds"))

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
shock_sign = 1 
cat(">>> shock sign is: ", shock_sign, "\n")

df_IRF_plot.BS.L <- (df_IRF_plot.BS.L*shock_sign) %>% as_tibble()
df_IRF_plot.BS.U <- (df_IRF_plot.BS.U*shock_sign) %>% as_tibble()
df_IRF_plot.BS.Median <- (df_IRF_plot.BS.Median*shock_sign) %>% as_tibble()
df_IRF_plot.BS.Mean <- (df_IRF_plot.BS.Mean*shock_sign) %>% as_tibble()
df_IRF_plot <- (df_IRF_plot*shock_sign) %>% as_tibble()


# the combination of titles
title_list_response <- c("Real Output Gap", 
                         "Consumer Price Inflation",
                         "Real House Price",
                         "Short-term Interest Rate"
)
title_list_shock <- c("Real Output",
                      "Inflation",
                      "House Price",
                      "Monetary Policy" 
)

if (shock_sign == (-1)) {
  negative_label <- "Negative "
} else if (shock_sign == 1) {
  negative_label <- ""
}

title_list <- c(paste0("Reponse of ", title_list_response, " to ", negative_label, title_list_shock[1], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", negative_label, title_list_shock[2], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", negative_label, title_list_shock[3], " Shock"),
                paste0("Reponse of ", title_list_response, " to ", negative_label, title_list_shock[4], " Shock")
)

# making figures
ind <- 0
for(i in 1:num_var){ # i denotes the shock
  for(j in 1:num_var){ # j denotes the response variable
    ind <- ind+1
    nam <- paste("shock", i, "y", j, sep = '')
    assign(nam, bind_cols(df_IRF_plot.BS.L[ind], df_IRF_plot.BS.U[ind],
                          df_IRF_plot.BS.Median[ind], df_IRF_plot.BS.Mean[ind],
                          df_IRF_plot[ind])
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
## By shocks
## shock1: monetary policy
## shock2: real output
## shock3: user cost
## shock4: credit
## shock5: construction cost
## shock6: sentiment 
## shock7: house price 

for (i in 1:num_var) {
  plt_num <- paste0("p", ((1:num_var) + num_var*(i-1)), collapse = ', ')
  evalStr <- paste0("fig_temp <- make_multiple_plots(list(", plt_num, "), ncol = 2, nrow = 4)")
  eval(parse(text=evalStr))
  
  # save plot
  if (shock_sign == (-1)) {
    ggsave_default(fig_temp, paste0("./result/figure/fig_IRF_outputgap_shock", i, "_", confidence_level*100, "CI_neg.pdf"),
                   width = 30, height = 40
    ) 
  } else if (shock_sign == 1) {
    ggsave_default(fig_temp, paste0("./result/figure/fig_IRF_outputgap_shock", i, "_", confidence_level*100, "CI.pdf"),
                   width = 30, height = 40
    ) 
  }
  
}

