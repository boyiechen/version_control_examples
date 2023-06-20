rm(list = ls())

# Load graph setting
source("./code/utility/preamble.R")
source("./code/utility/utils.R")

# Load IRF data
df_IRF_plot <- readRDS("./data/intermediate_result/df_IRF_plot.RDS")

# make plots
fig_list <- create_multiple_IRF_plot(df_IRF_plot, hrz + 1, c(paste0("V", 1:num_var^2)))

fig <- make_multiple_plots(fig_list, ncol = num_var, nrow = num_var)

ggsave_default(fig, "./result/figure/fig_IRF_all.pdf")

