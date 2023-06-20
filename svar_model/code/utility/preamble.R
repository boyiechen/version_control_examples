library(ggplot2)

if (!exists("hrz")) { hrz <- 39 }
if (!exists("num_var")) { num_var <- 7 }

Text_Size_Theme <- theme(
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.title = element_text(size = 12),
  plot.title = element_text(size=12)
  )
