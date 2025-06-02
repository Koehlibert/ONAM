library(dplyr)
library(ggplot2)
load("benchmarking_results.rds")
res_df <- data.frame(matrix(unlist(results), byrow = TRUE, ncol = 3)) %>%
  mutate(p_inf = factor(p_inf))
colnames(res_df) <- c("n", "p_inf", "Time")
ggplot(res_df, aes(x = n, y = Time, color = p_inf)) + geom_point()
