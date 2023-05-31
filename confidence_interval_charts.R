mean <-  c(0.3149, 0.2896)
std_err <- c(0.00498, 0.00614)

predictions <- bind_cols(mean, std_err)

predictions <- bind_rows(
          names = c("elastic net", "random forest"),
          mean = c(0.3149, 0.2896), 
          std_err = c(0.00498, 0.00614))
save(mean, std_err, file = "data/predictions.rda")

predictions <- load(file = "data/predictions.rda") %>% as.data.frame()


ci_plot %>% 
  ggplot(aes(x = .metric, y = model)) +
  geom_col() +
  geom_errorbar(aes(xmin = .metric - 1.96*std_err), xmax = .metric +1.96*std_err)



ci_plot <- predictions %>% 
  ggplot(aes(x = mean, y = names)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = mean - 1.96*std_err), xmax = mean +1.96*std_err) +
  labs(x = NULL,
       y = "rmse") +
  theme_minimal()


save(ci_plot, file = "ci_plot.rda")
