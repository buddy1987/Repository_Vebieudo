         avg = mean(Stage),  # calculate mean value for each day
  mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
