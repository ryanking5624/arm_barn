setwd("~/GitHub/arm_barn")
source("header.R")

prob <- read_delim("prob.txt", delim = ",", col_names = FALSE)
colnames(prob) <- c("team", "inning", "outs", "base_situation", "score_difference", "n", "n_won")
prob <- prob %>%
  mutate(win_prob = n_won / n)

write_csv(prob, file = "export/a/a01b.csv")
