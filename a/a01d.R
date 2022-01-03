setwd("~/arm_barn")
source("header.R")

# import data ----
prob <- read_csv("a01b.csv")
all2019 <- read_csv("~/download.folder/unzipped/all2019.csv")

# recalculate win probs with logistic regression ----
win_prob_model <- prob %>%
  mutate(base_situation = factor(base_situation)) %>%
  glm(win_prob ~ team*inning*outs + base_situation + score_difference,
      data = .,
      family = "binomial",
      weights = n)

prob <- prob %>%
  mutate(base_situation = factor(base_situation),
         win_prob= predict.glm(win_prob_model, type = "response"),
         base_situation = as.numeric(as.character(base_situation)))

# combine datasets ----
combined <- all2019 %>%
  mutate(team = if_else(bat_home_id == 1, "H", "V"),
         inning = inn_ct,
         outs = outs_ct,
         base_situation = case_when(is.na(base1_run_id) & is.na(base2_run_id) & is.na(base3_run_id) ~ 1,
                                    !is.na(base1_run_id) & is.na(base2_run_id) & is.na(base3_run_id) ~ 2,
                                    is.na(base1_run_id) & !is.na(base2_run_id) & is.na(base3_run_id) ~ 3,
                                    !is.na(base1_run_id) & !is.na(base2_run_id) & is.na(base3_run_id) ~ 4,
                                    is.na(base1_run_id) & is.na(base2_run_id) & !is.na(base3_run_id) ~ 5,
                                    !is.na(base1_run_id) & is.na(base2_run_id) & !is.na(base3_run_id) ~ 6,
                                    is.na(base1_run_id) & !is.na(base2_run_id) & !is.na(base3_run_id) ~ 7,
                                    TRUE ~ 8),
         score_difference = if_else(bat_home_id == 1,
                                    home_score_ct - away_score_ct,
                                    away_score_ct - home_score_ct)) %>%
  group_by(game_id) %>%
  mutate_at(vars(team:score_difference), .funs = list(lead = ~lead(.))) %>%
  ungroup() %>%
  mutate(inning_delta = inning_lead - inning,
         outs_delta = event_outs_ct,
         base_situation_delta = if_else(team == team_lead,
                                        base_situation_lead - base_situation,
                                        NA_real_),
         score_difference_delta = if_else(team == team_lead,
                                          score_difference_lead - score_difference,
                                          score_difference_lead + score_difference)) %>%
  left_join(prob %>%
              select(-n, -n_won)) %>%
  left_join(prob %>%
              select(-n, -n_won) %>%
              rename_all(.funs = ~paste0(.x, "_lead"))) %>%
  mutate(win_prob_lead = if_else(is.na(win_prob_lead),
                                 if_else(outs + outs_delta == 3,
                                         0,
                                         1),
                                 win_prob_lead),
         win_prob_delta = if_else(team == team_lead | is.na(team_lead),
                                  win_prob_lead - win_prob,
                                  win_prob_lead - (1 - win_prob)))

# calculate empirical leverage index
rates <- combined %>%
  group_by(team, inning, outs, base_situation, score_difference, win_prob_delta) %>%
  summarize(obs = n()) %>%
  mutate(prob = obs / sum(obs),
         li_num = weighted.mean(abs(win_prob_delta), prob)) %>%
  ungroup() %>%
  mutate(li_denom = weighted.mean(abs(win_prob_delta), obs)) %>%
  group_by(team, inning, outs, base_situation, score_difference) %>%
  summarize(li = li_num / li_denom) %>%
  ungroup() %>%
  distinct()

# bind back onto original data ----
final_data <- combined %>%
  left_join(rates) %>%
  select(game_id:year, li)

write_rds(final_data, "a01d.rds")
