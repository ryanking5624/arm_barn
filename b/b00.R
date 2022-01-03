setwd("~/arm_barn")
source("header.R")

all2019 <- read_rds("a01a.rds")
all2019_li <- read_rds("a01d.rds")

all2019 <- all2019 %>%
  left_join(all2019_li)
rm(all2019_li)

is_starter <- all2019 %>%
  group_by(game_id, bat_home_id) %>%
  summarize(starter = first(pit_id)) %>%
  ungroup() %>%
  mutate(starter_flag = 1) %>%
  rename(pit_id = starter)

pbp_summary <- all2019 %>%
  left_join(is_starter) %>%
  mutate(starter_flag = replace_na(starter_flag, 0),
         game_outs = outs_ct + (3 * (inn_ct-1)),
         pitcher_home = if_else(bat_home_id == 1, 0, 1),
         pitches_thrown = nchar(pitch_seq_tx %>%
                                  gsub("N", "", .) %>%
                                  gsub("\\+", "", .) %>%
                                  gsub("\\*", "", .) %>%
                                  gsub("1", "", .) %>%
                                  gsub("2", "", .) %>%
                                  gsub("3", "", .) %>%
                                  gsub("\\>", "", .)),
         top_flag = if_else(bat_lineup_id %in% 1:3,
                            1,
                            0),
         middle_flag = if_else(bat_lineup_id %in% 4:6,
                               1,
                               0),
         bottom_flag = if_else(bat_lineup_id %in% 7:9,
                               1,
                               0)) %>%
  group_by(game_id, pit_id, starter_flag) %>%
  summarize(outs_enter = min(game_outs),
            outs_leave = max(game_outs),
            own_score_enter = min(if_else(bat_home_id == 1, away_score_ct, home_score_ct)),
            opp_score_enter = min(if_else(bat_home_id == 0, away_score_ct, home_score_ct)),
            own_score_leave = max(if_else(bat_home_id == 1, away_score_ct, home_score_ct)),
            opp_score_leave = max(if_else(bat_home_id == 0, away_score_ct, home_score_ct)),
            first_batter_position = first(bat_lineup_id),
            last_batter_position = last(bat_lineup_id),
            l_batters_faced = sum(if_else(bat_hand_cd == "L", 1, 0)),
            r_batters_faced = sum(if_else(bat_hand_cd == "R", 1, 0)),
            pitches_thrown = sum(pitches_thrown),
            li_enter = first(li),
            top_faced = sum(top_flag),
            middle_faced = sum(middle_flag),
            bottom_faced = sum(bottom_flag)) %>%
  mutate(outs_recorded = outs_leave - outs_enter,
         batters_faced = l_batters_faced + r_batters_faced,
         pct_l_batters_faced = l_batters_faced / (l_batters_faced + r_batters_faced),
         pct_top_faced = top_faced / batters_faced,
         pct_middle_faced = middle_faced / batters_faced,
         pct_bottom_faced = bottom_faced / batters_faced) %>%
  ungroup()

pitcher_breakdown <- pbp_summary %>%
  group_by(pit_id) %>%
  summarize(events = n(),
            mean_starter_flag = mean(starter_flag),
            mean_outs_enter = mean(outs_enter),
            mean_score_diff_enter = mean(own_score_enter - opp_score_enter),
            mean_pitches_thrown = mean(pitches_thrown),
            max_pitches_thrown = max(pitches_thrown),
            mean_batters_faced = mean(batters_faced),
            max_batters_faced = max(batters_faced),
            mean_pct_l_batters_faced = weighted.mean(pct_l_batters_faced, w = batters_faced),
            mean_pct_top_faced = weighted.mean(pct_top_faced, w = batters_faced),
            mean_pct_middle_faced = weighted.mean(pct_middle_faced, w = batters_faced),
            mean_pct_bottom_faced = weighted.mean(pct_bottom_faced, w = batters_faced),
            mean_li_enter = mean(li_enter))

write_rds(pitcher_breakdown, "b00.rds")

