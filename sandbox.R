roster_temp_2 %>%
  arrange(date) %>% 
  group_by(key_retro) %>% 
  mutate(team_change_cumsum = cumsum(if_else(!is.na(type_flag) & type_flag == "Team Change",
                                             1,
                                             0)),
         deactivated_cumsum = cumsum(if_else(!is.na(type_flag) & type_flag == "Deactivated",
                                             1,
                                             0)),
         activated_cumsum = cumsum(if_else(!is.na(type_flag) & type_flag == "Activated",
                                           1,
                                           0)),
         pitch_cumsum = cumsum(pitch)) %>%
  filter(team_change_cumsum > 0) %>% 
  group_by(key_retro, team_change_cumsum) %>% 
  summarize(team_change_date = min(date),
            n_activated = n_distinct(activated_cumsum),
            n_deactivated = n_distinct(deactivated_cumsum),
            n_pitch = n_distinct(pitch_cumsum),
            .groups = "drop") %>% 
  filter(key_retro == "eliar001")

roster_temp_2 %>%
  filter(!is.na(type_flag)) %>% 
  group_by(key_retro) %>% 
  mutate(error_handling = case_when(type_flag == "Team Change" & lead(type_flag == "Deactivated") ~ 1,
                                    !is.na(type_flag) ~ -1,
                                    TRUE ~ 0)) %>% 
  right_join(roster_temp_2) %>% 
  group_by(key_retro) %>% 
  arrange(key_retro, date) %>% 
  fill(error_handling, .direction = "down") %>% 
  mutate(error_handling = replace_na(error_handling, 0),
         pitch_cumsum = cumsum(pitch)) %>% 
  group_by(key_retro, error_handling) %>% 
  summarize(n_pitch = n_distinct(pitch_cumsum),
            .groups = "drop") %>% 
  filter(error_handling == 1,
         n_pitch > 1) %>% 
  select(-n_pitch)

roster_temp_2 %>% 
  filter(pitch == 1 | type_flag_fill == )

roster_daily %>% 
  mutate(type_flag_alt = if_else(type_flag_fill == "Deactivated" & pitch == 1,
                                 "Activated",
                                 type_flag)) %>% 
  fill(type_flag_alt, .direction = "down") %>% 
  filter(!is.na(type_flag_alt), type_flag_alt == "Activated", type_flag_fill == "Deactivated")

roster_daily %>%
  group_by(key_retro) %>% 
  mutate(transaction_count = cumsum(!is.na(type_flag))) %>% 
  group_by(key_retro, transaction_count) %>% 
  mutate(type_flag_alt = if_else(type_flag_fill == "Deactivated" & pitch == 1,
                                 1,
                                 0),
         pitch_cumsum = cumsum(pitch)) %>% 
  group_by(key_retro, transaction_count, pitch_cumsum) %>%
  summarize()

roster_daily %>%  
  filter(!is.na(type_flag)) %>% 
  mutate(transaction_flag = 1) %>% 
  group_by(key_retro) %>% 
  arrange(date) %>% 
  mutate(transaction_group = row_number()) %>% 
  right_join(roster_daily) %>% 
  arrange(date) %>% 
  fill(transaction_group, .direction = "down") %>% 
  mutate(transaction_group = replace_na(transaction_group, 0)) %>% 
  group_by(key_retro, transaction_group, pitch, type_flag_fill) %>% 
  summarize(min_date = min(date)) %>% 
  filter(pitch == 1, type_flag_fill == "Deactivated") %>% 
  left_join(roster_daily %>% 
              filter(!is.na(type_flag)) %>% 
              mutate(transaction_flag = 1) %>% 
              group_by(key_retro) %>% 
              arrange(date) %>% 
              mutate(transaction_group = row_number()) %>% 
              right_join(roster_daily) %>% 
              arrange(date) %>% 
              fill(transaction_group, .direction = "down") %>% 
              mutate(transaction_group = replace_na(transaction_group, 0)) %>% 
              group_by(key_retro, transaction_group, type_flag_fill) %>% 
              summarize(max_date = max(date))) %>% 
  expand(key_retro, min_date:max_date) %>% 
  ungroup() %>% 
  mutate(date = as_date(`min_date:max_date`),
         type_flag_alt = "Activated") %>% 
  select(key_retro, date, type_flag_alt)
  
