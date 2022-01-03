setwd("~/arm_barn")
source("header.R")

tran <- read_rds("a02a.rds") %>%
  filter(player_id != "") %>%
  mutate(type_flag = case_when(grepl(" transferred ", note) ~ "Still Inactive",
                               grepl(" activated ", note) ~ "Activated",
                               grepl(" recalled ", note) ~ "Activated",
                               grepl(" selected the contract ", note) ~ "Activated",
                               grepl(" assigned to .* from ", note) ~ "Activated",
                               grepl(" sent ", note) ~ "Deactivated",
                               grepl(" released ", note) ~ "Deactivated",
                               grepl(" reassigned ", note) ~ "Deactivated",
                               grepl(" placed ", note) ~ "Deactivated",
                               grepl(" optioned ", note) ~ "Deactivated",
                               grepl(" designated ", note) ~ "Deactivated",
                               grepl(" returned to ", note) ~ "Deactivated",
                               grepl(" retired", note) ~ "Deactivated",
                               grepl(" elected free agency", note) ~ "Deactivated",
                               grepl(" claimed ", note) ~ "Team Change",
                               grepl(" traded ", note, ignore.case = TRUE) ~ "Team Change",
                               grepl(" signed ", note) ~ "Team Change",
                               grepl(" assigned ", note) ~ "Team Change",
                               grepl(" roster status changed ", note) ~ "Roster Status Change",
                               grepl(" invited ", note) ~ "Roster Status Change",
                               TRUE ~ "FIX ME"),
         relevant_team = if_else(team %in% mlb_teams,
                                 team,
                                 from_team),
         relevant_date = if_else(effective_date == (trans_date + 1),
                                 effective_date,
                                 trans_date))

pbp <- read_rds("a01e.rds")

pit_ids <- pbp %>%
  pull(pit_id) %>%
  unique()

# retro to mlbam ----
retro_to_mlbam <- function(id){
  baseballr:: playername_lookup(id) %>%
    pull(key_mlbam) %>%
    return()
}

# create dictionary for retro to mlbam ----
update_ids <- FALSE # toggle to true after uploading new play-by-play data, although run is faster than expected (under 5 mins)
if(update_ids){
  id_dict <- data.frame(key_retro = pit_ids,
                        key_mlbam = NA)
  
  for(i in 1:nrow(id_dict)){
    id_dict[i, "key_mlbam"] <- retro_to_mlbam(id_dict %>%
                                                slice(i) %>%
                                                pull(key_retro))
    print(i/nrow(id_dict))
    Sys.sleep(0.1)
  }
  write_rds(id_dict, "a02b_id_dict.rds")
}else{
  id_dict <- read_rds("a02b_id_dict.rds")
}

start_date <- pbp %>% pull(game_date) %>% min()
end_date <- pbp %>% pull(game_date) %>% max()

roster_template <- data.frame(date = rep(seq.Date(start_date, end_date, "day"), length(pit_ids)) %>%
                                sort(),
                             key_retro = rep(id_dict %>%
                                               pull(key_retro),
                                             length(seq.Date(start_date, end_date, "day"))),
                             key_mlbam = rep(id_dict %>%
                                               pull(key_mlbam) %>%
                                               as.character(),
                                             length(seq.Date(start_date, end_date, "day"))))

roster_temp <- roster_template %>%
  left_join(pbp %>%
              select(key_retro = pit_id,
                     date = relevant_date) %>%
              distinct() %>%
              mutate(pitch = 1)) %>% 
  mutate(pitch = replace_na(pitch, 0)) %>% 
  left_join(tran %>%
              select(key_mlbam = player_id,
                     date = trans_date,
                     type_flag,
                     team = relevant_team)) %>%
  arrange(key_retro)

roster_temp_2 <- roster_temp %>% 
  left_join(roster_temp %>%
              right_join(roster_temp %>%
                           filter(type_flag == "Roster Status Change") %>%
                           mutate(rsc = 1) %>%
                           select(key_retro, rsc) %>% 
                           distinct()) %>%
              select(-rsc) %>%
              filter(!is.na(type_flag)) %>%
              group_by(key_retro) %>%
              mutate(rsc = case_when(type_flag == "Roster Status Change" & !is.na(lag(type_flag)) ~ if_else(lag(type_flag) == "Activated",
                                                                                                            "Deactivated",
                                                                                                            "Activated"), 
                                     type_flag == "Roster Status Change" & !is.na(lead(type_flag)) ~ if_else(lead(type_flag) == "Activated",
                                                                                                             "Deactivated",
                                                                                                             "Activated"), 
                                     type_flag == "Roster Status Change" ~ "SINGLE",
                                     TRUE ~ NA_character_))) %>%
  mutate(type_flag_fill = case_when(!is.na(rsc) ~ rsc,
                                    type_flag == "Team Change" ~ "Deactivated",
                                    TRUE ~ type_flag),
         team_fill = team) 

roster_daily <- roster_temp_2 %>% 
  left_join(roster_temp_2 %>%
              arrange(date) %>% 
              group_by(key_retro) %>% 
              mutate(team_change_cumsum = cumsum(if_else(!is.na(type_flag) & type_flag == "Team Change",
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
                        n_pitch = n_distinct(pitch_cumsum),
                        .groups = "drop") %>% 
              filter(n_activated == 1,
                     n_pitch > 1) %>% 
              mutate(active_flag_new = 1) %>% 
              select(key_retro,
                     date = team_change_date,
                     active_flag_new)) %>% 
  left_join(roster_temp_2 %>%
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
                        date = min(date),
                        .groups = "drop") %>% 
              filter(error_handling == 1,
                     n_pitch > 1) %>% 
              select(-n_pitch)) %>% 
  mutate(active_flag_new = if_else(!is.na(error_handling) & error_handling == 1,
                                   1,
                                   active_flag_new),
         active_flag_new = replace_na(active_flag_new, 0),
         type_flag_fill = if_else(active_flag_new == 1,
                                  "Activated",
                                  type_flag_fill)) %>%
  select(-error_handling) %>% 
  group_by(key_retro) %>%
  fill(type_flag_fill, .direction = "down") %>%
  fill(team_fill, .direction = "down") %>%
  mutate(type_flag_fill = case_when(is.na(type_flag_fill) & lead(type_flag_fill) == "Activated" & cumsum(pitch) == 0 ~ "Deactivated",
                                    is.na(type_flag_fill) & lead(type_flag_fill) == "Deactivated" ~ "Activated",
                                    TRUE ~ type_flag_fill)) %>% 
  fill(type_flag_fill, .direction = "up") %>%
  fill(team_fill, .direction = "up") %>%
  mutate(type_flag_fill = replace_na(type_flag_fill, "Activated"),
         type_flag_fill = if_else(type_flag_fill == "Deactivated" & pitch == 1 & !is.na(type_flag),
                                  "Activated",
                                  type_flag_fill)) %>%
  ungroup()

roster_daily_full <- roster_daily %>% 
  left_join(roster_daily %>%
              filter(is.na(team_fill)) %>%
              left_join(pbp %>%
                          mutate(pit_team_id = if_else(bat_home_id == 1,
                                                       away_team_id,
                                                       home_team_id)) %>%
                          select(key_retro = pit_id,
                                 pit_team_id) %>%
                          distinct() %>%
                          left_join(mlb_dict,
                                    by = c("pit_team_id" = "retro_abbr")))) %>%
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
              select(key_retro, date, type_flag_alt)) %>% 
  mutate(team_fill = if_else(is.na(team_fill), team_name, team_fill),
         type_flag_alt = replace_na(type_flag_alt, ""),
         status = if_else(type_flag_fill == "Activated" | type_flag_alt == "Activated",
                          "Active",
                          "Inactive")) %>%
  select(date, key_retro, key_mlbam, status, team = team_fill, pitch)


active_roster <- roster_daily_full %>%
  filter(status == "Active")

error_ids <- roster_daily_full %>%
  filter(status == "Inactive", pitch == 1) %>% 
  select(key_mlbam, key_retro) %>% 
  distinct()

roster_daily %>% filter(key_retro %in% error_ids) %>% pull(type_flag) %>% table()

roster_daily %>% filter(key_mlbam == "543278", !is.na(type_flag))

View(tran %>% filter(player_id == "543278"))

# need exception handling for players who are missing activation after IL stint
# ^ more generally, we should treat a player's activation date as the first date they pitch, unless it is preceded by an activation transaction

# "eliar001" team change - pitch - deactivation
# "gonzg003" team change - pitch - deactivation

# seems like the problem is players with deactivations post-trade and pre-activation

# 543278 IL, rehab, *pitch*, option, IL
# 452657 IL, pitch

