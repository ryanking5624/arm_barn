setwd("~/arm_barn")
source("header.R")

pbp <- read_rds("a01a.rds") %>% 
  mutate(game_date = case_when(game_id == "DET201905190" & (inn_ct > 7 | (inn_ct == 7 & bat_home_it == 1)) ~ "2019-09-06",
                               TRUE ~ str_sub(game_id, start = 4, end = -2)) %>%
           as_date(., "%Y%m%d"))
library(rvest)
susp <- read_html("https://www.retrosheet.org/suspend.htm") %>% 
  html_nodes("pre") %>%
  html_text() %>%
  str_split(., "\n") %>%
  unlist()

susp_table <- data.frame(matrix(ncol = 10, nrow = 0)) %>% 
  magrittr::set_colnames(susp[2] %>%
                           str_split(., " ") %>%
                           unlist() %>%
                           .[. != ""])

susp_clean <- susp %>%
  .[-c(1:2)] %>%
  .[-c((which(. == "") %>% min()):length(.))] %>%
  str_squish()

susp_table <- data.frame(GameDate = word(susp_clean, 1),
                         DH = if_else(word(susp_clean, 2) == "2",
                                      "2",
                                      "0"),
                         Vis = if_else(word(susp_clean, 2) == "2",
                                       word(susp_clean, 3),
                                       word(susp_clean, 2)),
                         Hom = if_else(word(susp_clean, 2) == "2",
                                       word(susp_clean, 4),
                                       word(susp_clean, 3)),
                         Lg = if_else(word(susp_clean, 2) == "2",
                                      word(susp_clean, 5),
                                      word(susp_clean, 4)),
                         V = if_else(word(susp_clean, 2) == "2",
                                     word(susp_clean, 6),
                                     word(susp_clean, 5)),
                         H = if_else(word(susp_clean, 2) == "2",
                                     word(susp_clean, 7),
                                     word(susp_clean, 6)),
                         Out = if_else(word(susp_clean, 2) == "2",
                                       word(susp_clean, 8),
                                       word(susp_clean, 7)),
                         Resumed = if_else(word(susp_clean, 2) == "2",
                                           word(susp_clean, 9),
                                           word(susp_clean, 8))) %>%
  mutate(GameDate = as_date(GameDate, format = "%m/%d/%Y"),
         Resumed = as_date(Resumed, format = "%m/%d/%Y"),
         game_id = paste0(Hom,
                          year(GameDate),
                          if_else(month(GameDate) < 10, "0", ""),
                          month(GameDate),
                          if_else(day(GameDate) < 10, "0", ""),
                          day(GameDate),
                          DH)) %>% 
  filter(year(GameDate) == year)
  
pbp_susp <- pbp %>% 
  mutate(total_outs = 6 * (inn_ct - 1) + (3 * bat_home_id) + outs_ct) %>%
  left_join(susp_table %>% 
              mutate(total_outs = as.numeric(Out)) %>%
              select(game_id,
                     total_outs,
                     resume_date = Resumed)) %>%
  group_by(game_id) %>%
  fill(resume_date, .direction = "down") %>% 
  ungroup() %>% 
  mutate(relevant_date = if_else(is.na(resume_date),
                                 game_date,
                                 resume_date))

write_rds(pbp_susp, "a01e.rds")
