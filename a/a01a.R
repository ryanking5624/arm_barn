setwd("~/arm_barn")
source("header.R")

all2019 <- read_csv("~/download.folder/unzipped/all2019.csv") %>%
  mutate(game_date = str_sub(game_id, start = 4, end = -2) %>%
           as.numeric(),
         game_number = str_sub(game_id, start = -1),
         home_team_id = str_sub(game_id, start = 1, end = 3))

write_rds(all2019, "a01a.rds")
