library(baseballr)
library(retrosheet)
library(ggplot2)
library(janitor)
library(lubridate)
library(tidyverse)


year <- 2019

cluster_weights <- data.frame(
  events = 0.5,
  mean_starter_flag = 2,
  mean_outs_enter = 1,
  mean_score_diff_enter = 1,
  mean_pitches_thrown = 1,
  max_pitches_thrown = 1,
  mean_batters_faced = 1,
  max_batters_faced = 1,
  mean_pct_l_batters_faced = 1,
  mean_pct_top_faced = 1/3,
  mean_pct_middle_faced = 1/3,
  mean_pct_bottom_faced = 1/3,
  mean_li_enter = 2
)

mlb_teams <- c("Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles",
               "Boston Red Sox", "Chicago Cubs", "Chicago White Sox",
               "Cincinnati Reds", "Cleveland Indians", "Colorado Rockies",
               "Detroit Tigers", "Houston Astros", "Kansas City Royals",
               "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins",
               "Milwaukee Brewers", "Minnesota Twins", "New York Mets",
               "New York Yankees", "Oakland Athletics", "Philadelphia Phillies",
               "Pittsburgh Pirates", "San Diego Padres", "San Francisco Giants",
               "Seattle Mariners", "St. Louis Cardinals", "Tampa Bay Rays",
               "Texas Rangers", "Toronto Blue Jays", "Washington Nationals")
retro_abbr <- c("ARI", "ATL", "BAL",
                "BOS", "CHN", "CHA",
                "CIN", "CLE", "COL",
                "DET", "HOU", "KCA",
                "ANA", "LAN", "MIA",
                "MIL", "MIN", "NYN",
                "NYA", "OAK", "PHI",
                "PIT", "SDN", "SFN",
                "SEA", "SLN", "TBA",
                "TEX", "TOR", "WAS")
mlb_dict <- data.frame(team_name = mlb_teams,
                       retro_abbr = retro_abbr)