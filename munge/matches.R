#' # Match Analysis
#' ### Digging through Five Thirty Eight's match data
#' 

#+ packages
library(tidyverse)
library(tidymodels)
library(expappr)
library(ggthemes)

#+settings, echo = F
knitr::opts_chunk$set(dpi = 200, fig.width = 8, fig.height = 5, message = F, warning = F)
#ezspin_pt(file_name = "matches", project_directory = ".", file_folder = "munge", keep_html = F)

#+ download_data
spi_matches <- readRDS("./data/fivethirtyeight/spi_matches2018-09-22.RDS") %>% 
  mutate(match_id = row_number())
intl_rankings <- readRDS("./data/fivethirtyeight/intl_rankings2018-09-22.RDS")
club_rankings <- readRDS("./data/fivethirtyeight/club_rankings2018-09-22.RDS")

#' First, let's explore the data structure of each of these files

spi_matches %>% 
  head() %>% 
  knitr::kable()

#' #### Initial questions about the matches dataset:
#' - Does SPI change by match, or is it constant for each team across all matches?
#' - Does can I deduce home team from the columns? (Is home team = team1?)
#' - What are the nsxg columns and adj_score columns?
#' - Are second division matches captured? 
#' 
#' ##### Leagues
spi_matches %>% 
  group_by(league, league_id) %>% 
  summarise(num_matches = n()) %>% 
  arrange(league) %>% 
  knitr::kable()

#' Great! We have second division data for England, France, Spain, Germany, and Italy upon first glance. It could be interesting to look at predicting promotion or relegation. 
#' 
#' ##### SPI
spi_matches %>% 
  select(match_id, date, team1, team2, spi1, spi2) %>%
  gather(team_num, team, team1:team2) %>% 
  gather(spi_num, spi, spi1:spi2) %>% 
  mutate(team_num = str_remove_all(team_num, "[^0-9]"),
         spi_num = str_remove_all(spi_num, "[^0-9]")) %>% 
  filter(team_num == spi_num, team == "Manchester City") %>% 
  ggplot(aes(date, spi)) +
  geom_point(aes(color = team_num)) +
  theme_fivethirtyeight() +
  ggtitle("Manchester City's SPI Over Time")

#' Okay, so the SPI is current as of the match and is changing. There also seem to be about an equal number of records with team num = 1 as there are for 2. Let's verify that team1 is the home team, real quick.

spi_matches %>% 
  head(10) %>% 
  select(date, league, team1, team2)

#' I've confirmed via google that team1 was the home team for each of these games. I supposed there will be some games in the dataset played on neutral grounds as well. 
#' 
#' ### Tidying Up
#' Given the nastiness cleaning with team numbers above, I'll probably want to create a tidy version of this dataset. What should that look like? 
#' - 2 Records for each game
#' - 1 "team" column
#' - Home or away indicator
#' - All metrics for team first, and then "opponent metrics"
#' - Win or Loss
#' - Should allow me to easily calculate difference between spi and opponent spi this way. Maybe I should just include the difference, not the actual opponent data?
#' 

these_cols <- c("spi", "prob", "proj_score", "importance", "score", "xg", "nsxg", "adj_score")

home_teams <- spi_matches %>% 
  gather(key, val,team1:adj_score2) %>% 
  mutate(team_num = str_remove_all(key, "[^0-9]"),
         key_adj = str_remove_all(key, "[0-9]")) %>% 
  filter(team_num == 1) %>% 
  select(-team_num, -key) %>% 
  spread(key_adj, val) %>% 
  mutate(home = 1) %>% 
  select(match_id, date, league, league_id, team, home, spi, prob, proj_score, importance, score, xg, nsxg, adj_score) %>% 
  mutate_at(these_cols, as.double) %>% 
  mutate(score = as.integer(score))

away_teams <- spi_matches %>% 
  gather(key, val,team1:adj_score2) %>% 
  mutate(team_num = str_remove_all(key, "[^0-9]"),
         key_adj = str_remove_all(key, "[0-9]")) %>% 
  filter(team_num == 2) %>% 
  select(-team_num, -key) %>% 
  spread(key_adj, val) %>% 
  mutate(home = 0) %>% 
  select(match_id, date, league, league_id, team, home, spi, prob, proj_score, importance, score, xg, nsxg, adj_score) %>% 
  mutate_at(these_cols, as.double) %>% 
  mutate(score = as.integer(score))


 
comb_home_away <- function(first, second) {
  joinable <- function(df){
    new_df <- df %>% 
    select(match_id,
           opp_team = team,
           opp_spi = spi,
           opp_prob = prob,
           opp_proj_score = proj_score,
           opp_importance = importance,
           opp_score = score,
           opp_xg = xg,
           opp_nsxg = nsxg,
           opp_adj_score = adj_score)
    }
  
  calc_diffs <- function(df1, df2) {
    clean1 <- df1 %>% 
    inner_join(joinable(df2), by = "match_id") %>% 
    mutate(diff_score = score - opp_score,
           prob_tie = 1 - prob - opp_prob,
           diff_prob = prob - opp_prob,
           diff_spi = spi - opp_spi,
           diff_proj_score = proj_score - opp_proj_score,
           diff_importance = importance - opp_importance,
           diff_xg = xg - opp_xg,
           diff_nsxg = nsxg - opp_nsxg,
           diff_adj_score = adj_score - opp_adj_score
           ) %>% 
      mutate(match_result = case_when(diff_score == 0 ~ "tie",
                                    diff_score > 0 ~ "win",
                                    TRUE ~ "lose"),
           match_points = case_when(match_result == "win" ~ 3,
                                    match_result == "tie" ~ 1,
                                    TRUE ~ 0)) %>% 
      select(match_id, date, league, league_id, team, opp_team, home, score, opp_score, diff_score, match_result, match_points, prob, opp_prob, prob_tie, diff_prob, spi, opp_spi, diff_spi, proj_score, opp_proj_score, diff_proj_score, importance, opp_importance, diff_importance, xg, opp_xg, diff_xg, nsxg, opp_nsxg, diff_nsxg, adj_score, opp_adj_score, diff_adj_score)
  }
  
  final <- bind_rows(calc_diffs(first, second),
            calc_diffs(second, first)) %>% 
    arrange(match_id)
  print(final)
  
}

spi_matches_tidy <- comb_home_away(home_teams, away_teams)
#saveRDS(spi_matches_tidy, "./data/spi_matches_tidy.RDS")

#' Wow, tidying this data up was much more difficult than expected, but I think I'm a good place now with it. Each match will have 2 records (one for each team). This will make it easier to filter by team and quickly analyze wins / losses and compare to the opponent. New fields:
#' - `home` - 1 or 0
#' - "diff" column for every metric


#' 
#' ## Analysis Ideas
#' - Predicting promotion and relegation
#' - Classifying which games were played in neutral venues
#' - Predicting winners
