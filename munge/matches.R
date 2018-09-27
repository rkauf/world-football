#' # Match Analysis
#' ### Digging through Five Thirty Eight's match data
#' 

#+ packages
library(tidyverse)
library(tidymodels)
library(expappr)
library(ggthemes)
library(lubridate)
library(ggrepel)

source("munge/helpers.R")

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
  theme_expapp() +
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
home_teams <- spi_matches %>%
  select(-contains("2")) %>% 
  mutate(home = 1)

names(home_teams) <- names(home_teams) %>% str_remove("[0-9]")

home_teams <- home_teams %>%
  select(match_id, date, league, league_id, team, home, spi, prob, proj_score, importance, score, xg, nsxg, adj_score)

away_teams <- spi_matches %>%
  select(-contains("1")) %>% 
  mutate(home = 0)

names(away_teams) <- names(away_teams) %>% str_remove("[0-9]")

away_teams <- away_teams %>%
  select(match_id, date, league, league_id, team, home, spi, prob, proj_score, importance, score, xg, nsxg, adj_score)

spi_matches_combined <- comb_home_away(home_teams, away_teams)

# Season is the main other piece of data I'd like to add at this time
spi_matches %>%
  mutate(month = lubridate::month(date, label = TRUE),
         year = lubridate::year(date)) %>% 
  count(year, month) %>% 
  ggplot(aes(month, n)) +
  geom_col() +
  theme_expapp() +
  ggtitle("Total Matches by Month") +
  scale_y_continuous(labels = scales::comma, name = "Num Matches")

spi_matches2 <- spi_matches %>% 
  mutate(season = get_season(date))

spi_matches_tidy <- spi_matches_combined %>% 
  mutate(season = get_season(date))
  
matches_past <- spi_matches_tidy %>% 
  filter(!is.na(score))

matches_future <- spi_matches_tidy %>% 
  anti_join(matches_past %>% distinct(match_id), by = "match_id") %>% 
  filter(date > lubridate::today())

#saveRDS(spi_matches_tidy, "./data/fivethirtyeight/spi_matches_tidy.RDS")
  
#' #### Tidy Summary
#' Wow, tidying this data up was much more difficult than expected, but I think I'm a good place now with it. Each match will have 2 records (one for each team). This will make it easier to filter by team and quickly analyze wins / losses and compare to the opponent. New fields:
#' - `home` - 1 (home) or 0 (away)
#' - `diff` - column for every metric (team's minus opponent's)
#' - `result` - win, loss, or tie
#' - `points` - 3 for a win, 1 for a tie, 0 for a loss
#' - `season` - factors from 2014-15 to 2021-22

#' # Exploratory Data Analysis
#' ### Recreating League Tables

#' An interesting wrinkle is that some of these leagues are actually knockout tournaments, and we also have some match data for current seasons. I'd like to have a league reference table where I can quickly say whether a season is finished and whether it was a knockout tournament.
#' 

matches_past %>% 
  group_by(league, season, team) %>% 
  summarise(games_played = n_distinct(match_id)) %>% 
  ungroup() %>%
  group_by(league, season) %>% 
  summarise(max_team_gms = max(games_played),
            min_team_gms = min(games_played)) %>% 
  filter(max_team_gms != min_team_gms) %>% 
  head()

#' Hmmmmm I'd hoped that looking at the descrepancy between max and min games played would tell me whether it was a tournament, but alot of genuine leagues have discrepancies in number of games played. I may just have to hard code that in. 

season_results <- matches_past %>%
  filter(!(season %in% unique(matches_future$season))) %>% 
  group_by(league, season, team ) %>% 
  summarise(game_played = n(),
            points = sum(match_points),
            goal_diff = sum(diff_score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(league, season) %>% 
  mutate(league_position = row_number(-points)) %>% 
  arrange(league, season, -points, -goal_diff)

#' ### EPL 17-18 Final Standings
#+ epl1718, echo = F
season_results %>% 
  filter(league == "Barclays Premier League" & season == "2017-18") %>%
  ungroup() %>% 
  select(-league, -season) %>% 
  knitr::kable()

#+ big_leagues, fig.height = 8, fig.width = 12
big_leagues <- c("Barclays Premier League", "French Ligue 1", "German Bundesliga", "Portuguese Liga", "Spanish Primera Division", "Italy Serie A") # leaving out champions league given tournament style

big_league_matches <- matches_past %>%
  filter(league %in% big_leagues, !(season %in% unique(matches_future$season))) ## removing incomplete seasons

season_results %>% 
  #filter(league %in% big_leagues) %>% 
  group_by(league, season) %>% 
  mutate(pts_ahead = points - lead(points),
         team_season = paste(team,season)) %>% 
  filter(league_position == 1,game_played >= 20) %>% ## should get rid of junk seasons and tourney
  ggplot(aes(reorder(team_season, pts_ahead), pts_ahead, fill = league %in% big_leagues)) +
  geom_col() +
  coord_flip() +
  ggtitle("Winning Team Margin of Victory by Season") +
  ylab("Margin of Victory - Points") + 
  xlab("Team") +
  theme_expapp() +
  scale_fill_discrete(name = "In big league?")

#+ win_prob_home
matches_past %>% 
  group_by(league, season, home) %>% 
  summarise(matches_played = n_distinct(match_id),
            med_win_prob = median(prob),
            mean_win_prob = mean(prob)) %>% 
  filter(home == 1, matches_played > 25) %>% select(-home) %>% 
  mutate(league_season = paste(league, season)) %>% 
  ggplot(aes(matches_played, med_win_prob)) +
  geom_jitter(aes(color = season),size = 5, alpha = 0.7) +
  xlab("Matches Played") +
  ylab("Median Home Win %") +
  ggtitle("Expected Home Team Win Probability by Season") +
  theme_expapp() +
  scale_y_continuous(labels = scales::percent)


matches_past %>% 
  group_by(league, season, home) %>% 
  summarise(matches_played = n_distinct(match_id),
            med_win_prob = median(prob),
            mean_win_prob = mean(prob)) %>% 
  filter(home == 1, matches_played > 25) %>% select(-home) %>% 
  mutate(league_season = paste(league, season)) %>% 
  arrange(-med_win_prob) %>% 
  head(10) %>% 
  knitr::kable()

#' Interesting, seems like there is a bit of a trend with MLS and the Brasilian league, fivethirtyeight's model expects them to win at home more often than other leagues. Next I'll compare this to reality - do they actually win at home more often?

matches_past %>% 
  group_by(league, season, home) %>% 
  summarise(matches_played = n_distinct(match_id),
            med_win_prob = median(prob),
            mean_win_prob = mean(prob),
            actual_win_home = mean(match_result == "win")) %>% 
  filter(home == 1, matches_played > 25) %>% 
  mutate(act_less_exp = actual_win_home - med_win_prob) %>% 
  ggplot(aes(act_less_exp)) +
  geom_histogram(bins = 20, color = "white") +
  theme_expapp() +
  scale_x_continuous(labels = scales::percent, name = "Actual Win % minus expected win %") +
  ylab("Count of League Seasons") +
  ggtitle("Difference between Actual Home Win % and Expected", "By season, by league")

#+ perf_v_expected, fig.height = 5, fig.width = 8
league_seson_df <- matches_past %>% 
  filter(!(season %in% unique(matches_future$season)), league %in% big_leagues) %>% 
  mutate(league_season = paste(league,season)) %>% 
  distinct(league_season) %>% 
  arrange(league_season)

for (l in unique(league_seson_df$league_season)) {
  perf_plot <- matches_past %>% 
    mutate(league_season = paste(league, season)) %>% 
    filter(!(season %in% unique(matches_future$season)), league_season == l) %>% 
    inner_join(season_results, by = c("season", "league", "team")) %>% 
    group_by(league_season, league, season, team, league_position) %>% 
    summarise(matches_played = n_distinct(match_id),
              expected_win_pct = mean(prob),
              actual_win_pct = mean(match_result == "win")) %>% 
    mutate(win_pct_diff = actual_win_pct - expected_win_pct,
           winner = league_position == 1) %>% 
    filter(matches_played > 15) %>% 
    ggplot(aes(expected_win_pct, win_pct_diff, label = team, color = winner)) +
    geom_jitter(size = 3, alpha = 0.8, show.legend = FALSE) +
    scale_color_manual(values = c("black", "darkgreen")) +
    geom_text_repel(show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme_expapp() +
    scale_x_continuous(labels = scales::percent, name = "Expected Win %") +
    scale_y_continuous(labels = scales::percent, name = "Actual - Expected") +
    ggtitle(paste(l, "Expected Win Pct vs Actual"), "Overperformers above red line, underperformers below")
  
  print(perf_plot)
  
}




#' 
#' ## Analysis Ideas
#' - Predicting promotion and relegation
#' - Classifying which games were played in neutral venues
#' - Overperformers and underperformers
#' - Predicting winners
#' - How does time between previous game affect team performance?
