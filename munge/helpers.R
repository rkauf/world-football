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
  
}