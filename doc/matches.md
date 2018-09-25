# Match Analysis
### Digging through Five Thirty Eight's match data



```r
library(tidyverse)
library(tidymodels)
library(expappr)
library(ggthemes)
```


```r
spi_matches <- readRDS("./data/fivethirtyeight/spi_matches2018-09-22.RDS") %>% 
  mutate(match_id = row_number())
intl_rankings <- readRDS("./data/fivethirtyeight/intl_rankings2018-09-22.RDS")
club_rankings <- readRDS("./data/fivethirtyeight/club_rankings2018-09-22.RDS")
```

First, let's explore the data structure of each of these files


```r
spi_matches %>% 
  head() %>% 
  knitr::kable()
```



|date       | league_id|league                  |team1         |team2               |  spi1|  spi2|  prob1|  prob2| probtie| proj_score1| proj_score2| importance1| importance2| score1| score2|  xg1|  xg2| nsxg1| nsxg2| adj_score1| adj_score2| match_id|
|:----------|---------:|:-----------------------|:-------------|:-------------------|-----:|-----:|------:|------:|-------:|-----------:|-----------:|-----------:|-----------:|------:|------:|----:|----:|-----:|-----:|----------:|----------:|--------:|
|2016-08-12 |      1843|French Ligue 1          |Bastia        |Paris Saint-Germain | 51.16| 85.68| 0.0463| 0.8380|  0.1157|        0.91|        2.36|        32.4|        67.7|      0|      1| 0.97| 0.63|  0.43|  0.45|       0.00|       1.05|        1|
|2016-08-12 |      1843|French Ligue 1          |AS Monaco     |Guingamp            | 68.85| 56.48| 0.5714| 0.1669|  0.2617|        1.82|        0.86|        53.7|        22.9|      2|      2| 2.45| 0.77|  1.75|  0.42|       2.10|       2.10|        2|
|2016-08-13 |      2411|Barclays Premier League |Hull City     |Leicester City      | 53.57| 66.81| 0.3459| 0.3621|  0.2921|        1.16|        1.24|        38.1|        22.2|      2|      1| 0.85| 2.77|  0.17|  1.25|       2.10|       1.05|        3|
|2016-08-13 |      2411|Barclays Premier League |Burnley       |Swansea City        | 58.98| 59.74| 0.4482| 0.2663|  0.2854|        1.37|        1.05|        36.5|        29.1|      0|      1| 1.24| 1.84|  1.71|  1.56|       0.00|       1.05|        4|
|2016-08-13 |      2411|Barclays Premier League |Middlesbrough |Stoke City          | 56.32| 60.35| 0.4380| 0.2692|  0.2927|        1.30|        1.01|        33.9|        32.5|      1|      1| 1.40| 0.55|  1.13|  1.06|       1.05|       1.05|        5|
|2016-08-13 |      2411|Barclays Premier League |Southampton   |Watford             | 69.49| 59.33| 0.5759| 0.1874|  0.2367|        1.91|        1.05|        34.1|        30.7|      1|      1| 1.05| 0.22|  1.52|  0.41|       1.05|       1.05|        6|

#### Initial questions about the matches dataset:
- Does SPI change by match, or is it constant for each team across all matches?
- Does can I deduce home team from the columns? (Is home team = team1?)
- What are the nsxg columns and adj_score columns?
- Are second division matches captured? 

##### Leagues


```r
spi_matches %>% 
  group_by(league, league_id) %>% 
  summarise(num_matches = n()) %>% 
  arrange(league) %>% 
  knitr::kable()
```



|league                                   | league_id| num_matches|
|:----------------------------------------|---------:|-----------:|
|Argentina Primera Division               |      5641|         703|
|Australian A-League                      |      1948|         135|
|Austrian T-Mobile Bundesliga             |      1827|         312|
|Barclays Premier League                  |      2411|        1140|
|Belgian Jupiler League                   |      1832|         240|
|Brasileiro Série A                       |      2105|         760|
|Chinese Super League                     |      1979|         129|
|Danish SAS-Ligaen                        |      1837|         182|
|Dutch Eredivisie                         |      1849|         612|
|English League Championship              |      2412|        1109|
|English League One                       |      2413|         552|
|English League Two                       |      2414|         552|
|French Ligue 1                           |      1843|        1140|
|French Ligue 2                           |      1844|         760|
|German 2. Bundesliga                     |      1846|         612|
|German Bundesliga                        |      1845|         918|
|Greek Super League                       |      1884|         240|
|Italy Serie A                            |      1854|        1140|
|Italy Serie B                            |      1856|         814|
|Japanese J League                        |      1947|         147|
|Major League Soccer                      |      1951|         782|
|Mexican Primera Division Torneo Apertura |      1952|         320|
|Mexican Primera Division Torneo Clausura |      1975|         334|
|National Women's Soccer League           |      4582|         231|
|Norwegian Tippeligaen                    |      1859|         480|
|Portuguese Liga                          |      1864|         612|
|Russian Premier Liga                     |      1866|         480|
|Scottish Premiership                     |      2417|         426|
|South African ABSA Premier League        |      1983|         240|
|Spanish Primera Division                 |      1869|        1140|
|Spanish Segunda Division                 |      1871|         930|
|Swedish Allsvenskan                      |      1874|         480|
|Swiss Raiffeisen Super League            |      1879|         360|
|Turkish Turkcell Super Lig               |      1882|         611|
|UEFA Champions League                    |      1818|         346|
|UEFA Europa League                       |      1820|         349|
|United Soccer League                     |      2160|         561|

Great! We have second division data for England, France, Spain, Germany, and Italy upon first glance. It could be interesting to look at predicting promotion or relegation. 

##### SPI


```r
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
```

![plot of chunk unnamed-chunk-3](../graphs/matches//unnamed-chunk-3-1.png)

Okay, so the SPI is current as of the match and is changing. There also seem to be about an equal number of records with team num = 1 as there are for 2. Let's verify that team1 is the home team, real quick.


```r
spi_matches %>% 
  head(10) %>% 
  select(date, league, team1, team2)
```

```
## # A tibble: 10 x 4
##    date       league                  team1           team2               
##    <date>     <chr>                   <chr>           <chr>               
##  1 2016-08-12 French Ligue 1          Bastia          Paris Saint-Germain 
##  2 2016-08-12 French Ligue 1          AS Monaco       Guingamp            
##  3 2016-08-13 Barclays Premier League Hull City       Leicester City      
##  4 2016-08-13 Barclays Premier League Burnley         Swansea City        
##  5 2016-08-13 Barclays Premier League Middlesbrough   Stoke City          
##  6 2016-08-13 Barclays Premier League Southampton     Watford             
##  7 2016-08-13 Barclays Premier League Everton         Tottenham Hotspur   
##  8 2016-08-13 Barclays Premier League Crystal Palace  West Bromwich Albion
##  9 2016-08-13 French Ligue 1          Bordeaux        St Etienne          
## 10 2016-08-13 Barclays Premier League Manchester City Sunderland
```

I've confirmed via google that team1 was the home team for each of these games. I supposed there will be some games in the dataset played on neutral grounds as well. 

### Tidying Up
Given the nastiness cleaning with team numbers above, I'll probably want to create a tidy version of this dataset. What should that look like? 
- 2 Records for each game
- 1 "team" column
- Home or away indicator
- All metrics for team first, and then "opponent metrics"
- Win or Loss
- Should allow me to easily calculate difference between spi and opponent spi this way. Maybe I should just include the difference, not the actual opponent data?



```r
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
```

```
## # A tibble: 41,758 x 34
##    match_id date       league league_id team  opp_team  home score
##       <int> <date>     <chr>      <int> <chr> <chr>    <dbl> <int>
##  1        1 2016-08-12 Frenc…      1843 Bast… Paris S…     1     0
##  2        1 2016-08-12 Frenc…      1843 Pari… Bastia       0     1
##  3        2 2016-08-12 Frenc…      1843 AS M… Guingamp     1     2
##  4        2 2016-08-12 Frenc…      1843 Guin… AS Mona…     0     2
##  5        3 2016-08-13 Barcl…      2411 Hull… Leicest…     1     2
##  6        3 2016-08-13 Barcl…      2411 Leic… Hull Ci…     0     1
##  7        4 2016-08-13 Barcl…      2411 Burn… Swansea…     1     0
##  8        4 2016-08-13 Barcl…      2411 Swan… Burnley      0     1
##  9        5 2016-08-13 Barcl…      2411 Midd… Stoke C…     1     1
## 10        5 2016-08-13 Barcl…      2411 Stok… Middles…     0     1
## # ... with 41,748 more rows, and 26 more variables: opp_score <int>,
## #   diff_score <int>, match_result <chr>, match_points <dbl>, prob <dbl>,
## #   opp_prob <dbl>, prob_tie <dbl>, diff_prob <dbl>, spi <dbl>,
## #   opp_spi <dbl>, diff_spi <dbl>, proj_score <dbl>, opp_proj_score <dbl>,
## #   diff_proj_score <dbl>, importance <dbl>, opp_importance <dbl>,
## #   diff_importance <dbl>, xg <dbl>, opp_xg <dbl>, diff_xg <dbl>,
## #   nsxg <dbl>, opp_nsxg <dbl>, diff_nsxg <dbl>, adj_score <dbl>,
## #   opp_adj_score <dbl>, diff_adj_score <dbl>
```

```r
#saveRDS(spi_matches_tidy, "./data/spi_matches_tidy.RDS")
```


## Analysis Ideas
- Predicting promotion and relegation
- Classifying which games were played in neutral venues
- Predicting winners
