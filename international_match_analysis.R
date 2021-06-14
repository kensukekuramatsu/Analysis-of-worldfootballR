library(worldfootballR)
library(tidyverse)


euro_2016_match_urls <- get_match_urls(country = "ENG", gender = "M", season_end_year = 2016, tier="1st", non_dom_league_url = "https://fbref.com/en/comps/676/UEFA-Euro-Stats")


euro_2020_results <- get_match_results(country = "", gender = "M", season_end_year = 2021, tier = "", non_dom_league_url = "https://fbref.com/en/comps/676/history/European-Championship-Seasons")


# for international friendlies:
test <-  get_match_results(country = "", gender = "M", season_end_year = 2018, tier = "", non_dom_league_url = "https://fbref.com/en/comps/218/history/Friendlies-M-Seasons")



euro_2016_match_urls <- get_match_urls(country = "", gender = "M", season_end_year = 2016, non_dom_league_url = "https://fbref.com/en/comps/676/history/European-Championship-Seasons")

euro_2016_events <- get_match_summary(euro_2016_match_urls)


euro_2016_results <- get_match_results(country = "", gender = "M", season_end_year = 2016, tier = "", non_dom_league_url = "https://fbref.com/en/comps/676/history/European-Championship-Seasons")



wc_2018_urls <- get_match_urls(country = "", gender = "M", season_end_year = 2018, tier = "", non_dom_league_url = "https://fbref.com/en/comps/1/history/World-Cup-Seasons")
test <- get_advanced_match_stats(match_url = wc_2018_urls[1], stat_type = "summary", team_or_player = "team")



a <- get_match_results(country = "", gender = "M", season_end_year = 2019, non_dom_league_url = "https://fbref.com/en/comps/685/history/Copa-America-Seasons")



friendly_ints <- get_match_urls(country = "", gender = "M", season_end_year = 2021, tier = "", non_dom_league_url = "https://fbref.com/en/comps/218/history/Friendlies-M-Seasons")



aa <- worldfootballR::get_match_report(match_url = friendly_ints[1:3])

                                                                   