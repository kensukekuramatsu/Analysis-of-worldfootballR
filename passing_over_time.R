library(worldfootballR)
library(tidyverse)
library(here)


passing <- readRDS(here("Euro-league-passing-differences", "passing.rds"))


leagues <- c("ENG", "ESP", "FRA", "GER", "ITA")

seasons <- c(2018:2021)

# saveRDS(urls, "big_five_urls.rds")
# 
# urls <- readRDS("big_five_urls.rds")



urls <- get_match_urls(country = leagues , gender = "M", season_end_year = seasons)
each_season_passing <- get_advanced_match_stats(match_url = urls, stat_type = "passing", team_or_player = "team")



passing <- data.frame()

for(i in leagues) {
  each_passing <- data.frame()
  for(j in seasons) {
    print(glue::glue("Scraping {i} for the {j} season end year"))
    urls <- get_match_urls(country = i , gender = "M", season_end_year = j)
    each_season_passing <- get_advanced_match_stats(match_url = urls, stat_type = "passing", team_or_player = "team")

    each_passing <- rbind(each_passing, each_season_passing)
  }
  passing <- rbind(passing, each_passing)

}


saveRDS(passing, "passing_data_top_5_2018-2021.rds")



missing_urls <- c("https://fbref.com/en/matches/f64b52a1/Granada-Atletico-Madrid-November-23-2019-La-Liga",
                  "https://fbref.com/en/matches/ba35b2c1/Real-Sociedad-Eibar-November-30-2019-La-Liga",
                  "https://fbref.com/en/matches/d56117e1/Valencia-Villarreal-November-30-2019-La-Liga",
                  "https://fbref.com/en/matches/b26f621a/Sevilla-Leganes-December-1-2019-La-Liga",
                  "https://fbref.com/en/matches/df721812/Athletic-Club-Granada-December-1-2019-La-Liga",
                  "https://fbref.com/en/matches/900b9aad/Getafe-Levante-December-1-2019-La-Liga",
                  "https://fbref.com/en/matches/bb58b88d/Granada-Alaves-December-7-2019-La-Liga",
                  "https://fbref.com/en/matches/cd2f5f59/Levante-Valencia-December-7-2019-La-Liga",
                  "https://fbref.com/en/matches/ff79bec1/Athletic-Club-Getafe-February-2-2020-La-Liga",
                  "https://fbref.com/en/matches/8ffb06f8/Sevilla-Alaves-February-2-2020-La-Liga",
                  "https://fbref.com/en/matches/2a83083f/Villarreal-Osasuna-February-2-2020-La-Liga",
                  "https://fbref.com/en/matches/df75eab1/Barcelona-Levante-February-2-2020-La-Liga",
                  "https://fbref.com/en/matches/e1c0d7cb/Alaves-Eibar-February-7-2020-La-Liga",
                  "https://fbref.com/en/matches/5ea18fd9/Levante-Leganes-February-8-2020-La-Liga",
                  "https://fbref.com/en/matches/20deb712/Getafe-Valencia-February-8-2020-La-Liga",
                  "https://fbref.com/en/matches/6cffc26b/Sevilla-Athletic-Club-May-3-2021-La-Liga",
                  "https://fbref.com/en/matches/41c0dde1/Bordeaux-Dijon-April-28-2018-Ligue-1",
                  "https://fbref.com/en/matches/4d052eeb/Strasbourg-Nice-April-28-2018-Ligue-1",
                  "https://fbref.com/en/matches/1f0aa2e3/Monaco-Amiens-April-28-2018-Ligue-1",
                  "https://fbref.com/en/matches/0aedef37/Troyes-Caen-April-28-2018-Ligue-1",
                  "https://fbref.com/en/matches/2b8af14e/Rennes-Toulouse-April-29-2018-Ligue-1",
                  "https://fbref.com/en/matches/014d494d/Angers-Marseille-April-29-2018-Ligue-1",
                  "https://fbref.com/en/matches/6263a735/Amiens-Paris-Saint-Germain-May-4-2018-Ligue-1",
                  "https://fbref.com/en/matches/9b533b91/Saint-Etienne-Bordeaux-May-6-2018-Ligue-1",
                  "https://fbref.com/en/matches/b68c74c2/Lyon-Troyes-May-6-2018-Ligue-1",
                  "https://fbref.com/en/matches/e8a4ec8e/Nantes-Montpellier-May-6-2018-Ligue-1",
                  "https://fbref.com/en/matches/78d9ed3d/Metz-Angers-May-6-2018-Ligue-1",
                  "https://fbref.com/en/matches/fa7ea650/Dijon-Guingamp-May-6-2018-Ligue-1",
                  "https://fbref.com/en/matches/81e16ef9/Crotone-Atalanta-February-10-2018-Serie-A",
                  "https://fbref.com/en/matches/d81176b3/Napoli-Lazio-February-10-2018-Serie-A",
                  "https://fbref.com/en/matches/ed6da260/Sassuolo-Cagliari-February-11-2018-Serie-A",
                  "https://fbref.com/en/matches/2107c205/Internazionale-Bologna-February-11-2018-Serie-A",
                  "https://fbref.com/en/matches/2918d98e/Chievo-Genoa-February-11-2018-Serie-A",
                  "https://fbref.com/en/matches/aec94eb1/Torino-Udinese-February-11-2018-Serie-A",
                  "https://fbref.com/en/matches/54b5e673/Sampdoria-Hellas-Verona-February-11-2018-Serie-A",
                  "https://fbref.com/en/matches/be76b113/Udinese-Roma-February-17-2018-Serie-A")



missing_passing <- get_advanced_match_stats(match_url = missing_urls, stat_type = "passing", team_or_player = "team")

passing <- rbind(passing, missing_passing)

saveRDS(passing, "passing_data_top_5_2018-2021.rds")





test <- passing %>% 
  separate(., Match_Date, into = c("Wday", "Month", "Day", "Year"), sep = " ", remove = F) %>% 
  mutate(Day = gsub(",", "", Day) %>% as.numeric(),
         Year = as.numeric(Year),
         Month = match(Month, month.name),
         Match_Date_Clean = lubridate::ymd(paste(Year, Month, Day, sep = "-")))



test <- test %>%
  arrange(Season, Team, Match_Date_Clean) %>% 
  group_by(Season, Team) %>% mutate(game_num = row_number()) %>% ungroup()



test %>% 
  filter(Season != "2020-2021") %>% 
  group_by(game_num) %>% 
  summarise(avg_completion = mean(Cmp_percent_Total)) %>%
  ggplot(aes(x= game_num, y= avg_completion, group=1)) +
  geom_line()


test %>% 
  filter(Season != "2020-2021") %>% 
  group_by(League, game_num) %>% 
  summarise(avg_completion = mean(Cmp_percent_Total)) %>%
  ggplot(aes(x= game_num, y= avg_completion, group=1)) +
  geom_line() +
  facet_wrap(~ League)




test %>% 
  filter(Season != "2020-2021") %>% 
  group_by(game_num) %>% 
  summarise(avg_prg_dist = mean(PrgDist_Total)) %>%
  ggplot(aes(x= game_num, y= avg_prg_dist, group=1)) +
  geom_line()


test %>% 
  filter(Season != "2020-2021") %>% 
  group_by(League, game_num) %>% 
  summarise(avg_prg_dist = mean(PrgDist_Total)) %>%
  ggplot(aes(x= game_num, y= avg_prg_dist, group=1)) +
  geom_line() +
  facet_wrap(~ League)


test %>% 
  filter(Season != "2020-2021") %>% 
  group_by(game_num) %>% 
  summarise(avg_pct_short = mean(Cmp_percent_Short)) %>%
  ggplot(aes(x= game_num, y= avg_pct_short, group=1)) +
  geom_line()


test %>% 
  filter(Season != "2020-2021") %>% 
  group_by(League, game_num) %>% 
  summarise(avg_pct_short = mean(Cmp_percent_Short)) %>%
  ggplot(aes(x= game_num, y= avg_pct_short, group=1)) +
  geom_line() +
  geom_smooth(method = "lm") +
  geom_smooth(colour="red") +
  facet_wrap(~ League)






lm.fit <- test %>%
  lm(Cmp_percent_Total ~ game_num + League, data = .)


lm.fit %>% summary











