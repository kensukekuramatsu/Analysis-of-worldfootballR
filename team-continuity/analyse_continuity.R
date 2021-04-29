library(worldfootballR)
library(tidyverse)


epl_urls <- get_match_urls(country = "ENG", gender = "M", season_end_year = 2021)

epl_lineups_2021 <- get_match_lineups(epl_urls)

saveRDS(epl_lineups_2021, here::here("team-continuity", "epl_lineups.rds"))

epl_lineups_2021 <- readRDS(here::here("team-continuity", "epl_lineups.rds"))

epl_vals <- read.csv(here::here("player_valuations", "epl_2021_valuations.csv"), stringsAsFactors = F)

epl_vals_team <- epl_vals %>% 
  group_by(squad) %>% 
  summarise(total_value = sum(player_market_value_euro, na.rm = T)) %>% 
  ungroup()

epl_vals_team <- epl_vals_team %>% 
  mutate(squad = gsub(" FC", "", squad))



epl_lineups_2021 <- epl_lineups_2021 %>% mutate(matchdate = gsub(".*Match Report – ", "", Matchday))

epl_lineups_2021 <- epl_lineups_2021 %>% 
  separate(col = matchdate, into = c("weekday", "month", "day", "year"), sep = " ") %>% 
  mutate(year = gsub("\\D", "", year),
         day = gsub(",", "", day)) %>% 
  mutate(matchdate = lubridate::ymd(paste(year, str_pad(match(month, month.name), 2, "left", "0"), str_pad(day, 2, "left", "0"), sep = "-")))




team_matchdays <- epl_lineups_2021 %>% 
  arrange(Team, matchdate) %>% 
  distinct(Team, matchdate) %>% 
  group_by(Team) %>% 
  mutate(match_num = row_number()) %>% 
  ungroup() 

team_matchdays <- team_matchdays %>% 
  group_by(Team) %>% 
  mutate(last_match = lag(match_num)) %>% ungroup()

epl_lineups_2021 <- epl_lineups_2021 %>% 
  left_join(team_matchdays, by = c("Team", "matchdate"))


epl_starters <- epl_lineups_2021 %>% 
  filter(Starting == "Pitch")


# Arsenal <- epl_starters %>% filter(Team == "Arsenal")
# 
# for(i in 2:max(Arsenal$match_num)) {
#   first_list <- Arsenal %>% filter(match_num == i) %>% pull(Player_Name)
#   second_list <- Arsenal %>% filter(match_num == i-1) %>% pull(Player_Name)
#   print(length(intersect(first_list, second_list)) / 11)
# }


final_df_starters <- data.frame()

for(each_team in unique(epl_starters$Team)) {
  df <- epl_starters %>% filter(Team == each_team)
  
  out_df <- data.frame()
  for(i in 2:max(df$match_num)) {
    first_list <- df %>% filter(match_num == i) %>% pull(Player_Name)
    second_list <- df %>% filter(match_num == i-1) %>% pull(Player_Name)
    # each_continuity <- length(intersect(first_list, second_list)) / 11
    
    out_df[i-1, "Team"] <- each_team
    out_df[i-1, "match_num"] <- i
    out_df[i-1, "continuity"] <- length(intersect(first_list, second_list)) / 11
    # continuity <- c(continuity, each_continuity)
  }
  final_df_starters <- rbind(final_df_starters, out_df)
}


final_df_starters <- final_df_starters %>% 
  left_join(epl_vals_team, by = c("Team"="squad"))

final_df_starters %>% 
  group_by(Team, total_value) %>% 
  summarise(avg_continuity = mean(continuity, na.rm = T)) %>% 
  arrange(desc(avg_continuity))



final_df_starters %>% 
  group_by(Team, total_value) %>% 
  summarise(avg_continuity = mean(continuity, na.rm = T)) %>% 
  ggplot(aes(x=total_value, y=avg_continuity)) +
  geom_point() +
  geom_smooth()


epl_starters %>% 
  group_by(Team) %>% 
  summarise(MP = max(match_num),
            n_players_starting = n_distinct(Player_Name)) %>% 
  arrange(n_players_starting)


final_df_starters %>% 
  arrange(continuity) %>% head(10)


final_df_starters %>% 
  ggplot(aes(x= match_num, y= continuity, group=Team)) +
  geom_line() + 
  geom_smooth() +
  scale_y_continuous(limits = c(0,1.1)) +
  facet_wrap(~ Team)
