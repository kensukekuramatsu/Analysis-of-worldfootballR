library(worldfootballR)
library(tidyverse)
library(elo)

# load data
aleague <- readRDS(here::here("elo", "aleague_results.rds"))

# Incremental data collection: ------------------------------------------------
aleague_extra <- get_match_results(country = "AUS", gender = "M", 
                                   season_end_year = max(aleague$Season_End_Year, na.rm = T))


aleague <- rbind(aleague, aleague_extra)

aleague <- aleague %>% 
  filter(!is.na(HomeGoals)) %>% 
  arrange(Season_End_Year, Date) %>% 
  distinct(Date, Home, Away, .keep_all = T)

aleague <- aleague %>% 
  filter(Round == "Regular Season" | is.na(Round)) %>% 
  filter(!is.na(HomeGoals)) %>% 
  arrange(Date)

# * where: If the game is a draw or is won by one goal
#     * G = 1
#   * If the game is won by two goals
#     * G = 1,5
#   * If the game is won by three or more goals
#     * G = (11+N)/8
#     * Where N is the goal difference

# calculate goal margin index:

calc_goal_index <- function(home_goals, away_goals) {
  if(abs(home_goals - away_goals) <= 1) {
    G <- 1
  } else if (abs(home_goals - away_goals) == 2) {
    G <- 1.5
  } else {
    G <- (11 + abs(home_goals - away_goals))/8
  }
}


aleague <- aleague %>% 
  mutate(goal_index = mapply(calc_goal_index, HomeGoals, AwayGoals))



all_elo <- data.frame()
for(each_season in unique(aleague$Season_End_Year)) {
  
  df <- aleague %>% filter(Season_End_Year == each_season) %>% 
    elo.run(score(HomeGoals, AwayGoals) ~ adjust(Home, 40) + Away +
              k(20*goal_index), data = .) %>% data.frame()
  
  df <- bind_cols(
    aleague %>% filter(Season_End_Year == each_season) %>% select(Season_End_Year, Date, HomeGoals, AwayGoals),
    df
  )
  
  all_elo <- rbind(all_elo, df)
}


all_elo_cleaned <- all_elo %>% 
  select(Season_End_Year, Date, Team=team.A, GF=HomeGoals, Opponent=team.B, GA=AwayGoals, pWin=p.A, Elo=elo.A, Opp_Elo=elo.B, TeamUpdate=update.A) %>% mutate(home_away="Home") %>% 
  bind_rows(
    all_elo %>% 
      mutate(home_away="Away", pWin = (1-p.A)) %>% 
      select(Season_End_Year, Date, Team=team.B, GF=AwayGoals, Opponent=team.A, GA=HomeGoals, pWin, Elo=elo.B, Opp_Elo=elo.A, home_away, , TeamUpdate=update.B)
  )


saveRDS(all_elo, here::here("elo", "each_season_elo.rds"))

all_elo_cleaned %>% 
  ggplot(aes(x=Date, y= Elo, group = Season_End_Year, colour = factor(Season_End_Year))) +
  geom_line() +
  facet_wrap(~ Team) +
  theme(legend.position = "top")


test_df <- all_elo_cleaned %>% 
  arrange(Team, Season_End_Year, Date) %>% 
  group_by(Team, Season_End_Year) %>% 
  mutate(match_num = row_number()) %>% ungroup()


test_df %>% 
  filter(match_num == 16) %>% 
  arrange(Elo) %>% 
  select(Team, Season_End_Year, Elo) %>% 
  head()



num_matches <- test_df %>% group_by(Team, Season_End_Year) %>% summarise(n_matches = max(match_num)) %>% ungroup()


worst_seasons <- num_matches %>% 
  left_join(test_df, by = c("Team", "Season_End_Year", "n_matches" = "match_num")) %>% 
  arrange(Elo) %>% head(10)



worst_aleague_seasons <- worst_seasons %>%
  select(Team, Season_End_Year) %>% 
  left_join(test_df, by = c("Team", "Season_End_Year")) %>% 
  mutate(team_season = paste0(Team, "-", Season_End_Year)) 

# worst_aleague_seasons <- test_df %>% 
#   mutate(keep_filter = case_when(
#     Team == "Central Coast" & Season_End_Year == 2019 ~ "Keep",
#     Team == "Newcastle" & Season_End_Year == 2015 ~ "Keep",
#     Team == "Melb Victory" & Season_End_Year == 2021 ~ "Keep",
#     TRUE ~ "No"
#   )) %>% 
#   filter(keep_filter == "Keep") %>% 
#   select(-keep_filter)


worst_aleague_seasons %>% 
  mutate(Melb_Vict = str_detect(Team, "Melb Victory")) %>% 
  ggplot(aes(x= match_num, y= Elo, group = team_season, colour = Melb_Vict)) +
  geom_line() +
  scale_colour_manual(values = c("grey", "midnightblue"))


# load fonts
sysfonts::font_add_google(name = "Chivo", family = "chivo")
showtext::showtext_auto()

# set theme
theme_set(theme_minimal() +
            theme(plot.title = element_text(colour = "midnightblue", size = 26, family = "chivo", face = "bold"), 
                  plot.subtitle = element_text(colour = "grey20", size = 19, family = "chivo", face = "italic"),
                  axis.text.x = element_text(colour = "grey20", size = 14, family = "chivo"),
                  axis.text.y = element_text(colour = "grey20", size = 14, family = "chivo"),
                  axis.title.x = element_text(colour = "grey20", size = 16, family = "chivo"),
                  axis.title.y = element_text(colour = "grey20", size = 16, family = "chivo"),
                  plot.caption = element_text(colour = "grey20", size = 12, family = "chivo"),
                  strip.text = element_text(colour = "grey30", size = 12, face = "bold", family = "chivo")))


team_labels <- worst_aleague_seasons %>% 
  filter((Team == "Melb Victory" & Season_End_Year == 2021) | Team == "Central Coast" & Season_End_Year == 2016) %>% 
  arrange(Team, desc(match_num)) %>% 
  distinct(Team, .keep_all = T) %>% 
  mutate(shortened_season = ifelse(Team == "Melb Victory", "2020/21", "2015/16"))


worst_aleague_seasons %>% 
  mutate(Melb_Vict = str_detect(Team, "Melb Victory")) %>% 
  ggplot() +
  geom_line(aes(x= match_num, y= Elo, group = team_season), colour = "lightgrey", size=1, alpha = 0.3) +
  geom_line(data = worst_aleague_seasons %>% filter(Team == "Central Coast", Season_End_Year == 2016), aes(x=match_num, y= Elo), colour = "#f6c644", size=1.5) +
  geom_line(data = worst_aleague_seasons %>% filter(Team == "Melb Victory"), aes(x=match_num, y= Elo), colour = "midnightblue", size=1.5) +
  geom_hline(yintercept = 1500, linetype=2, colour="grey", size=1) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0,32), name = "Matches played") +
  geom_label(x=26, y=1500, label = "Average", colour = "grey", label.size = NA, size=6, fontface=2) +
  geom_text(data = team_labels, aes(x=match_num, y= Elo, label = paste0(Team, "\n", shortened_season), colour = Team), vjust=1, nudge_x = 1.9, size=5, fontface=2) +
  scale_colour_manual(values = c("#f6c644", "midnightblue")) +
  scale_y_continuous(limits = c(1300, 1550), name = "ELO Rating") +
  ggtitle("MELBOURNE VICTORY HISTORICALLY BAD THIS SEASON",
          subtitle = "Using data since the 2013/14 A-League season and calculating in-season Elo ratings, the\nVictory currently have a lower Elo rating after 16 games played than the 2015/16 Mariners,\nwho finished the A-Leage season with the worst Elo.") +
  labs(caption = paste0("\n*Data current to ", lubridate::today(), "\nSource: fbref.com through worldfootballR  ||  Table: @jase_ziv")) +
  theme(legend.position = "none")
