library(worldfootballR)
library(tidyverse)


xg_data <- get_match_results(country = "ENG", gender = "M", season_end_year = 2021)
saveRDS(xg_data, here::here("published", "top-6-xG", "match_results_prem_2021-01-21.rds"))

# create a long df of all teams home and away
a <- xg_data %>% 
  select(Season_End_Year, Date, Team=Home, Goals=HomeGoals, xG=Home_xG) %>% 
  mutate(HomeAway = "Home",
         goals_above_expected = Goals - xG) %>% 
  bind_rows( xg_data %>% 
               select(Season_End_Year, Date, Team=Away, Goals=AwayGoals, xG=Away_xG) %>%
               mutate(HomeAway = "Away",
                      goals_above_expected = Goals - xG)) %>%
  arrange(Season_End_Year, Team, Date) %>% 
  group_by(Season_End_Year, Team) %>% mutate(game_num = row_number()) %>% 
  filter(!is.na(Goals)) %>% 
  group_by(Season_End_Year, Team, game_num) %>% 
  summarise(Goals = sum(Goals, na.rm = T),
            xG = sum(xG, na.rm = T)) %>% ungroup() %>% 
  mutate(goal_above_expected = Goals - xG) %>% 
  group_by(Season_End_Year, Team) %>% 
  mutate(cum_total_goals = cumsum(Goals),
         cum_total_xg = cumsum(xG)) %>% 
  select(Season_End_Year, Team, game_num, Goals=cum_total_goals, xG=cum_total_xg) %>% 
  pivot_longer(cols = c(Goals:xG), names_to = "GoalType", values_to = "Goals")

# vector of the top 6 teams current to the 21st Jan, 2021
top_6 <- c("Everton", "Leicester City", "Liverpool", "Manchester City", "Manchester Utd", "Tottenham")

# create a labels DF
lab_df <- a %>% 
  filter(Team %in% c("Everton", "Tottenham")) %>% 
  arrange(Team, desc(game_num)) %>% distinct(Season_End_Year, Team, GoalType, .keep_all = T)

# load fonts
sysfonts::font_add_google(name = "Chivo", family = "chivo")
showtext::showtext_auto()

# set theme
theme_set(theme_minimal() +
            theme(plot.title = element_text(colour = "black", size = 22, family = "chivo"), 
                  plot.subtitle = element_text(colour = "grey20", size = 16, family = "chivo"),
                  axis.text.x = element_text(colour = "grey20", size = 12, family = "chivo"),
                  axis.text.y = element_text(colour = "grey20", size = 12, family = "chivo"),
                  axis.title.x = element_text(colour = "grey20", size = 12, family = "chivo"),
                  axis.title.y = element_text(colour = "grey20", size = 12, family = "chivo"),
                  plot.caption = element_text(colour = "grey20", size = 10, family = "chivo"),
                  strip.text = element_text(colour = "grey30", size = 12, face = "bold", family = "chivo")))


# Final Visualisation -----------------------------------------------------
a %>%
  filter(Team %in% top_6) %>% 
  ggplot(aes(x= game_num, y= Goals, group = GoalType, colour=GoalType)) +
  geom_line(size=1) +
  geom_text(data = lab_df, aes(x=game_num, y= Goals, label = GoalType, colour = GoalType), vjust=0,, hjust=1, family = "chivo", size=5) +
  scale_colour_manual(values = c("black", "orange")) +
  ggtitle("TOP END OF THE TABLE EXCEDDING EXPECTED GOALS?",
          subtitle = "The top two teams from last season (Liverpool & Man City) are scoring as expected, while\nthe other teams in the top 6 are exceeding their expected goals tally for the season so far") +
  labs(x= "Match Number", y= "Cumulative Goals",
       caption = paste0("\n*Data current to ", lubridate::today(), "\nSource: worldfootballR  ||  Table: @jase_ziv")) +
  facet_wrap(~ Team) +
  theme(legend.position = "none")


ggsave(here::here("published", "top-6-xG", "top_6_xg_performance.png"), width = 12, height = 8, units = "in")
