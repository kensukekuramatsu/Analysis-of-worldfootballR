library(worldfootballR)
library(tidyverse)

# Get Data ----------------------------------------------------------------
countries <- c("ENG", "ITA", "FRA", "GER", "ESP")
league_table <- get_season_team_stats(country = countries, gender = "M", season_end_year = c(2018:2021), stat_type = "league_table")
saveRDS(league_table, here::here("Euro-league-passing-differences", "league_table.rds"))
passing <- get_season_team_stats(country = countries, gender = "M", season_end_year = c(2018:2021), stat_type = "passing")
saveRDS(passing, here::here("Euro-league-passing-differences", "passing.rds"))
shooting <- get_season_team_stats(country = countries, gender = "M", season_end_year = c(2018:2021), stat_type = "shooting")
saveRDS(shooting, here::here("Euro-league-passing-differences", "shooting.rds"))


# Read in Data ------------------------------------------------------------
league_table <- readRDS(here::here("Euro-league-passing-differences", "league_table.rds"))
passing <- readRDS(here::here("Euro-league-passing-differences", "passing.rds"))
shooting <- readRDS(here::here("Euro-league-passing-differences", "shooting.rds"))


relevant_leage_data <- league_table %>% 
  select(Competition_Name, Country, Season_End_Year, Squad, MP, W, D, L, Pts)

passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>% 
  ggplot(aes(x= Att_Total, y= Pts)) +
  geom_point() +
  facet_wrap(~ Competition_Name)

passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>% 
  group_by(Competition_Name) %>% 
  summarise(cors = cor(Att_Total, Pts))


passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  mutate(att_per_90 = Att_Total / Mins_Per_90) %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>% 
  ggplot(aes(x= att_per_90, y= Pts)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Competition_Name)

passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  mutate(att_per_90 = Att_Total / Mins_Per_90) %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>% 
  group_by(Competition_Name) %>% 
  summarise(cors = cor(att_per_90, Pts))


passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>% 
  ggplot(aes(x= Cmp_Total, y= Pts)) +
  geom_point() +
  facet_wrap(~ Competition_Name)

passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>% 
  group_by(Competition_Name) %>% 
  summarise(cors = cor(Cmp_Total, Pts))

passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>% 
  ggplot(aes(x= Cmp_percent_Total, y= Pts)) +
  geom_point() +
  facet_wrap(~ Competition_Name)

passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>% 
  group_by(Competition_Name) %>% 
  summarise(cors = cor(Cmp_percent_Total, Pts)) 


passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>%
  split(.$Competition_Name) %>%
  map(~ lm(Pts ~ Att_Total, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared") %>% data.frame()

passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  left_join(relevant_leage_data) %>% 
  left_join(shooting) %>% 
  filter(Season_End_Year != 2021) %>%
  split(.$Competition_Name) %>%
  map(~ lm(Pts ~ Cmp_Total, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared") %>% data.frame()

###########################################################################
# EDA ---------------------------------------------------------------------
###########################################################################

passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  mutate(passess_attempted_90 = Att_Total / Mins_Per_90) %>% 
  group_by(Competition_Name, Country, Season_End_Year) %>% 
  summarise(avg_passess_attempted_90 = mean(passess_attempted_90)) %>% 
  ggplot(aes(x= Season_End_Year, y= avg_passess_attempted_90, group = Competition_Name, colour = Competition_Name)) +
  geom_line() +
  labs(x= "Season End", y= "Avg Passes per 90 minutes") +
  ggtitle("NOT AS MUCH PASSING IN LA LIGA GAMES",
          subtitle = "La Liga teams on average attempt almost 40 passes less\nper 90 minutes than the other top 5 European Leagues")

passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  mutate(passess_completed_90 = Cmp_Total / Mins_Per_90) %>% 
  group_by(Competition_Name, Country, Season_End_Year) %>% 
  summarise(avg_passess_completed_90 = mean(passess_completed_90)) %>% 
  ggplot(aes(x= Season_End_Year, y= avg_passess_completed_90, group = Competition_Name, colour = Competition_Name)) +
  geom_line() 



# Passing completion % ----------------------------------------------------

# option 1:
passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  group_by(Competition_Name, Country, Season_End_Year) %>% 
  summarise(avg_completion = mean(Cmp_percent_Total)) %>% 
  ggplot(aes(x= Season_End_Year, y= avg_completion, group = Competition_Name, colour = Competition_Name)) +
  geom_line(size=1) +
  geom_point(size=2) +
  labs(x= "Season End", y= "Completion %") +
  ggtitle("ALL LEAGUES COMPLETING MORE PASSES",
          subtitle = "Passing completions rates continue to rise for the top 5 European Leagues.\nInterestingly, there is almost a 4% difference in completion rates between\nLa Liga and Serie A") +
  scale_colour_manual(values = league_cols) +
  theme(legend.position = "top", legend.title = element_blank())


# boxplot:
passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  # group_by(Competition_Name, Country, Season_End_Year) %>% 
  # summarise(avg_completion = mean(Cmp_percent_Total)) %>% 
  ggplot(aes(x= factor(Season_End_Year), y= Cmp_percent_Total, colour = Competition_Name, fill = Competition_Name)) +
  geom_boxplot(alpha=0.5) +
  labs(x= "Season End", y= "Completion %") +
  ggtitle("ALL LEAGUES COMPLETING MORE PASSES",
          subtitle = "Passing completions rates continue to rise for the top 5 European Leagues.\nInterestingly, there is almost a 4% difference in completion rates between\nLa Liga and Serie A") +
  scale_colour_manual(values = league_cols) +
  scale_fill_manual(values = league_cols) +
  theme(legend.position = "top", legend.title = element_blank())



passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  mutate(percent_attempted_long = Att_Long / Att_Total) %>% 
  group_by(Competition_Name, Country, Season_End_Year) %>% 
  summarise(avg_att_long_percent = mean(percent_attempted_long)) %>% 
  ggplot(aes(x= Season_End_Year, y= avg_att_long_percent, group = Competition_Name, colour = Competition_Name)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent, name = "Average % Long Passes Attempted") +
  labs(x= "Season End") +
  ggtitle("PASSING LONG ISN'T UNIFORM ACROSS THE BIG 5 EURO LEAGUES",
          subtitle = "La Liga has seen an increase in the % of all passes attempted being long\nballs, while the EPL, Serie A and Ligue 1 all have seen decreases over\nthe last four years")




# not much in progressive passes between the leagues
passing %>%
  filter(Team_or_Opponent == "team") %>%
  mutate(progressive_passes_per_game = Prog / Mins_Per_90) %>%
  group_by(Competition_Name, Country, Season_End_Year) %>%
  summarise(avg_progressive_passes_per_game = mean(progressive_passes_per_game)) %>%
  ggplot(aes(x= Season_End_Year, y= avg_progressive_passes_per_game, group = Competition_Name, colour = Competition_Name)) +
  geom_line() 
  # scale_y_continuous(labels = scales::percent, name = "Average % Long Passes Attempted") +
  # labs(x= "Season End") +
  # ggtitle("PASSING LONG ISN'T UNIFORM ACROSS THE BIG 5 EURO LEAGUES",
  #         subtitle = "La Liga has seen an increase in the % of all passes attempted being long\nballs, while the EPL, Serie A and Ligue 1 all have seen decreases over\nthe last four years")



# interesting?
passing %>%
  filter(Team_or_Opponent == "team") %>%
  mutate(progressive_passes_per_pass = Att_Total / Prog) %>%
  group_by(Competition_Name, Country, Season_End_Year) %>%
  summarise(avg_progressive_passes_per_pass = mean(progressive_passes_per_pass)) %>%
  ggplot(aes(x= Season_End_Year, y= avg_progressive_passes_per_pass, group = Competition_Name, colour = Competition_Name)) +
  geom_line() +
  # scale_y_continuous(limits = c(0,13)) +
  labs(x= "Season End", y= "Passes") +
  ggtitle("MORE COMPLETED PASSES BEFORE A PROGRESSIVE PASS",
          subtitle = "Bundesliga teams on average make less completed passes before making a\nprogressive passes by almost a full pass on the other leagues")
  


passing %>%
  filter(Team_or_Opponent == "team") %>%
  mutate(progressive_passes_per_pass = Att_Total / Prog) %>%
  # group_by(Competition_Name, Country, Season_End_Year) %>%
  # summarise(avg_progressive_passes_per_pass = mean(progressive_passes_per_pass)) %>%
  ggplot(aes(x= factor(Season_End_Year), y= progressive_passes_per_pass, colour = Competition_Name, fill = Competition_Name)) +
  geom_boxplot(alpha = 0.5) 
  scale_y_continuous(limits = c(0,13)) +
  labs(x= "Season End", y= "Passes") +
  ggtitle("MORE COMPLETED PASSES BEFORE A PROGRESSIVE PASS",
          subtitle = "Bundesliga teams on average make less completed passes before making a\nprogressive passes by almost a full pass on the other leagues")




passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  mutate(PPA_per_game = PPA / Mins_Per_90) %>% 
  group_by(Competition_Name, Country, Season_End_Year) %>% 
  summarise(avg_PPA_per_game = mean(PPA_per_game)) %>% 
  ggplot(aes(x= Season_End_Year, y= avg_PPA_per_game, group = Competition_Name, colour = Competition_Name)) +
  geom_line() 
  # scale_y_continuous(labels = scales::percent, name = "Average % Long Passes Attempted") +
  # labs(x= "Season End") +
  # ggtitle("PASSING LONG ISN'T UNIFORM ACROSS THE BIG 5 EURO LEAGUES",
  #         subtitle = "La Liga has seen an increase in the % of all passes attempted being long\nballs, while the EPL, Serie A and Ligue 1 all have seen decreases over\nthe last four years")



passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  filter(Season_End_Year < 2021) %>% 
  ggplot(aes(x= Prog, y= PPA)) +
  geom_point() +
  facet_wrap(~ Competition_Name)


# plotting the relationship between progressive passes and passes into the pen area:
passing %>% 
  # filter(Country %in% c("GER", "FRA")) %>% 
  filter(Team_or_Opponent == "team") %>% 
  filter(Season_End_Year < 2021) %>% 
  ggplot(aes(x= Prog, y= PPA)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = paste0(Squad, "-", Season_End_Year)), colour = "grey60") +
  labs(x= "Progressive Passes", y= "Passes into Pen Area") +
  facet_wrap(~ Competition_Name) +
  theme_minimal()


passing %>% 
  filter(Team_or_Opponent == "team") %>% 
  filter(Season_End_Year < 2021) %>% 
  group_by(Competition_Name) %>% 
  summarise(corrr = cor(Prog, PPA))


shooting %>% 
  group_by(Competition_Name, Country, Season_End_Year) %>% 
  summarise(avg_shot_distance = mean(Dist_Standard)) %>% 
  ggplot(aes(x= Season_End_Year, y= avg_shot_distance, group = Competition_Name, colour = Competition_Name)) +
  geom_line() 
  # scale_y_continuous(labels = scales::percent, name = "Average % Long Passes Attempted") +
  # labs(x= "Season End") +
  # ggtitle("PASSING LONG ISN'T UNIFORM ACROSS THE BIG 5 EURO LEAGUES",
  #         subtitle = "La Liga has seen an increase in the % of all passes attempted being long\nballs, while the EPL, Serie A and Ligue 1 all have seen decreases over\nthe last four years")




shooting %>% 
  filter(Team_or_Opponent == "team") %>% 
  mutate(goals_std_game = Gls_Standard / Mins_Per_90) %>% 
  group_by(Competition_Name, Country, Season_End_Year) %>% 
  summarise(avg_goals_std_game = mean(goals_std_game)) %>% 
  ggplot(aes(x= Season_End_Year, y= avg_goals_std_game, group = Competition_Name, colour = Competition_Name)) +
  geom_line() 
  # scale_y_continuous(labels = scales::percent, name = "Average % Long Passes Attempted") +
  # labs(x= "Season End") +
  # ggtitle("PASSING LONG ISN'T UNIFORM ACROSS THE BIG 5 EURO LEAGUES",
  #         subtitle = "La Liga has seen an increase in the % of all passes attempted being long\nballs, while the EPL, Serie A and Ligue 1 all have seen decreases over\nthe last four years")



shooting %>% 
  filter(Team_or_Opponent == "team") %>% 
  mutate(goals_std_game = Gls_Standard / Mins_Per_90) %>% 
  # group_by(Competition_Name, Country, Season_End_Year) %>% 
  # summarise(avg_goals_std_game = mean(goals_std_game)) %>% 
  ggplot(aes(x= goals_std_game, fill = Competition_Name)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Season_End_Year)



shooting %>% 
  filter(Team_or_Opponent == "team") %>% 
  ggplot(aes(x= npxG_Expected, y= Gls_Standard)) +
  geom_point() +
  facet_wrap(~ Competition_Name)


shooting %>% 
  filter(Team_or_Opponent == "team") %>% 
  ggplot(aes(x= SoT_Standard, y= npxG_Expected)) +
  geom_point() +
  facet_wrap(~ Competition_Name)


shooting %>% 
  filter(Team_or_Opponent == "team") %>% 
  ggplot(aes(x= Sh_Standard, y= npxG_Expected)) +
  geom_point() +
  facet_wrap(~ Competition_Name)




shooting %>% 
  filter(Team_or_Opponent == "team") %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>% 
  ggplot(aes(x= Gls_Standard, y= W)) +
  geom_point(position = "jitter") +
  # ggrepel::geom_text_repel(aes(label = paste0(Squad, "-", Season_End_Year)), colour = "grey60") +
  facet_wrap(~ Competition_Name)



shooting %>% 
  filter(Team_or_Opponent == "team") %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>% 
  ggplot(aes(x= Gls_Standard, y= Pts)) +
  geom_point() +
  facet_wrap(~ Competition_Name)


shooting %>% 
  filter(Team_or_Opponent == "team") %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>% 
  lm(Pts ~ Gls_Standard + Competition_Name, data = .) %>% summary


league_table %>% 
  filter(Season_End_Year != 2021) %>% 
  lm(Pts ~ GF + GA + Competition_Name, data = .) %>% summary




shooting %>% 
  filter(Team_or_Opponent == "team") %>% 
  left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>% 
  ggplot(aes(x= Pts, y= Competition_Name)) +
  geom_boxplot()


# shooting %>% 
#   filter(Team_or_Opponent == "team") %>% 
#   left_join(relevant_leage_data) %>% 
#   filter(Season_End_Year != 2021) %>%
#   group_by(Competition_Name) %>% 
#   summarise(num_games = max(Mins_Per_90))






rsquared <- league_table %>% 
  filter(Team_or_Opponent == "team") %>% 
  # left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>%
  split(.$Competition_Name) %>%
  map(~ lm(Pts ~ GF + GA, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared") %>% data.frame()

colnames(rsquared) <- "adj_r"


cf <- league_table %>% 
  filter(Team_or_Opponent == "team") %>% 
  # left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>%
  split(.$Competition_Name) %>%
  map(~ lm(Pts ~ GF + GA, data = .x)) %>%
  map_dfr(~ as.data.frame(t(as.matrix(coef(.)))))


bind_cols(rsquared, cf)


league_table %>% 
  filter(Team_or_Opponent == "team") %>% 
  # left_join(relevant_leage_data) %>% 
  filter(Season_End_Year != 2021) %>%
  split(.$Competition_Name) %>%
  map(~ lm(Pts ~ GF + GA, data = .x)) %>%
  map("fitted.values") 


