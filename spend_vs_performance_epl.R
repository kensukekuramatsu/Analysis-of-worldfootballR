library(tidyverse)
library(worldfootballR)


options(scipen = 999)

team_vals <- get_player_market_values(country_name = "England", start_year = 2020)


team_vals_grouped <- team_vals %>%
  group_by(squad) %>%
  summarise(total_valuation = sum(player_market_value_euro, na.rm = T),
            avg_player_valuation = mean(player_market_value_euro, na.rm = T)) %>%
  ungroup()


league_table <- tm_matchday_table(country_name = "England", start_year = 2020, matchday = 32)



team_vals_grouped <- team_vals_grouped %>%
  mutate(squad = gsub(" FC", "", squad),
         squad = gsub(" & Hove Albion", "", squad),
         squad = gsub("United", "Utd", squad),
         squad = gsub("Leeds Utd", "Leeds", squad),
         squad = gsub(" Hotspur", "", squad),
         squad = gsub("wich Albion", "", squad),
         squad = gsub("West Ham Utd", "West Ham", squad),
         squad = gsub("Wolverhampton Wanderers", "Wolves", squad),
         squad = gsub("Manchester City", "Man City", squad),
         squad = gsub("Manchester Utd", "Man Utd", squad),
         squad = gsub("Leicester City", "Leicester", squad),
         squad = gsub("Tottenham", "Spurs", squad),
         squad = gsub("Newcastle Utd", "Newcastle", squad),
         squad = gsub("Sheffield Utd", "Sheff Utd", squad))



joined_df <- league_table %>%
  left_join(team_vals_grouped %>% mutate(squad = gsub(" FC", "", squad)), by = c("Team" = "squad"))


joined_df %>% 
  lm(Pts ~ avg_player_valuation, data = .) %>% 
  summary()



joined_df %>%
  ggplot(aes(x= Pts, y= avg_player_valuation)) +
  geom_smooth(method = "lm", se = F, colour = "steelblue", size=1, linetype=2) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = Team))



joined_df %>%
  mutate(pts_per_M_euro = Pts / (avg_player_valuation / 1000000)) %>%
  ggplot(aes(x= pts_per_M_euro, y= reorder(Team, pts_per_M_euro))) +
  geom_col()




team_xfers <- tm_team_transfer_balances(country_name = "England", start_year = 2020)


team_xfers_named <- team_xfers %>%
  mutate(net_income = income_euros - expenditure_euros) %>%
  mutate(team = gsub(" FC", "", team),
         team = gsub(" & Hove Albion", "", team),
         team = gsub("United", "Utd", team),
         team = gsub("Leeds Utd", "Leeds", team),
         team = gsub(" Hotspur", "", team),
         team = gsub("wich Albion", "", team),
         team = gsub("West Ham Utd", "West Ham", team),
         team = gsub("Wolverhampton Wanderers", "Wolves", team),
         team = gsub("Manchester City", "Man City", team),
         team = gsub("Manchester Utd", "Man Utd", team),
         team = gsub("Leicester City", "Leicester", team),
         team = gsub("Tottenham", "Spurs", team),
         team = gsub("Newcastle Utd", "Newcastle", team),
         team = gsub("Sheffield Utd", "Sheff Utd", team))


test <- league_table %>%
  left_join(team_xfers_named, by = c("Team" = "team"))




test %>%
  mutate(pts_per_M_euro_spent = Pts / (abs(net_income) / 1000000)) %>%
  ggplot(aes(x= pts_per_M_euro_spent, y= reorder(Team, pts_per_M_euro_spent))) +
  geom_col()

test %>%
  ggplot(aes(x= abs(net_income), y= Pts)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = Team))



