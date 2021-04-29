library(tidyverse)
library(worldfootballR)
library(scales)


team_vals <- get_player_market_values(country_name = "England", start_year = c(2016:2020))
saveRDS(team_vals, here::here("epl-value-vs-performance", "epl_team_valuations.rds"))

team_vals_grouped <- team_vals %>%
  mutate(Season_End_Year = season_start_year + 1) %>% 
  select(-season_start_year) %>% 
  group_by(squad, Season_End_Year) %>%
  summarise(total_valuation = sum(player_market_value_euro, na.rm = T),
            avg_player_valuation = mean(player_market_value_euro, na.rm = T)) %>%
  ungroup() %>% 
  filter(Season_End_Year != 2021) %>% 
  mutate(squad = gsub("AFC Bournemouth", "Bournemouth", squad),
         squad = gsub("Sunderland AFC", "Sunderland", squad),
         squad = gsub(" FC", "", squad),
         squad = gsub("Brighton & Hove Albion", "Brighton", squad),
         squad = gsub("Huddersfield Town", "Huddersfield", squad),
         squad = gsub("Manchester United", "Manchester Utd", squad),
         squad = gsub("Newcastle United", "Newcastle Utd", squad),
         squad = gsub("Sheffield United", "Sheffield Utd", squad),
         squad = gsub("Tottenham Hotspur", "Tottenham", squad),
         squad = gsub("West Bromwich Albion", "West Brom", squad),
         squad = gsub("West Ham United", "West Ham", squad),
         squad = gsub("Wolverhampton Wanderers", "Wolves", squad))




league_table <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = c(2017:2020), tier = "1st", stat_type = "league_table")
saveRDS(team_vals, here::here("epl-value-vs-performance", "league_table.rds"))

joined_df <- league_table %>%
  left_join(team_vals_grouped, by = c("Squad" = "squad", "Season_End_Year"))



joined_df %>% 
  lm(Pts ~ avg_player_valuation, data = .) %>% 
  summary()



joined_df %>%
  mutate(avg_player_valuation_M = avg_player_valuation / 1000000) %>% 
  mutate(Team_Season = paste0(Squad, " - ", Season_End_Year)) %>% 
  ggplot(aes(x= Pts, y= avg_player_valuation_M)) +
  geom_smooth(method = "lm", se = F, colour = "steelblue", size=1, linetype=2) +
  geom_point() +
  scale_y_continuous(labels = dollar_format(prefix = "€", suffix = "M"), name = "Average player valuation") +
  labs(x= "End of season points")

joined_df %>%
  mutate(avg_player_valuation_M = avg_player_valuation / 1000000) %>% 
  ggplot(aes(x= Pts, y= avg_player_valuation)) +
  geom_smooth(method = "lm", se = F, colour = "steelblue", size=1, linetype=2) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = Squad)) +
  scale_y_continuous(labels = dollar_format(prefix = "€", suffix = "M"), name = "Average player valuation") +
  facet_wrap(~ Season_End_Year)






team_xfers <- data.frame()

for(i in c(2016:2020)) {
  df <- tm_team_transfer_balances(country_name = "England", start_year = i)
  team_xfers <- rbind(team_xfers, df)
}

saveRDS(team_xfers, here::here("epl-value-vs-performance", "epl_team_transfer_balances.rds"))


team_xfers_cleaned <- team_xfers %>% 
  mutate(squad = gsub("AFC Bournemouth", "Bournemouth", squad),
         squad = gsub("Sunderland AFC", "Sunderland", squad),
         squad = gsub(" FC", "", squad),
         squad = gsub("Brighton & Hove Albion", "Brighton", squad),
         squad = gsub("Huddersfield Town", "Huddersfield", squad),
         squad = gsub("Manchester United", "Manchester Utd", squad),
         squad = gsub("Newcastle United", "Newcastle Utd", squad),
         squad = gsub("Sheffield United", "Sheffield Utd", squad),
         squad = gsub("Tottenham Hotspur", "Tottenham", squad),
         squad = gsub("West Bromwich Albion", "West Brom", squad),
         squad = gsub("West Ham United", "West Ham", squad),
         squad = gsub("Wolverhampton Wanderers", "Wolves", squad)) %>% 
  mutate(net_income = income_euros - expenditure_euros) %>%
  mutate(season = gsub(".*/", "", season) %>% paste0(20, .) %>% as.numeric()) %>% 
  filter(season != 2021)


test <- league_table %>%
  left_join(team_xfers_cleaned, by = c("Squad" = "squad", "Season_End_Year" = "season"))


test %>% 
  lm(Pts ~ net_income, data = .) %>% 
  summary()


test %>%
  ggplot(aes(x= Pts, y= net_income)) +
  geom_smooth(method = "lm", se = F, colour = "steelblue", size=1, linetype=2) +
  geom_point() +
  scale_y_continuous(labels = dollar_format(prefix = "€", suffix = "M"), name = "Average player valuation") +
  labs(x= "End of season points")





