library(elo)
library(worldfootballR)
library(tidyverse)



# code to get data using worldfootballR
league <- get_match_results(country = "AUS", gender = "M", season_end_year = 2021)

league <- league %>% 
  filter(Round == "Regular Season" | is.na(Round)) %>% 
  filter(!is.na(HomeGoals)) %>% 
  arrange(Date)


calc_goal_index <- function(home_goals, away_goals) {
  if(abs(home_goals - away_goals) <= 1) {
    G <- 1
  } else if (abs(home_goals - away_goals) == 2) {
    G <- 1.5
  } else {
    G <- (11 + abs(home_goals - away_goals))/8
  }
}

league <- league %>% 
  mutate(goal_index = mapply(calc_goal_index, HomeGoals, AwayGoals))

# # adjust for home team and include margin adjustment
# elo_since_2014 <- elo.run(score(HomeGoals, AwayGoals) ~ adjust(Home, 40) + Away +
#                    k(20*log(abs(HomeGoals - AwayGoals) + 1)), data = aleague) %>% data.frame()
# 
# elo_since_2014 <- bind_cols(
#   aleague %>% select(Season_End_Year, Date, HomeGoals, AwayGoals),
#   elo_since_2014
# )


# adjust for home team and include margin adjustment, but this margin has been calculated using the G index
elo_model <- elo.run(score(HomeGoals, AwayGoals) ~ adjust(Home, 40) + Away +
                 k(20*goal_index), data = league) 

elo <- elo_model %>% data.frame()

elo_joined <- bind_cols(
  league %>% select(Season_End_Year, Date, HomeGoals, AwayGoals),
  elo
)



newdat <- data.frame(
  Home = "Melb City",
  Away = "Central Coast"
)
predict(elo_model, newdata = newdat)


