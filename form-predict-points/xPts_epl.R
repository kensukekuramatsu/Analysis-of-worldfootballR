library(worldfootballR)
library(tidyverse)



epl <- get_match_results(country = "ENG", gender = "M", season_end_year = c(2015:2021))



saveRDS(epl, here::here("form-predict-points", "epl_results.rds"))

epl_table <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = c(2015:2021), stat_type = "league_table")
saveRDS(epl_table, here::here("form-predict-points", "epl_table.rds"))


epl <- readRDS(here::here("form-predict-points", "epl_results.rds"))
epl_table <- readRDS(here::here("form-predict-points", "epl_table.rds"))


clean_data <- function(df) {
  
  long_data <- df %>% 
    select(Season_End_Year, Date, Time, Squad=Home, GF=HomeGoals, Opponent=Away, GA=AwayGoals) %>% 
    mutate(home_away="Home") %>% 
    bind_rows(
      df %>% 
        select(Season_End_Year, Date, Time, Squad=Away, GF=AwayGoals, Opponent=Home, GA=HomeGoals) %>% 
        mutate(home_away="Away")
    ) %>% 
    arrange(Season_End_Year, Squad, Date) %>% 
    group_by(Season_End_Year, Squad) %>% mutate(game_num = row_number()) %>% ungroup() %>% 
    mutate(match_points_earned = case_when(
      GF == GA ~ 1,
      GF > GA ~ 3,
      TRUE ~ 0
    )) %>%
    group_by(Season_End_Year, Squad) %>% 
    mutate(cumulated_points = cumsum(match_points_earned)) %>% 
    mutate(points_per_match = cumulated_points / game_num) %>% ungroup()
  
  
  long_data <- long_data %>%
    left_join(epl_table, by = c("Season_End_Year", "Squad")) %>%
    mutate(exp_Pts = points_per_match * MP)
  
  return(long_data)
  
}


epl_reg_season_train <- epl %>%
  filter(Season_End_Year < 2020) %>% 
  clean_data(df=.)


epl_reg_season_train %>% 
  group_by(game_num) %>% 
  summarise(game_cor = cor(exp_Pts, Pts)) %>% 
  ggplot(aes(x=game_num, y=game_cor, group=1)) +
  geom_line()


epl_reg_season_train %>% 
  ggplot(aes(x=exp_Pts, y=Pts)) +
  geom_point() +
  facet_wrap(~ game_num)




calculate_rsquared_each_match <- function(df, match_num) {
  each_lm <- df %>%
    # filter(Season_End_Year < 2021) %>% 
    filter(game_num == match_num) %>% 
    lm(Pts ~ points_per_match, data = .) %>% summary 
  
  adj_r <- each_lm %>% .[["adj.r.squared"]]
  intercept <- each_lm[["coefficients"]][1,1]
  exp_Pts_coef <- each_lm[["coefficients"]][2,1]
  
  each_lm <- data.frame(match_number=match_num, r_squared=adj_r, intercept=intercept, exp_Pts_coef=exp_Pts_coef)
  return(each_lm)
}


model_coefficients <- data.frame()

for(i in c(1:37)) {
  each_df <- calculate_rsquared_each_match(df=epl_reg_season_train, match_num = i)
  model_coefficients <- rbind(model_coefficients, each_df)
}




historic_data <- epl_reg_season_train %>% 
  left_join(model_coefficients, by = c("game_num" = "match_number")) %>% 
  mutate(predicted_points = intercept + (exp_Pts_coef * points_per_match))


epl_reg_season_test <- epl %>%
  filter(Season_End_Year == 2020) %>% 
  clean_data(df=.)


epl_reg_season_test <- epl_reg_season_test %>% 
  left_join(model_coefficients, by = c("game_num" = "match_number")) %>% 
  mutate(predicted_points = intercept + (exp_Pts_coef * points_per_match))


rmse_comp <- data.frame()

for(i in c(1:37)) {
  each_df <- epl_reg_season_test %>% 
    filter(game_num == i)
  
  rmse_comp[i, "game_num"] <- i
  rmse_comp[i, "lm_rmse"] <- round(Metrics::rmse(each_df$Pts, each_df$predicted_points), 2)
  rmse_comp[i, "rate_xPts_rmse"] <- round(Metrics::rmse(each_df$Pts, each_df$exp_Pts), 2)
}




this_season <- epl %>% 
  filter(Season_End_Year == 2021) %>% 
  filter(!is.na(HomeGoals)) %>% 
  clean_data(.)


this_season <- this_season %>% 
  left_join(model_coefficients, by = c("game_num" = "match_number")) %>% 
  mutate(predicted_points = intercept + (exp_Pts_coef * points_per_match))




predicted_table <- epl_table %>% 
  filter(Season_End_Year == 2021) %>% 
  left_join(this_season %>% arrange(Squad, desc(game_num)) %>% distinct(Squad, .keep_all=T) %>% select(Season_End_Year, Squad, predicted_points), by = c("Season_End_Year", "Squad"))



rmse_comp <- data.frame()

for(i in c(1:38)) {
  each_df <- historic_data %>% 
    filter(game_num == i)
  
  rmse_comp[i, "game_num"] <- i
  rmse_comp[i, "lm_rmse"] <- round(Metrics::rmse(each_df$Pts, each_df$predicted_points), 2)
  rmse_comp[i, "rate_xPts_rmse"] <- round(Metrics::rmse(each_df$Pts, each_df$exp_Pts), 2)
}


model_coefficients %>% ggplot(aes(x=match_number, y=r_squared)) + geom_point()

historic_data %>%
  filter(game_num == 25) %>% 
  ggplot(aes(x=predicted_points, y= Pts)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  scale_y_continuous(breaks = seq(0, 100, 25)) +
  # scale_x_continuous(limits = c(0, 105), breaks = seq(0, 105, 15)) +
  # scale_y_continuous(limits = c(0, 105), breaks = seq(0, 105, 15)) +
  ggtitle("PREDICTED POINTS AFTER 19 MATCHES", subtitle = "As calculated by points per match at week 10, multipled by matches for the season") +
  facet_wrap(~ Season_End_Year, nrow=2)







historic_data %>%
  filter(game_num == 19) %>% 
  mutate(pred_error = predicted_points - Pts) %>% 
  arrange(pred_error) %>% 
  select(Season_End_Year, Squad, predicted_points, Pts) %>% head()


historic_data %>%
  filter(game_num == 19) %>% 
  mutate(pred_error = predicted_points - Pts) %>% 
  arrange(desc(pred_error)) %>% 
  select(Season_End_Year, Squad, predicted_points, Pts) %>% head()


historic_data %>%
  filter(Season_End_Year == 2020) %>% 
  ggplot() +
  geom_line(aes(x=game_num, y= Pts, group=1), colour="steelblue", linetype=2) +
  geom_line(aes(x=game_num, y= exp_Pts, group=1)) +
  geom_line(aes(x=game_num, y= predicted_points, group=1), colour="red") +
  ggtitle("EXPECTED POINTS BY PTS RATE 2020 SEASON") +
  facet_wrap(~ Squad)


