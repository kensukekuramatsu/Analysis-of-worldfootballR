library(worldfootballR)
library(tidyverse)



# results <- get_match_results(country = c("ENG", "GER", "ITA", "ESP", "FRA"), gender = "M", season_end_year = c(2015:2021))
# 
# results <- results %>% filter(!is.na(Wk))
# 
# saveRDS(results, here::here("form-predict-points", "top5_league_results.rds"))
# 
# table <- get_season_team_stats(country = c("ENG", "GER", "ITA", "ESP", "FRA"), gender = "M", season_end_year = c(2015:2021), stat_type = "league_table")
# saveRDS(table, here::here("form-predict-points", "top5_league_table.rds"))



results <- readRDS(here::here("form-predict-points", "top5_league_results.rds"))
results <- results %>% filter(!is.na(Wk),
                              !is.na(HomeGoals))


table <- readRDS(here::here("form-predict-points", "top5_league_table.rds"))
table <- table %>% select(-Pts.G)


# Incremental data collection: ------------------------------------------------

results_extra <- get_match_results(country = c("ENG", "GER", "ITA", "ESP", "FRA"), gender = "M", 
                                   season_end_year = max(results$Season_End_Year, na.rm = T))
table_extra <- get_season_team_stats(country = c("ENG", "GER", "ITA", "ESP", "FRA"), gender = "M", 
                                             season_end_year = max(table$Season_End_Year, na.rm = T), 
                                             stat_type = "league_table", tier = "1st")


results <- rbind(results, results_extra)

results <- results %>% 
  filter(!is.na(HomeGoals)) %>% 
  arrange(Season_End_Year, Competition_Name, Date) %>% 
  distinct(Competition_Name, Date, Home, Away, .keep_all = T)




table <- rbind(table, table_extra)


table <- table %>% 
  arrange(Competition_Name, Season_End_Year, desc(Pts, MP)) %>% 
  distinct(Competition_Name, Season_End_Year, Squad, .keep_all = T)



clean_data <- function(df) {
  
  long_data <- df %>% 
    select(Competition_Name, Season_End_Year, Date, Time, Squad=Home, GF=HomeGoals, Opponent=Away, GA=AwayGoals) %>% 
    mutate(home_away="Home") %>% 
    bind_rows(
      df %>% 
        select(Competition_Name, Season_End_Year, Date, Time, Squad=Away, GF=AwayGoals, Opponent=Home, GA=HomeGoals) %>% 
        mutate(home_away="Away")
    ) %>% 
    arrange(Competition_Name, Season_End_Year, Squad, Date) %>% 
    group_by(Competition_Name, Season_End_Year, Squad) %>% mutate(game_num = row_number()) %>% ungroup() %>% 
    mutate(match_points_earned = case_when(
      GF == GA ~ 1,
      GF > GA ~ 3,
      TRUE ~ 0
    )) %>%
    group_by(Competition_Name, Season_End_Year, Squad) %>% 
    mutate(cumulated_points = cumsum(match_points_earned)) %>% 
    mutate(points_per_match = cumulated_points / game_num) %>% ungroup()
  
  
  long_data <- long_data %>%
    left_join(table, by = c("Competition_Name", "Season_End_Year", "Squad")) %>%
    mutate(exp_Pts = points_per_match * MP)
  
  return(long_data)
  
}


reg_season_train <- results %>%
  filter(Season_End_Year <= 2020) %>% 
  clean_data(df=.)

# df <- reg_season_train %>% filter(Competition_Name == "Fußball-Bundesliga")
# match_num <- 17

calculate_rsquared_each_match <- function(df, match_num) {
  each_lm <- df %>%
    # filter(Competition_Name == league) %>%
    filter(game_num == match_num) %>% 
    lm(Pts ~ points_per_match + MP, data = .) %>% summary 
  
  adj_r <- each_lm %>% .[["adj.r.squared"]]
  intercept <- each_lm[["coefficients"]][1,1]
  exp_Pts_coef <- each_lm[["coefficients"]][2,1]
  MP_coef <- tryCatch(each_lm[["coefficients"]][3,1], error = function(e) 0)
  
  each_lm <- data.frame(match_number=match_num, r_squared=adj_r, intercept=intercept, exp_Pts_coef=exp_Pts_coef, MP_coef = MP_coef)
  return(each_lm)
}


model_coefficients <- data.frame()
each_league_model_coefficients <- data.frame()

for(each_league in unique(reg_season_train$Competition_Name)) {
  Competition <- each_league
  
  each_league_df <- reg_season_train %>% filter(Competition_Name == each_league)
  
  for(i in c(1:(max(each_league_df$MP)-1))) {
    each_df <- calculate_rsquared_each_match(df=each_league_df, match_num = i)
    each_df$Competition <- Competition
    each_league_model_coefficients <- rbind(each_league_model_coefficients, each_df)
  }
  model_coefficients <- bind_rows(model_coefficients, each_league_model_coefficients)
  
}

model_coefficients <- model_coefficients %>% distinct(.keep_all = T)



reg_season_test <- results %>%
  filter(Season_End_Year == 2020) %>% 
  clean_data(df=.) %>% 
  left_join(model_coefficients, by = c("game_num" = "match_number")) %>% 
  mutate(predicted_points = intercept + (exp_Pts_coef * points_per_match) + (MP_coef * MP))


# rmse_comp <- data.frame()
# 
# for(i in c(1:(max(each_league_df$MP)-1))) {
#   each_df <- reg_season_test %>% 
#     filter(game_num == i)
#   
#   rmse_comp[i, "game_num"] <- i
#   rmse_comp[i, "lm_rmse"] <- round(Metrics::rmse(each_df$Pts, each_df$predicted_points), 2)
#   rmse_comp[i, "rate_xPts_rmse"] <- round(Metrics::rmse(each_df$Pts, each_df$exp_Pts), 2)
# }


rmse_all_comps <- data.frame()
rmse_comp <- data.frame()

for(each_league in unique(reg_season_test$Competition_Name)) {
  Competition <- each_league
  
  each_league_df <- reg_season_test %>% filter(Competition_Name == each_league)
  
  for(i in c(1:(max(each_league_df$MP)-1))) {
    each_df <- each_league_df %>% 
      filter(game_num == i)
    
    rmse_comp[i, "Competition_Name"] <- Competition
    rmse_comp[i, "game_num"] <- i
    rmse_comp[i, "lm_rmse"] <- round(Metrics::rmse(each_df$Pts, each_df$predicted_points), 2)
    rmse_comp[i, "rate_xPts_rmse"] <- round(Metrics::rmse(each_df$Pts, each_df$exp_Pts), 2)
  }
  rmse_all_comps <- bind_rows(rmse_all_comps, rmse_comp)
  
}

rmse_all_comps <- rmse_all_comps %>% 
  distinct(.keep_all = T)


half_games <- rmse_all_comps %>% 
  group_by(Competition_Name) %>% 
  summarise(max_games = max(game_num+1, na.rm = T), 
            half_games = max_games/2) %>% ungroup()

rmse_all_comps %>% 
  ggplot(aes(x=game_num)) +
  geom_line(aes(y=lm_rmse), colour="green") +
  geom_line(aes(y=rate_xPts_rmse), colour="red") +
  geom_vline(data = half_games, aes(xintercept = half_games)) +
  facet_wrap(~ Competition_Name)



model_coefficients %>% ggplot(aes(x=match_number, y=r_squared, group=Competition, colour=Competition)) + geom_line()

season_cors <- results %>% clean_data(.) %>% 
  ungroup() %>% 
  group_by(Season_End_Year, game_num) %>% 
  summarise(season_cor = round(cor(exp_Pts, Pts), 3))

# season_cors
# 
# a <- results %>%
#   clean_data(.)
# 
# cor(a$exp_Pts, a$Pts)

results %>%
  clean_data(.) %>% 
  filter(game_num == 19) %>% 
  ggplot(aes(x=exp_Pts, y= Pts)) +
  geom_smooth(method = "lm", se=F, colour="steelblue") +
  geom_abline(intercept = 0, slope = 1, size=1, colour="orange") +
  geom_point(position = "jitter") +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  scale_y_continuous(breaks = seq(0, 100, 25)) +
  geom_text(data = season_cors %>% filter(game_num == 19), aes(label= paste0("Correlation: ", season_cor)), x=25, y=100) +
  # scale_x_continuous(limits = c(0, 105), breaks = seq(0, 105, 15)) +
  # scale_y_continuous(limits = c(0, 105), breaks = seq(0, 105, 15)) +
  ggtitle("PREDICTED POINTS AFTER 19 MATCHES", subtitle = "As calculated by points per match at week 10, multipled by matches for the season") +
  facet_wrap(~ Season_End_Year, nrow=2)


reg_season_test %>% 
  filter(game_num == 19) %>% 
  ggplot(aes(x=predicted_points, y= Pts)) +
  geom_smooth(method = "lm", se=F, colour="steelblue") +
  # geom_abline(intercept = 0, slope = 1, size=1, colour="orange") +
  geom_point(position = "jitter") +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  scale_y_continuous(breaks = seq(0, 100, 25)) +
  # geom_text(data = season_cors %>% filter(game_num == 19), aes(label= paste0("Correlation: ", season_cor)), x=25, y=100) +
  # scale_x_continuous(limits = c(0, 105), breaks = seq(0, 105, 15)) +
  # scale_y_continuous(limits = c(0, 105), breaks = seq(0, 105, 15)) +
  ggtitle("PREDICTED POINTS AFTER 19 MATCHES", subtitle = "As calculated by points per match at week 10, multipled by matches for the season") +
  facet_grid(~ Competition_Name)



season_cors_french <- results %>% clean_data(.) %>%
  filter(Competition_Name == "Ligue 1") %>%
  ungroup() %>% 
  group_by(Season_End_Year, game_num) %>% 
  summarise(season_cor = round(cor(exp_Pts, Pts), 3))

results %>%
  clean_data(.) %>%
  filter(Competition_Name == "Ligue 1") %>% 
  filter(game_num == 14) %>%
  ggplot(aes(x=exp_Pts, y= Pts)) +
  geom_smooth(method = "lm", se=F, colour="steelblue") +
  geom_abline(intercept = 0, slope = 1, size=1, colour="orange") +
  geom_point(position = "jitter") +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  scale_y_continuous(breaks = seq(0, 100, 25)) +
  geom_text(data = season_cors_french %>% filter(game_num == 14), aes(label= paste0("Correlation: ", season_cor)), x=25, y=100) +
  # scale_x_continuous(limits = c(0, 105), breaks = seq(0, 105, 15)) +
  # scale_y_continuous(limits = c(0, 105), breaks = seq(0, 105, 15)) +
  ggtitle("PREDICTED POINTS AFTER 19 MATCHES", subtitle = "As calculated by points per match at week 10, multipled by matches for the season") +
  facet_wrap(~ Season_End_Year, nrow=2)




