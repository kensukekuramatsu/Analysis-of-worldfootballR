library(worldfootballR)
library(tidyverse)
library(here)

options(scipen = 999)
# Set Theme ---------------------------------------------------------------
# load fonts
sysfonts::font_add_google(name = "Chivo", family = "chivo")
showtext::showtext_auto()

# set theme
theme_set(theme_minimal() +
            theme(plot.title = element_text(colour = "black", size = 26, family = "chivo", face = "bold"),
                  plot.title.position = "plot",
                  plot.subtitle = element_text(colour = "grey20", size = 20, family = "chivo"),
                  axis.text.x = element_text(colour = "grey20", size = 14, family = "chivo"),
                  axis.text.y = element_text(colour = "grey20", size = 14, family = "chivo"),
                  axis.title.x = element_text(colour = "grey20", size = 16, family = "chivo"),
                  axis.title.y = element_text(colour = "grey20", size = 16, family = "chivo"),
                  plot.caption = element_text(colour = "grey20", size = 13, family = "chivo"),
                  strip.text = element_text(colour = "grey30", size = 16, face = "bold", family = "chivo"),
                  legend.text = element_text(colour = "grey30", size = 14, family = "chivo"),
                  legend.title = element_text(colour = "grey30", size = 16, face = "bold", family = "chivo"),
                  text = element_text(colour = "grey20", size = 14, family = "chivo")))


# Initial Data collection: ------------------------------------------------
# aleague <- get_match_results(country = "AUS", gender = "M", season_end_year = c(2006:2021))
# saveRDS(aleague, here::here("form-predict-points", "aleague_results.rds"))
# 
# aleague_table <- get_season_team_stats(country = "AUS", gender = "M", season_end_year = c(2006:2021), stat_type = "league_table")
# saveRDS(aleague_table, here::here("form-predict-points", "aleague_table.rds"))

aleague <- readRDS(here::here("form-predict-points", "aleague_results.rds"))
aleague_table <- readRDS(here::here("form-predict-points", "aleague_table.rds"))

# Incremental data collection: ------------------------------------------------

aleague_extra <- get_match_results(country = "AUS", gender = "M", 
                             season_end_year = max(aleague$Season_End_Year, na.rm = T))
aleague_table_extra <- get_season_team_stats(country = "AUS", gender = "M", 
                                       season_end_year = max(aleague_table$Season_End_Year, na.rm = T), 
                                       stat_type = "league_table", tier = "1st")


aleague <- rbind(aleague, aleague_extra)

aleague <- aleague %>% 
  filter(!is.na(HomeGoals)) %>% 
  arrange(Season_End_Year, Date) %>% 
  distinct(Date, Home, Away, .keep_all = T)




aleague_table <- rbind(aleague_table, aleague_table_extra)


aleague_table <- aleague_table %>% 
  arrange(Season_End_Year, desc(Pts, MP)) %>% 
  distinct(Season_End_Year, Squad, .keep_all = T)




aleague_reg_season <- aleague %>% filter(Round == "Regular Season" | is.na(Round)) # %>% filter(Season_End_Year < 2021)
aleague_table <- aleague_table %>% select(Season_End_Year, Squad, MP, Pts)




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
    left_join(aleague_table, by = c("Season_End_Year", "Squad")) %>%
    mutate(exp_Pts = points_per_match * MP)
  
  return(long_data)
  
}


aleague_reg_season_train <- clean_data(df=aleague_reg_season%>% filter(Season_End_Year < 2021))

elo <- readRDS(here("elo", "each_season_elo.rds"))

elo <- elo %>%
  arrange(Team, Date) %>% 
  mutate(elo_before_game = lag(rating)) %>% 
  filter(!is.na(Season_End))


test <- aleague_reg_season_train %>% 
  left_join(elo %>% select(Date, Team, elo_before_game), by = c("Opponent" = "Team", "Date"))



calculate_rsquared_each_match <- function(df, match_num) {
  each_lm <- df %>%
    # filter(Season_End_Year < 2021) %>% 
    filter(game_num == match_num) %>% 
    lm(Pts ~ cumulated_points, data = .) %>% summary 
  
  adj_r <- each_lm %>% .[["adj.r.squared"]]
  intercept <- each_lm[["coefficients"]][1,1]
  exp_Pts_coef <- each_lm[["coefficients"]][2,1]
  
  each_lm <- data.frame(match_number=match_num, r_squared=adj_r, intercept=intercept, exp_Pts_coef=exp_Pts_coef)
  return(each_lm)
}


model_coefficients <- data.frame()
games_to_model <- max(aleague_reg_season_train$game_num, na.rm = T)-1

for(i in c(1:games_to_model)) {
  each_df <- calculate_rsquared_each_match(df=aleague_reg_season_train, match_num = i)
  model_coefficients <- rbind(model_coefficients, each_df)
}


# lm_each_match_num <- function(df, match_num) {
#   each_lm <- df %>%
#     # filter(Season_End_Year < 2021) %>% 
#     filter(game_num == match_num) %>% 
#     lm(Pts ~ points_per_match, data = .) 
#   
#   out_df <- df %>% 
#     filter(game_num == match_num)
#   
#   out_df$predicted_points <- predict(each_lm, out_df)
#   
#   return(out_df)
# }




# historic_data <- data.frame()
# 
# for(i in c(1:26)) {
#   each_match <- lm_each_match_num(df=aleague_reg_season_train, match_num = i)
#   
#   historic_data <- rbind(historic_data, each_match)
# }
# 
# historic_data <- historic_data %>% arrange(Season_End_Year, Date, Time)



historic_data <- aleague_reg_season_train %>% 
  left_join(model_coefficients, by = c("game_num" = "match_number")) %>% 
  # mutate(predicted_points = intercept + (exp_Pts_coef * points_per_match)) %>% 
  mutate(predicted_points = intercept + (exp_Pts_coef * cumulated_points))







this_season <- aleague %>%
  filter(Season_End_Year == 2021) %>% 
  filter(!is.na(HomeGoals)) %>% 
  clean_data(.) %>% 
  mutate(full_season_exp_Pts = points_per_match * 27)


this_season <- this_season %>% 
  left_join(model_coefficients, by = c("game_num" = "match_number")) %>% 
  # mutate(predicted_points = intercept + (exp_Pts_coef * points_per_match)) %>% 
  mutate(predicted_points = intercept + (exp_Pts_coef * cumulated_points))




predicted_table <- aleague_table %>% 
  filter(Season_End_Year == 2021) %>% 
  left_join(this_season %>% arrange(Squad, desc(game_num)) %>% distinct(Squad, .keep_all=T) %>% select(Season_End_Year, Squad, predicted_points, full_season_exp_Pts), by = c("Season_End_Year", "Squad"))



rmse_comp <- data.frame()

for(i in c(1:26)) {
  each_df <- historic_data %>% 
    filter(game_num == i)
  
  rmse_comp[i, "game_num"] <- i
  rmse_comp[i, "lm_rmse"] <- Metrics::rmse(each_df$Pts, each_df$predicted_points)
  rmse_comp[i, "rate_xPts_rmse"] <- Metrics::rmse(each_df$Pts, each_df$exp_Pts)
}


rmse_comp %>% 
  ggplot() +
  geom_line(aes(x= game_num, y= lm_rmse, group=1)) +
  geom_line(aes(x=game_num, y= rate_xPts_rmse, group=1), colour="red")





model_coefficients %>% ggplot(aes(x=match_number, y=r_squared)) + geom_point()

historic_data %>%
  filter(game_num == 12) %>% 
  ggplot(aes(x=predicted_points, y= Pts)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
  ggtitle("PREDICTED POINTS AFTER 10 MATCHES", subtitle = "As calculated by points per match at week 12, multipled by matches for the season") +
  facet_wrap(~ Season_End_Year, nrow=2)



historic_data %>%
  filter(game_num == 10) %>% 
  ggplot(aes(x=points_per_match, y= Pts)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5)) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
  ggtitle("POINTS PER 90 AFTER 10 MATCHES", subtitle = "As calculated by points per match at week 10, multipled by matches for the season")




historic_data %>%
  filter(game_num < 26) %>% 
  mutate(error = predicted_points - Pts) %>% 
  ggplot(aes(x=error)) +
  geom_histogram() +
  facet_wrap(~ game_num, scales="free", ncol=5)


historic_data %>%
  filter(game_num == 10) %>% 
  ggplot(aes(x=points_per_match, y= Pts)) +
  geom_smooth(method = "lm", se=F) +
  geom_point() +
  facet_wrap(~ Season_End_Year, nrow=2)


historic_data %>%
  filter(game_num == 12) %>% 
  mutate(pred_error = predicted_points - Pts) %>% 
  arrange(pred_error) %>% 
  select(Season_End_Year, Squad, predicted_points, Pts) %>% head()


historic_data %>%
  filter(game_num == 10) %>% 
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




