library(worldfootballR)
library(tidyverse)


aleague <- readRDS(here::here("form-predict-points", "aleague_results.rds"))

# Incremental data collection: ------------------------------------------------

aleague_extra <- get_match_results(country = "AUS", gender = "M", 
                                   season_end_year = max(aleague$Season_End_Year, na.rm = T))


aleague <- rbind(aleague, aleague_extra)

aleague <- aleague %>% 
  filter(!is.na(HomeGoals)) %>% 
  arrange(Season_End_Year, Date) %>% 
  distinct(Date, Home, Away, .keep_all = T)



aleague_reg_season <- aleague %>% 
  filter(Round == "Regular Season" | is.na(Round)) %>% 
  filter(!is.na(HomeGoals)) %>% 
  arrange(Date)
# aleague_table <- aleague_table %>% select(Season_End_Year, Squad, MP, Pts)

aleague_reg_season21 <- aleague_reg_season %>% 
  filter(Season_End_Year == 2021,
         !is.na(HomeGoals)) %>% 
  arrange(Date)



# solutions for the belo elo function found here: https://opisthokonta.net/?p=404
eloRating <- function(home="Home", away="Away", homeGoals="HomeGoals",
                      awayGoals="AwayGoals", data, kfactor=24, initialRating=1500,
                      homeAdvantage=0){
  
  data <- data %>% arrange(Date)
  
  #Make a list to hold ratings for all teams
  all.teams <- levels(as.factor(union(levels(as.factor(data[[home]])),
                                      levels(as.factor(data[[away]])))))
  
  
  start_date <- min(data$Date, na.rm = T) %>% lubridate::floor_date(unit = "month")-1
  ratings <- rep(initialRating, times=length(all.teams))
  # names(ratings) <- all.teams
  ratings <- data.frame(Date = start_date, Team=all.teams, rating=ratings)
  
  ratingsOut <- ratings
  #Loop trough data and update ratings
  for (idx in 1:dim(data)[1]){
    Date <- data$Date[idx]
    #get current ratings
    homeTeamName <- data[[home]][idx]
    awayTeamName <- data[[away]][idx]
    # homeTeamRating <- ratingsOut %>% filter(Team == homeTeamName) %>% arrange(desc(Date)) %>% slice(1) %>% pull(rating) + homeAdvantage
    homeTeamRating <- ratingsOut %>% filter(Team == homeTeamName) %>% arrange(desc(Date)) %>% slice(1) %>% pull(rating)
    awayTeamRating <- ratingsOut %>% filter(Team == awayTeamName) %>% arrange(desc(Date)) %>% slice(1) %>% pull(rating)
    
    #calculate expected outcome 
    expectedHome <- 1 / (1 + 10^(((awayTeamRating - homeTeamRating) + homeAdvantage)/400))
    expectedAway <- 1 - expectedHome
    
    #Observed outcome
    goalDiff <- data[[homeGoals]][idx] - data[[awayGoals]][idx]
    if (goalDiff == 0){
      resultHome <- 0.5
      resultAway <- 0.5
    } else if (goalDiff < 0){
      resultHome <- 0
      resultAway <- 1
    } else if (goalDiff > 0){
      resultHome <- 1
      resultAway <- 0
    }
    
    #update ratings
    home_new_rating <- as.numeric(homeTeamRating) + kfactor*(resultHome - expectedHome)
    away_new_rating <- as.numeric(awayTeamRating) + kfactor*(resultAway - expectedAway)
    
    test_out <- data.frame(Date, Team=homeTeamName, rating=home_new_rating) %>% 
      bind_rows(data.frame(Date, Team=awayTeamName, rating=away_new_rating))
    
    ratingsOut <- bind_rows(ratingsOut, test_out)
  }
  
  return(ratingsOut) 
}






elo_since_2014 <- eloRating(data = aleague_reg_season) %>% data.frame()

  

elo_since_2014 %>% 
  ggplot(aes(x= Date, y= rating, group=Team)) +
  geom_line() +
  facet_wrap(~ Team)

elo_this_season <- eloRating(data = aleague_reg_season21)
elo_this_season_ha <- eloRating(data = aleague_reg_season21, homeAdvantage = 100)
elo_this_season_with_k30_home <- eloRating(data = aleague_reg_season21, k=30, homeAdvantage = 100)


elo_this_season %>% 
  ggplot(aes(x= Date, y= rating, group=Team)) +
  geom_line() +
  facet_wrap(~ Team)

