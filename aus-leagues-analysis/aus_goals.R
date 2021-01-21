library(tidyverse)
library(worldfootballR)
library(gt)


aus_urls <- get_match_urls(country = "AUS", gender = c("M", "F"), season_end_year = c(2018:2021))

aus_f_m <- get_match_report(aus_urls)

saveRDS(aus_f_m, here::here("aus_men_and_women_2019-21.rds"))

aus_f_m <- readRDS("aus_men_and_women_2019-21.rds")

get_first_goal <- function(goals_string) {
  first_goal <- goals_string %>% 
    gsub("\\+[[:digit:]]", " ", .) %>% 
    str_extract_all(., "[[:digit:]]+") %>% 
    unlist() %>% as.numeric() %>% min()
  if(is.infinite(first_goal)) {
    first_goal <- NA
  }
  
  return(first_goal)
}

aus_f_m <- aus_f_m %>% 
  # filter so only regular season
  filter(str_detect(Matchweek, "Regular Season")) %>% 
  mutate(total_goals = Home_Score + Away_Score) %>% 
  mutate(first_home_goal = sapply(Home_Goals, get_first_goal),
         first_away_goal = sapply(Away_Goals, get_first_goal)) 


season_summaries <- aus_f_m %>% 
  filter(Season != "2017-2018") %>% 
  mutate(first_goal = case_when(
    first_home_goal < first_away_goal ~ first_home_goal,
    TRUE ~ first_away_goal
  )) %>% 
  mutate(is_Goal = total_goals >0) %>% 
  group_by(Season, League) %>% 
  summarise(n_games = n_distinct(Game_URL),
            goal_game_perc = scales::percent(mean(is_Goal)),
            total_goals_season = sum(total_goals),
            avg_goals_game = round(mean(total_goals),2),
            avg_first_goal = round(mean(first_goal, na.rm = T), 2),
            avg_first_home = mean(first_home_goal, na.rm = T),
            avg_first_away = mean(first_away_goal, na.rm = T)) %>% ungroup() %>% 
  mutate(goals_per_hour = (total_goals_season / (n_games * 90) * 60))




test <- season_summaries %>% 
  select(Season, League, n_games, goal_game_perc, avg_goals_game, avg_first_goal) %>% 
  pivot_wider(names_from = League, values_from = c(n_games:avg_first_goal))


sysfonts::font_add_google(name = "Chivo", family = "chivo")

final_table <- test %>% 
  gt(rowname_col = "Season") %>% 
  tab_spanner(
    label = "# Games",
    columns = vars(`n_games_A-League`, `n_games_W-League`)
  ) %>%
  tab_spanner(
    label = "% Games with Goal",
    columns = vars(`goal_game_perc_A-League`, `goal_game_perc_W-League`)
  ) %>%
  tab_spanner(
    label = "Goals/Game",
    columns = vars(`avg_goals_game_A-League`, `avg_goals_game_W-League`)
  ) %>%
  tab_spanner(
    label = "Avg Mins First Goal",
    columns = vars(`avg_first_goal_A-League`, `avg_first_goal_W-League`)
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "lightblue"),
      cell_text(weight = "bold", color = "white")
    ),
    locations = cells_body(
      columns = vars(`avg_goals_game_A-League`, `avg_goals_game_W-League`),
      rows = "2020-2021")
  ) %>%
  cols_label(
    `n_games_A-League` = "A-League",
    `n_games_W-League` = "W-League",
    `goal_game_perc_A-League` = "A-League", 
    `goal_game_perc_W-League` = "W-League",
    `avg_goals_game_A-League` = "A-League", 
    `avg_goals_game_W-League` = "W-League",
    `avg_first_goal_A-League` = "A-League", 
    `avg_first_goal_W-League` = "W-League"
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center")
    ),
    locations = list(
      cells_column_labels(gt::everything()),
      cells_column_spanners(gt::everything())
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = list(
      cells_body(gt::everything()),
      cells_row_groups(gt::everything())
    )
  ) %>%
  tab_header(
    title = md("**W-LEAGUE GAMES FEATURE MORE GOALS, AND SEE THEM MORE QUICKLY**"),
    subtitle = md("*Almost a goal a game more being scored in the W-League to start this season, while the goals are coming 13 minutes quicker on average than in the A-League*")
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "Chivo", size = "xx-large", align = "center")
    ),
    locations = list(
      cells_title(groups = "title")
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(font = "Chivo", size = "x-large", align = "center")
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  ) %>%
  tab_source_note(
    source_note = md("SOURCE: [worldfootballR](https://github.com/JaseZiv/worldfootballR)<br>TABLE: [@jase_ziv](https://twitter.com/jaseziv)")
  ) %>% 
  tab_footnote(
    footnote = md("*Matches without goals excluded from calculation*"), 
    locations = cells_column_labels(
      columns = c(7,8) # note
    )
  ) %>% 
  cols_align(
    align = "center"
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "bottom", color = "lightgrey", weight = px(1)
    ),
    locations = cells_body(
      columns = TRUE
    )
  )  %>% 
  tab_options(
    column_labels.background.color = "white",
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    table.font.size = 16,
    heading.align = "left"
  ) %>% 
  opt_table_font(
    font = c(
      google_font(name = "Chivo"),
      default_fonts()
    )
  ) 

final_table


