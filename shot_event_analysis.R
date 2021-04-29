library(worldfootballR)
library(tidyverse)
library(rvest)
library(gt)

###########################################################################
# Get Data ----------------------------------------------------------------
###########################################################################

# get URLs
urls <- get_match_urls(country = "ENG", gender = "M", season_end_year = 2021)
# get data
final_out <- get_match_shooting(match_url = urls)

###########################################################################
# Create Table ------------------------------------------------------------
###########################################################################

# load fonts
sysfonts::font_add_google(name = "Chivo", family = "chivo")

###########################
#----- Create Table: -----#
###########################
final_out %>%
  mutate(is_goal = str_detect(Outcome, "Goal")) %>%
  filter(is_goal) %>% 
  group_by(Team, Body_Part) %>%
  summarise(n_goals = n()) %>% 
  mutate(percent_goals = round((n_goals / sum(n_goals) * 100), 2)) %>%
  ungroup() %>% 
  group_by(Team) %>% 
  mutate(`Total Goals` = sum(n_goals)) %>% 
  select(-n_goals) %>%
  ungroup() %>% 
  filter(Body_Part != "Other") %>% 
  pivot_wider(names_from =  Body_Part, values_from = c(percent_goals), values_fill = 0) %>% 
  mutate(`Dominant Body Part` = case_when(
    `Right Foot` > `Left Foot` ~ "Right",
    `Left Foot` > `Right Foot` ~ "Left",
    `Head` > `Left Foot` & `Head` > `Right Foot` ~ "Head",
    TRUE ~ "All Equal"
  )) %>% 
  mutate(`Abs % Difference` = case_when(
    `Dominant Body Part` == "Right" ~ `Right Foot` - `Left Foot`,
    `Dominant Body Part` == "Left" ~ `Left Foot` - `Right Foot`,
    TRUE ~ 0
  )) %>%
  select(-`Right Foot`, -`Left Foot`, -`Head`) %>% 
  gt() %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center")
    ),
    locations = list(
      cells_column_labels(gt::everything()))
    ) %>%
  tab_header(
    title = md("**DOMINANT BODY PARTS OF PREMIER LEAGUE TEAMS SCORING IN 2020/21?**"),
    subtitle = md("*Other than Liverpool, Leeds and Burnley, all teams score more goals with the left boot. Burnley interestingly have scored exactly the same amount of goals with the head, left and right boots (note they have also had two goals classed as 'other'.
                  Man Utd and Southamptom have the largest percentage differential between body part, while Man City and Newcastle prefer the right foot also, they have the lowest differential.*")
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "Chivo", size = "xx-large", align = "left")
    ),
    locations = list(
      cells_title(groups = "title")
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(font = "Chivo", size = "x-large", align = "left")
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  cols_align("left", columns = vars(Team)) %>%
  data_color(
    columns = vars(`Abs % Difference`),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    )
  ) %>% 
  tab_source_note(
    source_note = md("SOURCE: fbref.com || [worldfootballR](https://github.com/JaseZiv/worldfootballR)<br>TABLE: [@jase_ziv](https://twitter.com/jaseziv)")
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



