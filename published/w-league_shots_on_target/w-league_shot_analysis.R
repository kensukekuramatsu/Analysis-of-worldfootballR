library(tidyverse)
library(worldfootballR)
library(gt)

w_urls <- get_match_urls(country = "AUS", gender = "F", season_end_year = c(2018:2021))

#----- Initial Scrape -----#
# wleague <- get_advanced_match_stats(match_url = w_urls, stat_type = "summary", team_or_player = "team")
# saveRDS(wleague, here::here("w-league_shots", "w-league_team_match_summary.rds"))

# #----- Additional Scrapes -----#
# wleague <- readRDS(here::here("published", "w-league_shots_on_target", "w-league_team_match_summary.rds"))
# 
# additional_urls <- w_urls[!w_urls %in% wleague$Game_URL]
# wleague_extra <- get_advanced_match_stats(match_url = additional_urls, stat_type = "summary", team_or_player = "team")
# 
# wleague <- bind_rows(wleague, wleague_extra)
# rm(wleague_extra)
# 
# saveRDS(wleague, here::here("published", "w-league_shots_on_target", "w-league_team_match_summary.rds"))

wleague <- readRDS(here::here("published", "w-league_shots_on_target", "w-league_team_match_summary.rds"))




sysfonts::font_add_google(name = "Chivo", family = "chivo")
showtext::showtext_auto()

theme_set(theme_minimal() +
            theme(plot.title = element_text(colour = "black", size = 22, family = "chivo"), 
                  plot.subtitle = element_text(colour = "grey20", size = 16, family = "chivo"),
                  axis.text.x = element_text(colour = "grey20", size = 12, family = "chivo"),
                  axis.text.y = element_text(colour = "grey20", size = 12, family = "chivo"),
                  axis.title.x = element_text(colour = "grey20", size = 12, family = "chivo"),
                  axis.title.y = element_text(colour = "grey20", size = 12, family = "chivo"),
                  plot.caption = element_text(colour = "grey20", size = 10, family = "chivo"),
                  strip.text = element_text(colour = "grey30", size = 12, face = "bold", family = "chivo")))


wleague %>% 
  ggplot(aes(x= Sh, y= SoT)) +
  geom_point(colour="steelblue") +
  geom_smooth(method = "lm", se=F, colour="orange", linetype=2) +
  ggtitle("MORE SHOTS, MORE ON TARGET IN THE W-LEAGUE",
          subtitle = "Orange line represents the expected SoT for shot totals. Points above the line\nindicate team performed better than expected") +
  labs(caption = "*Data current to 18/01/2021\nSource: worldfootballR  ||  Table: @jase_ziv")

expected_SoT_lm <- wleague %>% lm(SoT ~ Sh, data = .)
summary(expected_SoT_lm)

wleague <- wleague %>% 
  mutate(expected_SoT = predict(expected_SoT_lm),
         SoT_above_expected = SoT - expected_SoT)



# wleague %>% 
#   ggplot(aes(x= SoT_above_expected, y= Team)) +
#   geom_boxplot(colour = "steelblue") +
#   geom_vline(xintercept = 0, colour = "orange", linetype=2) +
#   facet_wrap(~ Season) +
#   theme(axis.title.y = element_blank())



gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
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
      heading.align = "left",
      heading.title.font.size = "x-large",
      heading.subtitle.font.size = "large",
      ...
    ) 
}


# worst performing SoT above expectation team seasons
output_table <- wleague %>% 
  # filter(Season == "2020-2021") %>%
  group_by(Team, Season) %>% 
  summarise(n_games = n_distinct(Game_URL),
            total_shots = sum(Sh),
            total_SoT = sum(SoT),
            avg_SoT_above_exp = round(mean(SoT_above_expected), 3)) %>% 
  arrange(avg_SoT_above_exp) %>% ungroup() %>% head(15) %>% 
  gt() %>% 
  data_color(
    columns = vars(avg_SoT_above_exp),
    colors = scales::col_numeric(
      palette = c("#3fc1c9", "white"),
      domain = NULL
    )
  ) %>% 
  tab_source_note(
    source_note = md("SOURCE: [worldfootballR](https://github.com/JaseZiv/worldfootballR)<br>TABLE: [@jase_ziv](https://twitter.com/jaseziv)")
  ) %>% 
  tab_footnote(
    footnote = md("Linear Model: Expected SoT = Total Shots x 0.41689"), 
    locations = cells_column_labels(
      columns = c(6) # note
    )
  ) %>% 
  cols_label(n_games = "MP",
             total_shots = "Total Shots (Season)",
             total_SoT = "Total Shots on Traget (Season)",
             avg_SoT_above_exp = "Avg SoT Above Expected") %>% 
  tab_header(title = md("**A WAYWARD BRISBANE THIS SEASON**"),
             subtitle = "Brisbane are currently having the worst season in regards to Expected Shots on Target, as modelled by the the number of shots taken") %>% 
  gt_theme_538()

output_table

gtsave(output_table, here::here("published", "w-league_shots_on_target", "SoT_above_expected.html"))
       