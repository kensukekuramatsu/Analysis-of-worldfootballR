library(worldfootballR)
library(tidyverse)
library(scales)
library(gt)


# aleague_vals <- get_player_market_values(country_name = "Australia", start_year = 2020)
# saveRDS(aleague_vals, here::here("player_valuations", "aleague-valuations", "aleague_vals.rds"))
# 
# aleague_table <- get_season_team_stats(country = "AUS", gender = "M", season_end_year = "2021", stat_type = "league_table")
# saveRDS(aleague_table, here::here("player_valuations", "aleague-valuations", "aleague_table.rds"))


aleague_vals <- readRDS(here::here("player_valuations", "aleague-valuations", "aleague_vals.rds"))
aleague_table <- readRDS(here::here("player_valuations", "aleague-valuations", "aleague_table.rds"))





aleague_table$perc_points_earned <- aleague_table$Pts / (aleague_table$MP * 3)

aleague_table$pts_per_90 <- aleague_table$Pts / aleague_table$MP

aleague_table <- aleague_table %>% 
  mutate(Squad = case_when(
    Squad == "Adelaide" ~ "Adelaide United",
    Squad == "Brisbane" ~ "Brisbane Roar",
    Squad == "Central Coast" ~ "Central Coast Mariners",
    Squad == "Melb City" ~ "Melbourne City FC",
    Squad == "Melb Victory" ~ "Melbourne Victory",
    Squad == "Newcastle" ~ "Newcastle United Jets",
    Squad == "W Sydney" ~ "Western Sydney Wanderers",
    Squad == "Wellington" ~ "Wellington Phoenix",
    Squad == "Western United" ~ "Western United FC",
    TRUE ~ Squad
  ))


aleague_vals <- aleague_vals %>% 
  left_join(aleague_table %>% select(Squad, perc_points_earned, pts_per_90), by = c("squad" = "Squad"))


aleague_vals <- aleague_vals %>% 
  mutate(player_market_value_euro = ifelse(is.na(player_market_value_euro), 0, player_market_value_euro))

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
      heading.title.font.size = "xx-large",
      heading.subtitle.font.size = "x-large",
      ...
    ) 
}

top_value_players_gt <- aleague_vals %>% 
  arrange(desc(player_market_value_euro)) %>%
  mutate(Rank = row_number()) %>% 
  select(Rank, player_name, squad, player_age, player_position, player_nationality, player_market_value_euro) %>% 
  mutate(player_market_value_euro = scales::dollar(player_market_value_euro, prefix = "€")) %>% 
  head(20) %>% 
  bind_rows(
    aleague_vals %>% 
      arrange(desc(player_market_value_euro)) %>%
      mutate(Rank = row_number()) %>% 
      filter(player_name == "Iker Guarrotxena") %>% 
      select(Rank, player_name, squad, player_age, player_position, player_nationality, player_market_value_euro) %>% 
      mutate(player_market_value_euro = scales::dollar(player_market_value_euro, prefix = "€"))
  ) %>% 
  gt() %>% 
  tab_style(
    style = list(
      cell_fill(color = alpha("orange", 0.6))
    ),
    locations = cells_body(
      columns = vars(Rank, player_name, squad, player_age, player_position, player_nationality, player_market_value_euro), # not needed if coloring all columns
      rows = c(1:3))
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = alpha("yellow", 0.6))
    ),
    locations = cells_body(
      columns = vars(Rank, player_name, squad, player_age, player_position, player_nationality, player_market_value_euro), # not needed if coloring all columns
      rows = c(21))
  ) %>% 
  tab_header(md("**THE A-LEAGUE'S MONEY MEN**"),
             subtitle = "The 20 most valuable A-League players according to transfermarkt.com. There are six players valued at over €1M, while the three most expensive players all hail from Spain. After watching Iker Guarrotxena dominate against Macarthur, surprisingly he isn't one of the three.") %>% 
  cols_label(player_name = "Player",
             squad = "Club",
             player_age = "Age",
             player_position = "Position",
             player_nationality = "Nationality",
             player_market_value_euro = "Value (Euros)") %>%
  tab_source_note(
    source_note = md("SOURCE: [transfermarkt](transfermarkt.com) || [worldfootballR](https://github.com/JaseZiv/worldfootballR)<br>TABLE: [@jaseziv](https://twitter.com/jaseziv)")
  ) %>% 
  gt_theme_538()

top_value_players_gt


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

aleague_vals %>% 
  group_by(squad) %>% 
  summarise(median_value = median(player_market_value_euro)) %>% ungroup() %>% 
  ggplot(aes(x=median_value, y= reorder(squad, median_value))) +
  geom_col() +
  geom_text(aes(label= dollar(median_value, prefix = "€")), hjust=0) +
  scale_x_continuous(limits = c(0,400000)) +
  ggtitle("WESTERN UNITED IS WHERE THE VALUE IS AT",
          "The newest Victorian team have the highest median player valuation of all the clubs\nthis season, with a median player value of €350k, while the Reds of Adelaide have\nthe lowest, with a median player value of €150k") +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())



aleague_vals %>% 
  group_by(squad, pts_per_90) %>% 
  summarise(median_value = median(player_market_value_euro)) %>% ungroup() %>% 
  mutate(dollars_to_points_per = median_value / pts_per_90) %>% 
  ggplot(aes(x=dollars_to_points_per, y= reorder(squad, -dollars_to_points_per))) +
  geom_col() +
  geom_text(aes(label= dollar(dollars_to_points_per, prefix = "€")), hjust=0) +
  scale_x_continuous(limits = c(0,400000)) +
  ggtitle("CENTRAL COST MAKING THE MOST OF EACH DOLLAR",
          "The Euro per points per 90 minutes earned this season") +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())



aleague_vals %>% 
  group_by(squad, pts_per_90) %>% 
  summarise(total_value = sum(player_market_value_euro, na.rm = T)) %>% ungroup() %>% 
  mutate(dollars_to_points_per = total_value / pts_per_90) %>% 
  ggplot(aes(x=dollars_to_points_per, y= reorder(squad, -dollars_to_points_per))) +
  geom_col() +
  geom_text(aes(label= dollar(dollars_to_points_per, prefix = "€")), hjust=0) +
  scale_x_continuous(limits = c(0,10750000)) +
  ggtitle("CENTRAL COST MAKING THE MOST OF EACH DOLLAR",
          "The Euro per points per 90 minutes earned this season") +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())



aleague_vals %>% 
  group_by(squad, pts_per_90) %>% 
  summarise(total_value = sum(player_market_value_euro, na.rm = T)) %>% ungroup() %>% 
  ggplot(aes(x=total_value, y= pts_per_90)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = squad)) +
  scale_x_continuous(label= label_dollar(prefix = "€")) +
  ggtitle("UNHERALDED CENTRAL COST MAKING THE MOST OF EACH EURO OF VALUE",
          "The Euro per points per 90 minutes earned this season")







aleague_vals %>% 
  arrange(desc(player_market_value_euro)) %>%
  select(player_name, squad, player_age, player_position, player_nationality, player_market_value_euro) %>% 
  mutate(player_market_value_euro = scales::dollar(player_market_value_euro, prefix = "€")) %>% 
  head(20) %>% 
  count(squad, sort = T)

aleague_vals %>% 
  ggplot(aes(x=player_market_value_euro, y= squad)) +
  geom_boxplot() +
  # geom_point(alpha=0.5, aes(colour=squad)) +
  scale_x_continuous(labels = scales::label_dollar(prefix = "€")) +
  geom_vline(xintercept = median(aleague_vals$player_market_value_euro, na.rm = T))


aleague_vals %>% 
  group_by(player_age) %>% 
  summarise(median_val = median(player_market_value_euro, na.rm = T)) %>% 
  ggplot(aes(x= player_age, y= median_val, group=1)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "€"))
