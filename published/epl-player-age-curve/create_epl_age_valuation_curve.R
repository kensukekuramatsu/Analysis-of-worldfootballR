# devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)
library(tidyverse)



epl_vals <- get_player_market_values(country_name = "England", start_year = "2020")

write.csv(epl_vals, here::here("published", "epl-player-age-curve", "epl_2021_valuations.csv"), row.names = F)

# load fonts
sysfonts::font_add_google(name = "Chivo", family = "chivo")
showtext::showtext_auto()

# set theme
theme_set(theme_minimal() +
            theme(plot.title = element_text(colour = "black", size = 24, family = "chivo", face = "bold"), 
                  plot.subtitle = element_text(colour = "grey20", size = 18, family = "chivo"),
                  axis.text.x = element_text(colour = "grey20", size = 14, family = "chivo"),
                  axis.text.y = element_text(colour = "grey20", size = 14, family = "chivo"),
                  axis.title.x = element_text(colour = "grey20", size = 16, family = "chivo"),
                  axis.title.y = element_text(colour = "grey20", size = 16, family = "chivo"),
                  plot.caption = element_text(colour = "grey20", size = 12, family = "chivo"),
                  strip.text = element_text(colour = "grey30", size = 14, face = "bold", family = "chivo")))

epl_vals %>% 
  group_by(player_age) %>% 
  summarise(n_players =n(),
            median_val = median(player_market_value_euro, na.rm = T)) %>% 
  ggplot(aes(x= player_age, y= median_val, group=1)) +
  # geom_point(size=3, colour="grey60") +
  geom_line(size=2, colour= "midnightblue") +
  geom_area(colour="midnightblue", fill="midnightblue", alpha=0.5) +
  # geom_text(aes(x=player_age, y= median_val, label=n_players)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x= "Age", y= "Median Value",
       caption = paste0("Data extracted using worldfootballR\nSource: transfermarkt.com  ||  Plot: @jaseziv")) +
  ggtitle("THE MID 20s PLAYERS WILL COST YOU IN THE EPL",
          subtitle = "Using the median player valuations for each year of age of players in the 20/21 EPL season\nshows that player values typically peak when a player is in their mid 20s (24 and 25 year\nolds are the most expensive age), then starts to drop from 26 onwards") +
  theme(plot.title.position = "plot", plot.caption.position = "plot")


ggsave(here::here("published", "epl-player-age-curve", "epl-valuation-age-curve.png"), width = 12, height = 8, units = "in")
