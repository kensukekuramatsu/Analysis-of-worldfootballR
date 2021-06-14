library(worldfootballR)
library(tidyverse)
library(scales)


# countries <- c("England", "Italy", "Germany", "Spain", "France")
# all_transfers <- data.frame()
# 
# for(each_country in countries) {
#   
#   each_season_df <- data.frame()
#   
#   for(i in c(2010:2020)) {
#     print(paste0("Scraping country: ", each_country, " for season: ", i))
#     urls <- tm_league_team_urls(country_name = each_country, start_year = i)
#     season_transfers <- tm_team_transfers(urls)
#     each_season_df <- rbind(each_season_df, season_transfers)
#   }
#   
#   all_transfers <- rbind(all_transfers, each_season_df)
#   
# }
# 
# 
# 
# 
# all_transfers <- all_transfers %>% rename(season=i)
# 
# saveRDS(all_transfers, "Top5_transfers_2010_2020.rds")


top5_xfers_2010_2020 <- readRDS("Top5_transfers_2010_2020.rds")

top5_xfers_2010_2020 %>% filter(is.na(league)) %>% count(country)

# from the above validation, we can see that there are some Ligue 1 teams without a league, let's fix that:
top5_xfers_2010_2020 <- top5_xfers_2010_2020 %>% 
  mutate(ifelse(is.na(league), "Ligue 1", league))


# load fonts
sysfonts::font_add_google(name = "Chivo", family = "chivo")
showtext::showtext_auto()

sysfonts::font_add_google(name = "Permanent Marker", family = "permanent marker")
showtext::showtext_auto()


# set theme
theme_set(theme_minimal() +
            theme(text = element_text(family = "permanent marker"),
                  plot.background = element_rect(fill = "wheat1"),
                  plot.title = element_text(colour = "grey20", size = 28, face = "bold", family = "permanent marker"), 
                  plot.subtitle = element_text(colour = "grey40", size = 21),
                  plot.title.position = "plot", plot.caption.position = "plot",
                  axis.text.x = element_text(colour = "wheat3", size = 14),
                  axis.text.y = element_text(colour = "wheat3", size = 14),
                  axis.title.x = element_text(colour = "wheat3", size = 16),
                  axis.title.y = element_text(colour = "wheat3", size = 16),
                  plot.caption = element_text(colour = "wheat4", size = 12),
                  strip.text = element_text(colour = "wheat4", size = 18, face = "bold", family = "permanent marker"),
                  legend.title = element_text(colour = "wheat3", size = 14), 
                  legend.text = element_text(colour = "wheat3", size = 12),
                  panel.grid = element_line(colour = "wheat")))


top5_xfers_2010_2020 <- top5_xfers_2010_2020 %>% mutate(player_age = as.numeric(player_age)) %>% 
  filter(player_age < 2000)


str_right <- function(string, n) {
  substr(string, nchar(string) - (n - 1), nchar(string))
}


short_seasons <- unique(top5_xfers_2010_2020$season + 1) %>% str_right(., 2) %>% as.numeric()

top5_xfers_2010_2020 %>% 
  filter(!is.na(country_2)) %>% 
  mutate(home_transfer = country_2 == country) %>% 
  mutate(league = case_when(
    country == "England" ~ "Premier League",
    country == "Italy" ~ "Serie A", 
    country == "Germany" ~ "Bundesliga",
    country == "Spain" ~ "La Liga",
    country == "France" ~ "Ligue 1"
  )) %>% 
  group_by(league, season, transfer_type, home_transfer) %>%
  summarise(n = n()) %>% 
  mutate(perc_home = n / sum(n)) %>% 
  filter(home_transfer) %>% 
  filter(transfer_type == "Arrivals") %>% 
  ggplot(aes(x=season, y= perc_home, group=transfer_type)) +
  geom_area(size=2, colour="steelblue", fill="steelblue", alpha=0.7)  +
  # ggrepel::geom_text_repel(aes(label = percent(perc_home, accuracy = 0.1), colour = transfer_type)) +
  scale_x_continuous(labels = paste0("'", seq(min(short_seasons), max(short_seasons), 2)), breaks= seq(2010, 2020, 2)) +
  scale_y_continuous(limits = c(0, .8), labels = percent) +
  labs(caption = "Source: Transfermarkt.com\nPackage: worldfootballR\nVisualisation: @jaseziv") +
  ggtitle("HOME GROWN TRANSFERS IN THE TOP 5 EURO LEAGUES",
          subtitle = "Since the 2010-11 season, Serie A has the highest proportion of home grown players coming in to the league,\nalthough this trend is falling. The proportion of home grown players in Ligue 1 has also been on a steady\ndecline, as it has in the EPL (although the last two years have seen this trend reverse). La Liga has seen the\nproportion of Spaniards transferring in to clubs on a steady increase since the 2017-18 season.") +
  facet_wrap(~ league, nrow=1) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())