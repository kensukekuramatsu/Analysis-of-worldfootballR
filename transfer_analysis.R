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
  group_by(country, season, transfer_type) %>% 
  summarise(total_fee = sum(transfer_fee, na.rm = T)) %>% 
  pivot_wider(, names_from = transfer_type, values_from = total_fee) %>% ungroup() %>% 
  ggplot(aes(x= season, y= Arrivals)) +
  geom_area(aes(y= Arrivals, group=1), colour = "darkred", fill="darkred", alpha=0.3, size=2) +
  scale_x_continuous(labels = seq(min(short_seasons), max(short_seasons), 2), breaks= seq(2010, 2020, 2)) +
  scale_y_continuous(labels = scales::dollar, name = "Total Fee") +
  labs(caption = "Source: Transfermarkt.com\nPackage: worldfootballR\nVisualisation: @jaseziv") +
  ggtitle("SPENDING IN THE TOP 5 LEAGUES COOLING DOWN?",
          subtitle = "The trend of teams' transfer spending in the top-5 Euro leagues was increasing until the 2017-18 season, however this\nhas been dropping ever since. The financial impact of COVID-19 may see this decreasing trend\ncontinue for some time.") +
  facet_wrap(~ country, nrow=1) +
  theme(axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(linetype = 2, colour = "grey20"), panel.grid.minor.y = element_blank())



