library(tidyverse)
library(worldfootballR)

# get all match URLs
matches <- get_match_urls(country = "ENG", gender = "M", season_end_year = c(2018:2021))

# read in the data already extracted
reds_match_summary_players <- readRDS(here::here("liverpool", "reds_match_summary_players.rds"))
existing_urls <- reds_match_summary_players$Game_URL %>% unique()

liv_urls <- matches[grep("liverpool", tolower(matches))]

# read in the additional game(s) not already collected
additional_reds <- get_advanced_match_stats(match_url = liv_urls[!liv_urls %in% existing_urls],
                                            stat_type = "summary", team_or_player = "player")

# add the two DFs together
reds_match_summary_players <- reds_match_summary_players %>% bind_rows(additional_reds)

# vector for the front three
front_three <- c("Roberto Firmino", "Sadio Mané", "Mohamed Salah")

# filter to get games for only the three of them
front_three_games <- reds_match_summary_players %>% 
  filter(Player %in% front_three)


# create a long df of all players
a <- front_three_games %>% 
  separate(., Match_Date, into = c("Wday", "Month", "Day", "Year"), sep = " ", remove = F) %>% 
  mutate(Day = gsub(",", "", Day) %>% as.numeric(),
         Year = as.numeric(Year),
         Month = match(Month, month.name),
         Match_Date_Clean = lubridate::ymd(paste(Year, Month, Day, sep = "-"))) %>% 
  arrange(Season, Player, Match_Date_Clean) %>% 
  group_by(Season, Player) %>% mutate(game_num = row_number()) %>% 
  filter(!is.na(Gls)) %>% 
  group_by(Season, Player, game_num) %>% 
  summarise(Goals = sum(Gls, na.rm = T),
            xG = sum(xG_Expected, na.rm = T)) %>% ungroup() %>% 
  mutate(goal_above_expected = Goals - xG) %>% 
  group_by(Season, Player) %>% 
  mutate(cum_total_goals = cumsum(Goals),
         cum_total_xg = cumsum(xG)) %>% 
  select(Season, Player, game_num, Goals=cum_total_goals, xG=cum_total_xg) %>% 
  pivot_longer(cols = c(Goals:xG), names_to = "GoalType", values_to = "Goals")


# create a labels DF
lab_df <- a %>% 
  filter(Season == "2020-2021") %>%
  arrange(Player, desc(game_num)) %>% distinct(Season, Player, GoalType, .keep_all = T)

# load fonts
sysfonts::font_add_google(name = "Chivo", family = "chivo")
showtext::showtext_auto()

# set theme
theme_set(theme_minimal() +
            theme(plot.title = element_text(colour = "black", size = 22, family = "chivo"), 
                  plot.subtitle = element_text(colour = "grey20", size = 16, family = "chivo"),
                  axis.text.x = element_text(colour = "grey20", size = 12, family = "chivo"),
                  axis.text.y = element_text(colour = "grey20", size = 12, family = "chivo"),
                  axis.title.x = element_text(colour = "grey20", size = 12, family = "chivo"),
                  axis.title.y = element_text(colour = "grey20", size = 12, family = "chivo"),
                  plot.caption = element_text(colour = "grey20", size = 10, family = "chivo"),
                  strip.text = element_text(colour = "grey30", size = 12, face = "bold", family = "chivo")))


# Final Visualisation -----------------------------------------------------
final_plot <- a %>%
  ggplot(aes(x= game_num, y= Goals, group = GoalType, colour=GoalType)) +
  geom_line(size=1) +
  geom_text(data = lab_df, aes(x=game_num, y= Goals, label = GoalType, colour = GoalType), vjust=0,, hjust=0, family = "chivo", size=5) +
  scale_colour_manual(values = c("black", "orange")) +
  ggtitle("BOBBY AND SADIO NOT CAPITALISING",
          subtitle = "Two of the main front three in Firmino and Mane are scoring below expected, with only\nMo exceeding xG this season. With the raft of injuries at the club, these two need to start\ntaking more of their chances for the Reds to have any chance of a repeat title...\nOr hope Jota is back real soon!") +
  labs(x= "Match Number", y= "Cumulative Goals",
       caption = paste0("\n*Data current to ", lubridate::today(), "\nSource: worldfootballR/fbref.com  ||  Table: @jase_ziv")) +
  facet_grid(Season ~ Player) +
  theme(legend.position = "none")

final_plot

ggsave(here::here("published", "liverpool_front_three_xG", "liverpool_front_three_xG.png"), width = 12, height = 8, units = "in")
