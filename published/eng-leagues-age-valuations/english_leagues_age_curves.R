
# Libraries ---------------------------------------------------------------

# devtools::install_github("JaseZiv/worldfootballR", ref = "main")

library(gridExtra)
library(worldfootballR)
library(tidyverse)
library(magick)
library(scales)


# Get Data ----------------------------------------------------------------

# the below is commented as the data had been extracted and saved as rds to this project. Un-comment for updated data:

# epl_vals <- get_player_market_values(country_name = "England", start_year = 2020)
# saveRDS(epl_vals, here::here("published", "eng-leagues-age-valuations", "epl_vals2021.rds"))
# 
# championship_vals <- get_player_market_values(start_year = 2020, league_url = "https://www.transfermarkt.com/championship/startseite/wettbewerb/GB2")
# saveRDS(championship_vals, here::here("published", "eng-leagues-age-valuations", "campionship_vals2021.rds"))
# 
# league_one_vals <- get_player_market_values(start_year = 2020, league_url = "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3")
# saveRDS(league_one_vals, here::here("published", "eng-leagues-age-valuations", "league_one_vals2021.rds"))
# 
# league_two_vals <- get_player_market_values(start_year = 2020, league_url = "https://www.transfermarkt.com/league-two/startseite/wettbewerb/GB4")
# saveRDS(league_two_vals, here::here("published", "eng-leagues-age-valuations", "league_two_vals2021.rds"))



# Set Theme ---------------------------------------------------------------
# load fonts
sysfonts::font_add_google(name = "Chivo", family = "chivo")
showtext::showtext_auto()

# set theme
theme_set(theme_minimal() +
            theme(plot.title = element_text(colour = "black", size = 26, family = "chivo", face = "bold"), 
                  plot.subtitle = element_text(colour = "grey20", size = 20, family = "chivo"),
                  axis.text.x = element_text(colour = "grey20", size = 14, family = "chivo"),
                  axis.text.y = element_text(colour = "grey20", size = 14, family = "chivo"),
                  axis.title.x = element_text(colour = "grey20", size = 16, family = "chivo"),
                  axis.title.y = element_text(colour = "grey20", size = 16, family = "chivo"),
                  plot.caption = element_text(colour = "grey20", size = 13, family = "chivo"),
                  strip.text = element_text(colour = "grey30", size = 16, face = "bold", family = "chivo"),
                  legend.text = element_text(colour = "grey30", size = 14, family = "chivo"),
                  legend.title = element_text(colour = "grey30", size = 16, face = "bold", family = "chivo")))

# Read in data ------------------------------------------------------------
epl_vals <-readRDS(here::here("published", "eng-leagues-age-valuations", "epl_vals2021.rds"))
championship_vals <- readRDS(here::here("published", "eng-leagues-age-valuations", "campionship_vals2021.rds"))
league_one_vals <- readRDS(here::here("published", "eng-leagues-age-valuations", "league_one_vals2021.rds"))
league_two_vals <- readRDS(here::here("published", "eng-leagues-age-valuations", "league_two_vals2021.rds"))




# Data Wrangling ----------------------------------------------------------
# creata function to calculate the median value for each age and league
age_fun <- function(df, league_name){
  
  league_df <- df %>% 
    mutate(player_market_value_euro = ifelse(is.na(player_market_value_euro), 0, player_market_value_euro)) %>% 
    group_by(player_age) %>% 
    summarise(n_players = n(),
              median_val = median(player_market_value_euro, na.rm = T)) %>% 
    mutate(league_name = league_name) %>% ungroup() 
  
  return(league_df)
}



median_ages <- age_fun(df=epl_vals, league_name = "EPL") %>% 
  bind_rows(age_fun(df=championship_vals, league_name="Championship")) %>% 
  bind_rows(age_fun(df=league_one_vals, league_name = "League One")) %>% 
  bind_rows(age_fun(df=league_two_vals, league_name = "League Two"))

median_ages <- median_ages %>% 
  mutate(league_name = fct_relevel(league_name,
                                   "EPL", "Championship", "League One", "League Two"))




# Create Visualisation ----------------------------------------------------
# create a df for the outlier in the Championship
text_df <- championship_vals %>% 
  select(comp_name, player_name, player_age, player_market_value_euro) %>% 
  filter(player_age == 17, 
         player_market_value_euro > 3000000) %>% 
  mutate(plot_y_val = 4000000)


# create main plot:
a <- median_ages %>%
  ggplot(aes(x= player_age, y= median_val, group=1)) +
  geom_line(size=1, colour="steelblue") +
  geom_point(aes(x=player_age, y= median_val, size= n_players), alpha=0.5, colour="steelblue") +
  scale_size_continuous(guide = guide_legend()) +
  guides(size = guide_legend(title = "Num Players", label.hjust = 1)) +
  scale_y_continuous(labels = scales::label_dollar(prefix = "€")) +
  ggtitle("PLAYER VALUATION AGE CURVES FOR THE MAIN ENGLISH LEAGUES",
          subtitle = paste0("The most value in the EPL is in players aged 28 and under, while as the leagues go down levels, the\nmost valuable player ages progressively gets older. Note, 17 year old ", 
                            text_df$player_name, " (one of two\n17 year olds) in the ", text_df$comp_name, " is valued at ", scales::dollar(text_df$player_market_value_euro, prefix = "€"))) +
  labs(x= "Player Age", y= "Median Value",
       caption = "\nData: transfermarkt.com || Sourced using worldfootballR\nVisualisation: @jaseziv") +
  # geom_label(aes(x=player_age, y= median_val, label=n_players, vjust=0)) +
  facet_wrap(~ league_name, scales = "free_y") +
  # geom_text(data = text_df, aes(x= player_age, label = paste0(player_name, " - ", scales::dollar(player_market_value_euro, prefix = "€"))), y=4000000, hjust=0) +
  theme(plot.title.position = "plot",
        legend.text.align = 0.5)


# read in logos for plots and get ready for plotting
logo_file <- magick::image_read(here::here("img", "transfermarkt_logo.png"))
wf_logo <- magick::image_read(here::here("img", "logo.png"))

tm_logo <- logo_file %>%
  image_scale("400")

wf_logo <- wf_logo %>%
  image_scale("400")

tm_logo <- grid::rasterGrob(tm_logo, width = 0.4, height = 0.3)
wf_logo <- grid::rasterGrob(wf_logo, width = 0.4, height = 0.7)

# create a layout for where to put the logos
lay <- rbind(c(1, 1, 1, 1, 1, 1),
             c(1, 1, 1, 1, 1, 1),
             c(1, 1, 1, 1, 1, 1),
             c(1, 1, 1, 1, 1, 1),
             c(1, 1, 1, 1, 1, 1),
             c(1, 1, 1, 1, 1, 2),
             c(1, 1, 1, 1, 1, 3))

# plot to see how it looks before saving
gridExtra::grid.arrange(a, tm_logo, wf_logo, layout_matrix=lay)

# save the output visualisation
ggsave(here::here("published", "eng-leagues-age-valuations", "ENG_value-age-curves.png"), gridExtra::grid.arrange(a, tm_logo, wf_logo, layout_matrix=lay), width = 35, height = 28, units = "cm")

       