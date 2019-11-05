library(rvest)
library(tidyverse)
library(gganimate)
library(ggimage)

url <- "https://en.wikipedia.org/wiki/List_of_World_Series_champions"

data <- url %>% read_html() %>% 
  html_table(fill = TRUE)

champ <- data[[2]] %>% 
  janitor::clean_names() %>% 
  select(-ref) %>% 
  filter(!(year %in% c(1904, 1994, 2019))) %>% 
  separate(winning_team, into = c("winning_team", "attend_times", "win_times", "lose_times"), 
           sep = "[\\(,–\\)]") %>% 
  separate(games, into = c("win", "lose", "tie"), sep = "[–]") %>% 
  separate(losing_team, into = c("losing_team"), sep = "[\\(|\\[]") %>% 
  mutate(winning_team = trimws(gsub("(*)\\[(.|.\\d)\\]", "\\1", winning_team)),
         losing_team = trimws(losing_team), 
         lose = gsub("(\\d)\\[.\\]", "\\1", lose), 
         tie = if_else(year %in% c(1907, 1912, 1922), 1, 0),
         team_name = case_when(
           winning_team == "Boston Americans" ~ "Boston Red Sox", 
           winning_team == "Philadelphia Athletics" ~ "Oakland Athletics", 
           winning_team == "Boston Braves" | winning_team == "Milwaukee Braves" ~ "Atlanta Braves", 
           winning_team == "New York Giants" ~ "San Francisco Giants", 
           winning_team == "Washington Senators" ~ "Minnesota Twins", 
           winning_team == "Brooklyn Dodgers" ~ "Los Angeles Dodgers", 
           winning_team == "Florida Marlins" ~ "Miami Marlins", 
           winning_team == "Anaheim Angels" ~ "Los Angeles Angels", 
           TRUE ~ winning_team
         )) %>% 
  mutate_at(c("attend_times", "win_times", "lose_times", "win", "lose"), as.numeric) %>% 
  rename(winning_manager = manager, losing_manager = manager_2) %>% 
  select(1, 12, 2:11)

# write_csv(champ, "../data/wschampions.csv")

# team logo
teams <- sort(unique(data[[3]][-1,]$Team))
logo_png_url <- "https://i1.wp.com/sportleaguemaps.com/wp-content/uploads/2018/07/"
logo_svg_url <- "https://www.mlbstatic.com/team-logos/"

teamlogo <- tibble(team_name = teams, 
                    number = c(109, 144, 110, 111, 112, 145, 113, 114, 115, 116, 117, 
                               118, 108, 119, 146, 158, 142, 121, 147, 133, 143, 
                               134, 135, 137, 136, 138, 139, 140, 141, 120), 
                    team_logo_svg = paste0(logo_svg_url, number, ".svg"), 
                    team_logo_png = paste0(logo_png_url, gsub(" ", "-", team_name), ".png")) %>% 
  mutate(team_logo_png = if_else(team_name == "Miami Marlins", 
                             "https://i1.wp.com/sportleaguemaps.com/wp-content/uploads/Miami-Marlins.png", 
                             team_logo_png))
# write_csv(teamlogo, "../data/teamlogo.csv")

champ <- champ %>% left_join(teamlogo, by = "team_name")

# begin to plot
p <- ggplot(champ, aes(year, win_times)) + 
  geom_image(aes(image = team_logo_svg), size = 0.03) + 
  geom_vline(xintercept = 1903, color = "red", linetype = "dotted") + 
  geom_vline(xintercept = 1904, color = "blue", linetype = "dashed") + 
  geom_vline(xintercept = 1994, color = "blue", linetype = "dashed") + 
  hrbrthemes::theme_ipsum_rc() + 
  labs(title = "MLB WS Champions", x = "", y = "Titles", 
       caption = "Data: Wikipedia | Graphic: Juby")

p + annotate("text", x = 1915, y = 10, label = "1903, First played", color = "#006284") + 
  annotate("segment", x = 1903, xend = 1909, y = 8.5, yend = 10, color = "#006284") + 
  annotate("text", x = 1916, y = 20, label = "1904, No world series", color = "#006284") + 
  annotate("segment", x = 1904, xend = 1909, y = 18.5, yend = 20, color = "#006284") + 
  annotate("text", x = 2006, y = 15, label = "1994, No world series", color = "#006284") + 
  annotate("segment", x = 1994, xend = 1999, y = 13.5, yend = 15, color = "#006284")

p <- p + transition_reveal(year)

animate(p, fps = 8, width = 2000, height = 1000)
anim_save("tidydata/plot/wschampions.gif")