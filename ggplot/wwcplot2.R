# https://github.com/gkaramanis/tidytuesday/tree/master/week-28

library(magrittr)
library(data.table)
library(ggplot2)
library(ggimage)

wwc_outcomes <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
codes <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

winloss <- wwc_outcomes %>% 
  .[, .(game_n = 1:.N, 
        win_i  = -2 + as.integer(factor(win_status))
        ), by = team] %>% 
  .[codes, on = "team", country := i.country]

ggplot(winloss) + 
  geom_tile(aes(25.5, 0), height = 3, width = 50, fill = "grey97") + 
  geom_tile(aes(25.5, 0), height = 1, width = 50, fill = "grey93") + 
  geom_tile(aes(game_n, win_i, alpha = win_i, fill = as.factor(win_i)), 
            color = "white") + 
  geom_text(aes(-1, 0, label = team), hjust = 1, size = 3, check_overlap = TRUE) + 
  facet_wrap(~ team, ncol = 2) + 
  scale_fill_manual(values = c("#8c5358", "#465675", "#27b376"), 
                    labels = c("loss", "draw", "win")) +
  scale_alpha_continuous(range = c(1, 1), guide = FALSE) + 
  coord_fixed(xlim = c(-10, 50)) + 
  theme_void() + 
  theme(strip.text = element_blank(), 
        legend.position = "top", 
        legend.title = element_blank())
