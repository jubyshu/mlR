library(data.table)
library(ggplot2)
library(ggchicklet)
library(ggimage)

wwc_outcomes <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

unique(squads$country)

goals <- wwc_outcomes[, .(goal = sum(score)), by = .(year, team)]

# goals[, year := as.factor(year)
#       ][, total := sum(goal), by = year
#         ][, prop := goal / total * 100]
# 
# ggplot(goals, aes(year, prop, fill = team, group = year)) + 
#   geom_col() + 
#   coord_polar(theta = "y") + 
#   theme_bw()

top_team <- goals[, sum(goal), by = team][order(-V1)][1:10, team]
codes2 <- data.table(team = top_team, 
                    code = c("US", "DE", "NO", "SE", "BR", "CN", "GB", "JP", "AU", "CA"))

goals2 <- goals[team %in% top_team][, all_goal := sum(goal), by = team]
goals2[codes, on = "team", country := i.country]
goals2[, country := as.factor(country)
       ][, country2 := forcats::fct_reorder(country, all_goal)]
goals2[codes2, on = "team", code := i.code]

ggplot(goals2, aes(country2, goal, fill = year)) + 
  geom_chicklet(width = 0.5) + 
  coord_flip() + 
  geom_flag(aes(y = -6, image = code)) + 
  scale_y_continuous(breaks = seq(0, 140, 20), position = "right") + 
  scale_fill_brewer(palette = "Set3") + 
  hrbrthemes::theme_ft_rc(grid = "none", plot_margin = margin(10, 10, 10, 10)) + 
  theme(legend.position = c(0.75, 0.1), legend.direction = "horizontal") + 
  labs(x = "", fill = NULL, 
       title = "Top 10 Teams Scoring Most in WWC History", 
       caption = "Source: data.world & Graphic: @Juby")

ggsave(here::here("wwcgoal.png"))

# -------------- #
wwc_19 <- wwc_outcomes[year == "2019", .(scores = sum(score)), keyby = team]
wwc_19[codes, on = "team", country := i.country]
quarter_teams <- c("NOR", "ENG", "FRA", "USA", "ITA", "NED", "SWE", "GER")
wwc_quarter <- wwc_19[team %in% quarter_teams]

squads[is.na(caps), "caps"] <- 0
squads[is.na(goals), "goals"] <- 0
squads[country == "US", "country"] <- "United States"

squads_quarter <- squads[codes, on = "country", team := i.team
                         ][team %in% quarter_teams, 
                           .(avg_age = round(mean(age), 2), 
                             avg_caps = round(mean(caps), 2), 
                             avg_goals = round(mean(goals), 2)), by = team]
wwc <- squads_quarter[wwc_quarter, on = "team"
                      ][, team := NULL]

wwc <- melt(wwc, id.vars = "country")

ggplot(wwc, aes(variable, value, group = country, color = country)) + 
  geom_point() + 
  geom_line() + 
  coord_polar()
