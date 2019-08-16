library(data.table)
library(ggplot2)
library(ggridges)

birds <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/bird_impacts.csv")

# look at data
summary(birds)

# exploring plot
ggplot(birds, aes(as.factor(incident_month))) + 
  geom_bar()

# species
species <- birds[, .(count = .N), keyby = .(species, incident_month)]
species[, total := sum(count), by = species]
top_birds <- species[frankv(-total, ties.method = "dense") < 15
                     ][!(species %like% "Unknown")
                       ][order(-total)
                         ][, bird := forcats::fct_reorder(species, total)
                           ][, prop := count / total, by = species
                             ][, incident_month := as.factor(incident_month)]

ggplot(top_birds, aes(bird, total, fill = incident_month)) + 
  geom_col() + 
  coord_flip()

ggplot(top_birds, aes(incident_month, bird, height = prop * 5, group = bird)) + 
  geom_ridgeline(scale = 1, fill = "#93d5dc", color = "#5cb3cc", alpha = 0.7) + 
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                              "Aug", "Sep", "Oct", "Nov", "Dec"),) + 
  scale_y_discrete(expand = c(0, 0.2)) + 
  geom_label(aes(0.9, bird, label = total), vjust = 0.5, hjust = 1, 
             color = "white", fill = "#93d5dc") + 
  hrbrthemes::theme_ipsum_rc(grid = "X") + 
  labs(x = "", y = "", 
       title = "Top 10 Birds Strike by Month", 
       subtitle = "Unknown birds not taken account of", 
       caption = "Source: FAA & Graphic: Juby")
