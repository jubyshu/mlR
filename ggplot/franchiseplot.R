library(data.table)
library(ggplot2)

media <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

str(media)

top_franchise <- media[, sum(revenue), by = franchise
                       ][order(-V1)][1:10, franchise]

top_media <- media[franchise %in% top_franchise
                   ][, revenue_total := sum(revenue), by = franchise
                     ][, `:=`(franchise = as.factor(franchise), 
                              franchise2 = forcats::fct_reorder(franchise, revenue_total))]

ggplot(top_media, aes(franchise2, revenue, fill = revenue_category)) + 
  geom_col() + 
  coord_flip() + 
  scale_y_continuous(labels = c("0", "25b", "50b", "75b", "100b", "125b")) + 
  labs(title = "Top 10 Franchise Revenue", 
       y = "Revenue", 
       caption = "Source: TidyTuesday") + 
  theme(plot.background = element_rect(fill = "black"), 
        panel.background = element_rect(fill = "black"), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text = element_text(color = "white", size = 13), 
        legend.background = element_rect(fill = "black"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        text = element_text(color = "white"), 
        plot.title = element_text(size = 18))

star_wars <- top_media[franchise == "Star Wars"]

ggplot(star_wars, aes(franchise, revenue, fill = revenue_category)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y")
