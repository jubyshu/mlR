library(magrittr)
library(data.table)
library(ggplot2)

r4ds_members <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")
fwrite(r4ds_members, "~/downloads/r4ds.csv")

members <- r4ds_members[, date := as.Date(date)] %>% 
  .[, `:=`(total_memb_lag1 = shift(total_membership, type = "lag", n = 1), 
           daily_active_lag1 = shift(daily_active_members, type = "lag", n = 1))] %>% 
  .[, `:=`(total_memb_diff = total_membership - total_memb_lag1, 
           daily_active_diff = daily_active_members - daily_active_lag1)]

ggplot(members, aes(date)) + 
  geom_ribbon(aes(ymin = 0, ymax = total_memb_diff), 
              fill = "blue", alpha = 0.3) + 
  geom_ribbon(aes(ymin = 0, ymax = daily_active_diff), 
              fill = "red", alpha = 0.2) + 
  scale_x_date(date_breaks = "2 months")
  
ggplot(members) + 
  geom_line(aes(date, total_memb_diff), color = "blue") + 
  geom_line(aes(date, daily_active_diff), color = "red", alpha = 0.5) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b, %Y") + 
  scale_y_continuous(breaks = seq(-140, 190, 40)) + 
  annotate("text", x = as.Date("2017-09-05"), y = 180, 
           label = "Total Membership Change", hjust = 0, size = 2.5) + 
  annotate("text", x = as.Date("2019-02-10"), y = 140, 
           label = "Daily Active Membership Change", hjust = 0, size = 2.5) + 
  hrbrthemes::theme_ipsum_rc(grid = "y", axis_text_size = 8, caption_size = 8, 
                             plot_margin = margin(10, 10, 10, 10)) + 
  labs(x = "", y = "", 
       title = "Daily Changes of R4DS Membership", 
       caption = "Source: R4DS Slack | Graphic: @jubyshu")

ggsave("~/desktop/r4dsplot.png")
