library(data.table)
library(lubridate)
library(ggplot2)
library(ggridges)

# ufo <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")
# fwrite(ufo, "~/downloads/ufo.csv")
ufo <- fread("~/downloads/ufo.csv")

ufo <- ufo[, date_time := date(mdy_hm(date_time))
           ][, `:=`(year = year(date_time), 
                    month = month(date_time), 
                    sigh_date = format(date_time, "%m-%d"))
             ][year >= 1995]

ufo2 <- ufo[, .(count = .N), by = sigh_date][order(sigh_date)]
ufo2[ufo, on = "sigh_date", month := i.month]
ufo2[, date := as.Date(sigh_date, "%m-%d")]

# July 4
ufo[sigh_date == "07-04", unique(count)]

ggplot(ufo2, aes(date, count)) + 
  geom_ribbon(aes(x = date, ymax = count), ymin = 0, fill = "#6497b1", color = "#005b96") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0), date_minor_breaks = "day") + 
  scale_y_continuous(expand = c(0, 0), position = "right", limits = c(0, 1200)) + 
  annotate("text", x = as.Date("2019-01-01"), y = 600, 
           label = "Janurary 1st", hjust = 0) + 
  annotate("text", x = as.Date("2019-07-04"), y = 1120, 
           label = "July 4th", hjust = 0) + 
  theme(axis.text.x = element_text(hjust = 0, size = 12), 
        panel.background = element_rect(color = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())

