library(data.table)
library(lubridate)
library(ggplot2)
library(ggridges)

ufo <- fread("~/downloads/ufo2.csv")
str(ufo)

# do with date
ufo[, date := mdy_hm(date)
    ][, `:=`(sigh_year = year(date), 
             sigh_month = month(date), 
             sigh_date = format(date, "%m-%d"))]

sapply(ufo, function(x) sum(is.na(x)))

ufo2 <- ufo[!is.na(date) & sigh_year > 1994 & sigh_year != 2019
            ][, .(num = .N), by = .(sigh_year, sigh_date)
              ][, total := sum(num), by = sigh_year
                ][, prop := num / total * 100, by = sigh_year
                  ][, sigh_date := as.Date(sigh_date, "%m-%d")]

ggplot(ufo2, aes(sigh_date, sigh_year, height = prop, group = sigh_year)) + 
  geom_ridgeline(scale = 1, fill = '#25D366', color = '#128C7E', alpha = 0.8) + 
  scale_y_continuous(limits = c(1995, 2020), breaks = seq(1995, 2018, 1), 
                     expand = c(0, 0), position = "right") + 
  scale_x_date(date_breaks = "month", date_minor_breaks = "day", 
               date_labels = "%b", expand = c(0, 0)) + 
  # annotate("segment", x = as.Date("2019-10-20"), xend = as.Date("2019-11-07"), 
  #          y = 2018, yend = 2019, arrow = arrow(), color = "red", size = 1) + 
  # annotate("text", x = as.Date("2019-10-06"), y = 2018, 
  #          label = "most sighs", color = "red", size = 5) + 
  hrbrthemes::theme_ipsum_rc(grid = "X") + 
  theme(axis.text.x = element_text(hjust = 0)) + 
  labs(x = NULL, y = NULL, title = "Meet with UFO")

ufo2[which.max(num)]
