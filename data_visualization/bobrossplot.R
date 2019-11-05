library(data.table)
library(ggplot2)
library(magrittr)

# import data
ross <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

# look at data
str(ross)

# clean col names
ross <- janitor::clean_names(ross)

# divide episode
ross[, c("season", "episode") := tstrsplit(episode, "E")]
setcolorder(ross, c(70, 1:69))
ross[, `:=`(season = grep("S(*)", season), 
            episode = as.integer(episode))]

# melt data
ross2 <- melt(ross, id.vars = c("season", "episode", "title"),
              variable.name = "element", value.name = "count")
