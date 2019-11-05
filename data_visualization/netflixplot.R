library(data.table)
library(maps)
library(ggplot2)

# data: https://docs.google.com/spreadsheets/d/17KyqNGvXRL4aIyXsciXJafD-vqLSDhJGtAYd_GJM-Go
netflix <- fread("~/downloads/netflix.csv")

# set col names and remove useless row/col
setnames(netflix, names(netflix), 
         c("country", "subscribers", "avg_month_revenue", "total_month_revenue", 
           "total_year_revenue", "NA", "source", "others"))

netflix <- netflix[-1, !"NA"]

# parse number
netflix <- netflix[, lapply(.SD, readr::parse_number), .SDcols = c(2:5), by = country]

# world map
world <- map_data("world")
setDT(world)

# netflix country
netflix[country == "United States", country := "USA"]
netflix[country == "United Kingdom", country := "UK"]
netflix_country <- world[region %in% netflix$country]
netflix_country[netflix, on = c(region = "country"), subscribers := i.subscribers]
usa <- netflix_country[region == "USA"]
netflix_country<- netflix_country[!(region == "USA")]

# plot
ggplot(world) + 
  geom_polygon(aes(long, lat, group = group), fill = "#7a7374") + 
  geom_polygon(aes(long, lat, group = group, fill = subscribers), 
               data = netflix_country) + 
  geom_polygon(aes(long, lat, group = group), data = usa, fill = "#82111f") + 
  scale_fill_gradient(low = "#f1c4cd", high = "#ee3f4d") + 
  hrbrthemes::theme_ft_rc(grid = "none") + 
  theme(axis.text = element_blank()) + 
  labs(title = " Netflix Subscribers", 
       x = "", y = "",
       caption = "Source: https://docs.google.com/spreadsheets/d/17KyqNGvXRL4aIyXsciXJafD-vqLSDhJGtAYd_GJM-Go")
