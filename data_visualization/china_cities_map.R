library(ggplot2)
library(gganimate)
library(data.table)
library(leaflet)

china <- geojsonio::geojson_read("china/china.json", what = "sp")
china_df <- broom::tidy(china)

cities <- read.csv("china/china_cities.csv")

p <- ggplot() + 
  geom_polygon(aes(long, lat, group = group), data = china_df, color = "#ffffff") + 
  geom_point(aes(long, lat), data = cities, color = "yellow", size = 0.5) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "#333333"), 
        title = element_text(size = 18, color = "white")) + 
  labs(title = "Cities of {closest_state}") + 
  transition_states(admin, state_length = 3, wrap = FALSE) + 
  shadow_mark()

animate(p)

jsonlite::write_json(cities, "cities.json")

setDT(cities)
jsc <- cities[admin == "Jiangsu"]

nj <- cities[city == "Nanjing"]


js <- geojsonio::geojson_read("china/geometryprovince/jiangsu.json", what = "sp")

leaflet(js) %>% 
  setView(nj$long, nj$lat, 8) %>% 
  addTiles() %>% 
  addPolygons() %>% 
  addMarkers(jsc$long, jsc$lat, label = jsc$city)

sd <- geojsonio::geojson_read("china/geometryprovince/shandong.json", what = "sp")

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = js) %>% 
  addPolygons(data = sd)
