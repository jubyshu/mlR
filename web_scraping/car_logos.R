library(rvest)
library(data.table)

base_url <- "https://www.carlogos.org/Car-Logos/"

urls <- paste0(base_url, "list_1_", 1:7, ".html")

# cars
cars <- urls %>% 
  lapply(. %>% 
    read_html() %>% 
    html_nodes(".logo1 dd a") %>% 
    html_text(trim = TRUE)
  ) %>% 
  unlist()
cars <- cars[c(FALSE, TRUE)]

# logos
logos <- urls %>% 
  lapply(. %>% 
    read_html() %>% 
    html_nodes(".logo1 img") %>% 
    html_attr("src")
  ) %>% 
  unlist()

# text
type <- urls %>% 
  lapply(. %>% 
    read_html() %>% 
    html_nodes(".logo1 dd") %>% 
    html_text(trim = TRUE)
  ) %>% 
  unlist()

car_logos <- data.table(car = cars, type = type, logo = logos)

car_logos[, type := mapply(function(x, y) gsub(x, "", y), car, type)]

car_logos[, year := regmatches(type, gregexpr("[0-9]+(-|–).+|Unknown", type))]

car_logos[, type := gsub("[0-9]+(-|–).+|Unknown", "", type)]

car_logos[car == "33 Logos", `:=`(car = "Volvo", type = "Luxury Vehicles", year = "1927-Present")]


# upload to google sheets
library(googlesheets)

icon_logo <- tempfile(fileext = ".csv")
fwrite(car_logos, icon_logo)

gs_upload(icon_logo, "car_logos")

remove(icon_logo)
