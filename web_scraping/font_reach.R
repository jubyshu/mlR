library(rvest)
library(RSelenium)
library(ggplot2)

driver <- rsDriver(port = 1210L, browser = "firefox")
remdr <- driver$client

remdr$navigate("http://www.fontreach.com/#top")

font_name <-  remdr$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_nodes(".stagger li .domain") %>% 
  html_text(trim = TRUE)

site_count <- remdr$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_nodes(".stagger li span") %>% 
  html_text(trim = TRUE)

pop_fonts <- data.table(font = font_name, site = site_count)

pop_fonts[, site := as.numeric(gsub(",", "", site))]

pop_fonts<- pop_fonts[!font %in% c("serif", "sans-serif", "Monospace", "Font Awesome")]

pop_fonts[, font := reorder(font, site)]

ggplot(pop_fonts[1:15] ) + 
  geom_col(aes(font, site)) + 
  theme(axis.text.x = element_text(angle = -15)) + 
  geom_text(aes(font, site, label = site), vjust = 0)
