library(RSelenium)
library(rvest)

driver <- rsDriver(port = 1210L, browser = "firefox")
remDr <- driver$client

remDr$navigate("https://www.instagram.com/accounts/login")

username <- remDr$findElement(using = "xpath", "//input[@name='username']")
username$sendKeysToElement(list("username"))

pwd <- remDr$findElement(using = "xpath", "//input[@name='password']")
pwd$sendKeysToElement(list("password"))

login <- remDr$findElement(using = "xpath", "//button[@type='submit']")
login$clickElement()

remDr$navigate("https://www.instagram.com/elaiza_ikd/")

for (i in 1:10) {
  remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(5)
}

img_url <- remDr$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_nodes(".KL4Bh img") %>% 
  html_attr("src")

dir.create("ikeda")
mapply(function(x, y) download.file(x, y, mode = "wb"), img_url, 
       paste0("ikeda/ikeda_img_", 1:length(img_url), ".jpg"))

remDr$close()
driver$server$stop()
