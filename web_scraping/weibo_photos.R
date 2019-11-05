library(RSelenium)
library(rvest)

driver <- rsDriver(port = 7895L, browser = "chrome", version = "3.141.59", chromever = "76.0.3809.126")
remDr <- driver$client

remDr$navigate("https://www.weibo.com/")

mail <- remDr$findElement(using = "css selector", "#loginname")
mail$sendKeysToElement(list("username"))

pwd <- remDr$findElement(using = "css selector", ".password .W_input")
pwd$sendKeysToElement(list("password"))

login <- remDr$findElement(using = "css selector", ".login_btn")
login$clickElement()

remDr$navigate("https://www.weibo.com/p/1005051574425704/photos")

for(i in 1:10) {
  remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(5)
}

img_url <- remDr$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_nodes(".photo_cont .photo_pict") %>% 
  html_attr("src")

for (i in 1:length(img_url)) {
  img_url[i] = lest::case_when(
    grepl("^//wxt", img_url[i]) ~ gsub("//wxt.sinaimg.cn/thumb300", "https://wx3.sinaimg.cn/large", img_url[i]), 
    grepl("^//wt", img_url[i]) ~ gsub("//wt.sinaimg.cn/thumb300", "https://ww3.sinaimg.cn/large", img_url[i]), 
    TRUE ~ img_url[i]
  )
}

mapply(function(x, y) download.file(x, y, mode = "wb"), img_url, 
       paste0("lmy/lmy_img_", 1:length(img_url), ".jpg"))
