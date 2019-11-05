# key: vE84o68ON5T8RkwrtIBoA
# secret: b3pDykCyjC8QpQs4vKaGW6oFJooSeTMfojePzIErL6c
library(tidyverse)
library(rvest)

# Best Books of the 20th Century
url <- "https://www.goodreads.com/list/show/6.Best_Books_of_the_20th_Century?page="

link <- map2_chr(url, 1:78, paste0)

# book title
book_title <- link %>% 
  map(
    . %>% 
    read_html() %>% 
    html_nodes(".bookTitle span") %>% 
    html_text(trim = TRUE)
  ) %>% 
  unlist()

# book author
book_author <- link %>% 
  map(
    . %>% 
      read_html() %>% 
      html_nodes(".authorName span") %>% 
      html_text(trim = TRUE)
  ) %>% 
  unlist()

# book rating
book_rating <- link %>% 
  map(
    . %>% 
      read_html() %>% 
      html_nodes(".minirating") %>% 
      html_text(trim = TRUE)
  ) %>% 
  unlist()

# book score & people voted
score_voted <- link %>% 
  map(
    . %>% 
      read_html() %>% 
      html_nodes(".smallText.uitext a") %>% 
      html_text(trim = TRUE)
  ) %>% 
  unlist()

# get book socore
book_score <- score_voted[c(TRUE, FALSE)]
# get people voted
people_voted <- score_voted[c(FALSE, TRUE)]

best_book_20th <- tibble(title = book_title, author = book_author, rating = book_rating, 
                         score = book_score, voted = people_voted)

# tide data
best_book_20th <- best_book_20th %>% 
  separate(rating, into = c("rating", "rated"), sep = "[-–—]") %>% 
  mutate_at(vars("rating", "rated", "score", "voted"), parse_number) %>% 
  mutate(author = as.factor(author))
# import data
write_csv(best_book_20th, "best_book_20th_century_goodreads.csv")
