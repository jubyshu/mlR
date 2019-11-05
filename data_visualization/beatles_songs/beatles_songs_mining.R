library(tidyverse)
library(genius)
library(tidytext)
library(wordcloud2)

beatles_songs <- read_csv("../data/beatles_original_songs.csv")

# get lyrics from genius.com
safe_genius_lyrics <- safely(genius_lyrics)

lyrics <- map(beatles_songs$song, 
              ~ safe_genius_lyrics("The Beatles", ., "title")) %>% 
  map("result")

names(lyrics) <- beatles_songs$song

beatles_lyrics <- bind_rows(lyrics)

# write_csv(beatles_lyrics, "tidydata/data/beatles_lyrics.csv")
beatles_lyrics <- read_csv("../data/beatles_lyrics.csv")

# bind songs and lyric
origin_lyrics <- beatles_songs %>% 
  left_join(beatles_lyrics, by = c(song = "track_title")) %>% 
  filter(!is.na(lyric)) %>% 
  group_by(song) %>% 
  mutate(line = row_number(song)) %>% 
  ungroup()

# word analyse
tidy_lyrics <- origin_lyrics %>% 
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words)

# romove some meaningless words
new_stop_words <- c("na", "yeah", "9", "tit", "da", "hey", "gonna", 
                    "ah", "ooh", "la", "whoa", "ha")

tidy_lyrics <- tidy_lyrics %>% 
  filter(!(word %in% new_stop_words))

# plot word cloud
tidy_lyrics %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  filter(n > 20) %>% 
  wordcloud2(shape = "diamonds", fontFamily = "PT Serif",)

# sentiment analyse
lyrics_afinn <- origin_lyrics %>% 
  unnest_tokens(word, lyric) %>%  
  inner_join(get_sentiments("afinn")) %>% 
  mutate(sentiment = if_else(value > 0, "positive", "negative"))

lyrics_afinn %>% 
  group_by(year_released, sentiment) %>% 
  summarise(score = mean(value)) %>% 
  ggplot() + 
  geom_col(aes(year_released, score, fill = sentiment), alpha = 0.6, width = 0.3) + 
  geom_line(aes(year_released, total), color = "#FFB11B", 
            data = lyrics_afinn %>% 
              group_by(year_released) %>% 
              summarise(total = mean(value))) + 
  scale_x_continuous(breaks = seq(1962, 1970, 1)) + 
  labs(x = "", y = "sentiment", fill = "", 
       title = "Sentiment in Lyrics of The Beatles") + 
  hrbrthemes::theme_ipsum_rc(base_family = "PT Serif", grid = "Y") + 
  theme(legend.position = "top")