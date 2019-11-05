library(rvest)
library(tidyverse)

# song list from beatlesbible
bb_url <- "https://www.beatlesbible.com/songs/"

record_songs <- bb_url %>% 
  read_html() %>% 
  html_nodes(".post-content ul li a") %>% 
  html_text() %>% 
  as_tibble(.name_repair = ~ c("song"))

record_songs <- record_songs %>% 
  filter(song != "") %>% 
  mutate(song = if_else(str_detect(song, ", The$"), paste0("The ", gsub(", The$", "", song)), song))

# write_csv(record_songs, "../data/beatles_record_songs.csv")

# song list from wikipedia
wiki_url <- "https://en.wikipedia.org/wiki/List_of_songs_recorded_by_the_Beatles"

data <- wiki_url %>% 
  read_html() %>% 
  html_table(fill = TRUE)

main_songs <- data[[3]] %>% 
  as_tibble() %>% 
  select(-`Ref(s)`) %>% 
  rename(song = Song, release = `Core catalogue release`, songwriter = `Songwriter(s)`, 
         lead_vocal = `Lead vocal(s)[b]`, year_released = Year)

other_songs <- data[[5]] %>% 
  as_tibble() %>% 
  select(-c("Notes", "Ref.")) %>% 
  rename(song = Song, release = `Release(s)`, songwriter = `Songwriter(s)`, 
         lead_vocal = `Lead vocal(s)`, year_recorded = Yearrecorded, 
         year_released = Yearreleased)

original_songs <- bind_rows(main_songs, other_songs) %>% 
  mutate(song = gsub("\"(.+)\".*", "\\1", song)) %>% 
  filter(grepl("John Lennon", songwriter) | grepl("Paul McCartney", songwriter) | 
           grepl("George Harrison", songwriter) | grepl("Richard Starkey", songwriter)) %>% 
  filter(!grepl("German version", song)) %>% 
  select(-year_recorded) %>% 
  arrange(song) %>% 
  mutate(
    songwriter = case_when(
      song %in% c("Dig It", "Flying", "Maggie Mae") ~ "John Lennon/Paul McCartney/George Harrison/Richard Starkey", 
      song == "What Goes On" ~ "John Lennon/Paul McCartney/Richard Starkey", 
      songwriter == "John LennonPaul McCartney" ~ "John Lennon/Paul McCartney", 
      TRUE ~ songwriter), 
    lead_vocal = case_when(
      lead_vocal == "John LennonPaul McCartney" ~ "John Lennon/Paul McCartney", 
      lead_vocal == "John LennonPaul McCartneyGeorge Harrison" ~ "John Lennon/Paul McCartney/George Harrison", 
      lead_vocal == "Paul McCartneyJohn LennonGeorge Harrison" ~ "Paul McCartney/John Lennon/George Harrison", 
      song == "Carry That Weight" ~ "Paul McCartney(with John Lennon/George Harrison/Ringo Starr)", 
      song %in% c("Sun King", "This Boy", "Yes It Is") ~ "John Lennon(with Paul McCartney/George Harrison)", 
      TRUE ~ lead_vocal))

# write_csv(original_songs, "../data/beatles_original_songs.csv")