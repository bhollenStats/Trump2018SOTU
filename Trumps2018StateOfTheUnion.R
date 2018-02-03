# Text analysis of Trump's 2018 State of the Union address

library(rvest)
library(tidyr)
library(dplyr)
library(stringr)
library(tidytext)
library(wordcloud)

sotu_url <- 'https://www.politico.com/story/2018/01/30/trump-state-of-the-union-2018-transcript-full-text-379363'

webPage <- read_html(sotu_url)

results<-webPage %>% html_nodes('p')

str_results <- data_frame(as.character(results)) 
names(str_results) <- "trumpText"

cleansed_text <- str_results %>%
  filter(!str_detect(trumpText, "^<p class="),
         !str_detect(trumpText, "^<p style=")) %>%
  mutate(trumpText=str_replace_all(trumpText, regex('</[a-z]>|<[a-z]>|\n|<br>'), "")) %>%
  mutate(trumpText=str_replace_all(trumpText, regex('"'), '\''))

tidy_text <- cleansed_text %>%
  unnest_tokens(word, trumpText) %>%
  anti_join(stop_words)

tidy_text %>%
  group_by(word) %>%
  count(word) %>%
  filter(n>=5) %>%
  arrange(desc(n)) %>%
  with(wordcloud(word, n, min.freq=5, max.words = 55, colors="darkgreen"),random.order=FALSE, fixed.asp=FALSE)

