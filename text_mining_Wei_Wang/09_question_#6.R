# Word cloud
series$book <- factor(series$book, levels = rev(hp_books))
series %>% 
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))