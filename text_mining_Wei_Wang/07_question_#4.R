
afinn <- series %>%
  group_by(book) %>% 
  mutate(word_count = 1:n(),
         index = word_count %/% 500 + 1) %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(book, index) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(series %>%
                            group_by(book) %>% 
                            mutate(word_count = 1:n(),
                                   index = word_count %/% 500 + 1) %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing"),
                          series %>%
                            group_by(book) %>% 
                            mutate(word_count = 1:n(),
                                   index = word_count %/% 500 + 1) %>%
                            inner_join(get_sentiments("nrc") %>%
                                         filter(sentiment %in% c("positive", "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(book, method, index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  select(book, index, method, sentiment)

bind_rows(afinn, bing_and_nrc) %>%
  ungroup() %>%
  mutate(book = factor(book, levels = hp_books)) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_bar(alpha = 0.65, stat = "identity", show.legend = FALSE) +
  facet_grid(book ~ method) +
  theme(legend.position = 'none', # get rid of legend
        text = element_text(size = 9), # determine fontsize
        axis.text.x = element_text(hjust = 1, size = 9))
