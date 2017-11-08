
# PLOT WORD FREQUENCY PER BOOK
series %>%
  group_by(book, word) %>%
  anti_join(stop_words, by = "word") %>% # delete stopwords
  count() %>% # summarize count per word per book
  arrange(desc(n)) %>% # highest freq on top
  group_by(book) %>% # 
  mutate(top = seq_along(word)) %>% # identify rank within group
  filter(top <= 16) %>% # retain top 15 frequent words
  # create barplot
  ggplot(aes(x = -top, fill = book)) + 
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure words are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = word), size = 15/3.5, hjust = "left") +
  theme(legend.position = 'none', # get rid of legend
        text = element_text(size = 8), # determine fontsize
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15/1.5), # rotate x text
        axis.ticks.y = element_blank(), # remove y ticks
        axis.text.y = element_blank()) + # remove y text
  labs(y = "Word count", x = "", # add labels
       title = "Harry Plotter: Most frequent words throughout the saga") +
  facet_grid(. ~ book) + # separate plot for each book
  coord_flip() # flip axes

## As we can imagine, Harry is the most common word in every single book and Ron and Hermione are also present. 
## So Harry is the most important character based on how much it was mentioned. 