
used_words <- series %>%
  group_by(word) %>%
  anti_join(stop_words, by = "word") %>% # delete stopwords
  count() # summarize count per word per book
# Plot the top ten used words in exception to stop words
words_freq <- as.data.frame(used_words)   
ggplot(subset(words_freq, n>1600), aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y = "Word count", x = "", # add labels
       title = "Harry Plotter: Top ten used words in exception to stop words")