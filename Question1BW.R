library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(RColorBrewer)
library(harrypotter)
library(tidyr)
library(RColorBrewer)
library(wordcloud)
library(rebus)
library(textreadr)

# extract text from Harry Potter (character)'s summary
hp_wikipedia_sum = read_docx('HarryPotter-Summary.docx')

#set the summary into a dataframe and name a title called hp.sum
hp.sum <- data.frame(line = 1:109, text = hp_wikipedia_sum)

#Restructure it in the one-token-per-row format
sum_tokenized <- hp.sum %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(line = row_number()) # add sentences

sum.word.list <- sum_tokenized %>%
  unnest_tokens(word, sentence)

#define custom stop word used in data cleaning
custom_stop_words <-  data.frame(line = 1:9, word = c('Wikipedia','Wikimedia','The','Puppet','rowling','Stories','harry','potter',"harry's"))

#exclude custom stop word and stop words
tidylist <- sum.word.list %>% 
  anti_join(custom_stop_words, by = "word") %>% 
  anti_join(stop_words)

#count words
word_count <- tidylist %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n))
head(word_count)
# Visualization of the most common words
word_count_gt_10 <- word_count %>%
  filter(n > 10)



# plot using word cloud

wordcloud(words = word_count_gt_10$word, freq = word_count_gt_10$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))