# load required data
load("Yimo/data/whole_series.Rda")
load("Yimo/data/name.rda")
load("Yimo/data/bookMetadata.rda")
load("Yimo/data/plot_theme.rda")
load("Yimo/data/harry_gram.rda")


###1. who is the most important charecter based on how often it was mentioned ?

#Count the frequency of character names and select ten of them.
q1 <- function() {
#Select the top 10 most frequently mentioned characters.  
name_freq = whole_series%>%
  anti_join(stop_words)%>%
  inner_join(name, by = c(word = "lower"))%>%
  count(word)%>%
  top_n(10)%>%
  ungroup()%>%
  mutate(word = reorder(str_to_title(word), n))

ggplot(name_freq, aes(x = word, y = n, fill = n))+
  labs(x = "Charcter Name", y = "Frequency")+
  geom_col(show.legend = FALSE) +
  ggtitle("10 Most Frequently Mentioned Names")+
  coord_flip()+
  plot_theme+
  ggsave('Yimo/graph/q1.png')
}


###2. What's the difference between the frequency of the name "Ron" and "Hermione" through chapters
q2 <- function() {
  
#Calculate the difference of frequency of Ron and Hermione
ron_minus_herm = whole_series%>%
  group_by(book, chapter)%>%
  summarise(ron = sum(word == "ron"), hermione = sum(word == "hermione"))%>%
  replace_na(list(ron = 0, hermione = 0))%>% #Replace NA with 0
  mutate(dif = ron - hermione, signal = dif/abs(dif))


ggplot(ron_minus_herm, aes(as.integer(chapter), dif, fill = signal))+
  geom_col(show.legend = F)+
  plot_theme+
  facet_wrap(~book, scales = "free_x")+
  labs(x = "Chapter", y = "Difference")+
  ggtitle(("The Difference of Frequency Between Ron and Hermione"), subtitle = ("Ron Minus Hermione"))+
  plot_theme+
  ggsave("Yimo/graph/q2.png")

}


###3. what the top ten used words in exception to stop words ?
q3 <- function() {
#Remove stop words
word_freq = whole_series%>%
  anti_join(stop_words)%>%
  count(word)



#Plot with wordcloud
png("Yimo/graph/q3.png")
wordcloud(words = word_freq$word, freq = word_freq$n, min.freq = 5, max.words = 50, random.order = F, colors=brewer.pal(12, "Paired"), rot.per = 0.2)
dev.off()
}








###4.what is the most scariest book based on sentiment analysis ?
q4 <- function() {
#Load nrc
emotion = get_sentiments("nrc")

#Count the word
total = whole_series%>%
  count(book)%>%
  rename(total_word = n)

#Rank the book based the sentiment "fear"
scare_emotion = whole_series%>%
  left_join(total, by = "book")%>%
  inner_join(emotion, by = "word")%>%
  count(book, sentiment, total_word)%>%
  ungroup()%>%
  mutate(percent = n/total_word)%>%
  filter(sentiment == "fear")%>%
  arrange(desc(percent))


ggplot(scare_emotion,aes(x = book, y = percent, fill = book))+
  geom_col()+
  labs(x = NULL, y = "Percent")+
  ggtitle("The Percentage of Words Related to Fear")+
  scale_y_continuous(labels = percent_format())+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_discrete(name = "Book")+
  plot_theme+
  ggsave('Yimo/graph/q4.png')
}

###5. Sentiment by book
q5 <- function() {
#load afinn
score = get_sentiments("afinn")
#load nrc
emotion = get_sentiments("nrc")

#Calculate the sentiment score
harry_sentiment = whole_series%>%
  inner_join(emotion, by = "word")%>%
  inner_join(score, by = "word")%>%
  group_by(book, sentiment)%>%
  mutate(score = sum(score), signal = as.character(abs(score)/score+1), total = n())%>% #signal measure whether the score is positive or negative
  ungroup()%>%
  mutate(sentiment = reorder(sentiment, total))

harry_sentiment$signal <- factor(harry_sentiment$signal, levels=c("0", "2"), labels=c("Score<0","Score>0"))#reset the name displayed in the legend

ggplot(harry_sentiment, aes(sentiment, score, fill = signal))+
  geom_col()+
  facet_wrap(~book, ncol = 3, scales = "free_y")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  ggtitle("Sentiment by Book")+
  scale_fill_discrete(name = "Sentiment")+
  scale_color_manual(labels = c("Score<0", "Score>0"), values = c("indianred4", "turquoise4"))+
  labs(y = "Count")+
  plot_theme+
  theme(panel.spacing = unit(0.3, "lines"))+
  scale_linetype_discrete("Model 1") +
  scale_shape_discrete("Model 1") +
  scale_colour_discrete("Model 1")+
  coord_flip()+
  ggsave('Yimo/graph/q5.png')

}

##6. The change of "anger" through chapters
q6 = function(){
  #The total words of each book
  total = whole_series%>%
    count(book)%>%
    rename(total_word = n)
  
  #load nrc
  emotion = get_sentiments("nrc")
  
  #count the percent of words related to anger
anger = whole_series%>%
  inner_join(total)%>%
  inner_join(emotion, by = "word")%>%
  filter(sentiment == "anger")%>%
  count(book, chapter, sentiment, total_word)%>%
  mutate(percent = n/total_word)%>%
  ungroup()

ggplot(anger, aes(x = chapter, y = percent, color = book))+ 
  geom_point(show.legend = F)+
  geom_line(size = 0.7, show.legend = F)+
  scale_y_log10(labels = percent_format())+
  facet_wrap(~book, scales = "free_x")+
  labs(x = "Chapter", y = "Percent")+
  ggtitle("The Change of Words Related to Anger Through Chapters")+
  plot_theme+
  ggsave('Yimo/graph/q6.png')
}


##7. How does sentiment change through chapter?
q7 = function(){
#Load afinn
score = get_sentiments("afinn")

#Calculate sentiment score based on book and chapter
emotion_in_chap = whole_series%>%
  count(book, chapter, word)%>%
  inner_join(score, by = "word")%>%
  group_by(book, chapter)%>%
  summarise(contribution = sum(score*n)/sum(n))


ggplot(emotion_in_chap, aes(x = chapter, y = contribution, color = book))+ 
  geom_col(show.legend = F)+
  facet_wrap(~book,  scales = "free_x")+
  labs(x = "Chapter", y = "Contribution")+
  ggtitle("The Change of Sentiment Through Chapters", subtitle = "Based on Sentiment Score")+
  plot_theme+
  ggsave('Yimo/graph/q7.png')
}




##8. Sentiment by popularity
q8 = function(){
  #load afinn
score = get_sentiments("afinn")

#Get sales and sentiment score of each book
sentiment_by_popularity = whole_series%>%
  count(book, chapter, word)%>%
  inner_join(score, by = "word")%>%
  inner_join(bookMetadata, by = c(book = "Title"))%>%
  rename(sales = Volume.Sales)%>%
  group_by(book, sales)%>%
  mutate(contribution = sum(n*score))%>%
  select(book, sales, contribution)%>%
  ungroup()%>%
  unique()

ggplot(sentiment_by_popularity)+
  geom_bar( aes(x = book, y = sales, fill = contribution), stat = "identity")+
  scale_x_discrete(limits = rev(sentiment_by_popularity$book))+
  labs(x = "Book", y = "Sales")+
  scale_fill_continuous(name = "Sentiment Contribution")+
  coord_flip()+
  plot_theme+
  theme(legend.position = c("right"))+
  ggsave('Yimo/graph/q8.png')
}


###9. Term frequency
q9 = function(){
count_word = whole_series%>%
  count(book, word, sort = T)%>%
  ungroup()

total_words = count_word%>%
  group_by(book)%>%
  summarize(total = sum(n))

freq_term = count_word%>%
  left_join(total_words)



ggplot(freq_term, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 3, scales = "free_y")+
  xlab("Frequency")+
  ylab("Number of Words")+
  scale_x_log10(labels = percent_format())+
  ggtitle("Term Frequency Analysis")+
  plot_theme+
  ggsave('Yimo/graph/q9.png')
}




##10. Zipf's Law
q10 = function(){
  count_word = whole_series%>%
    count(book, word, sort = T)%>%
    ungroup()
  
  total_words = count_word%>%
    group_by(book)%>%
    summarize(total = sum(n))
  
  freq_term = count_word%>%
    left_join(total_words)
  
freq_by_rank <- freq_term %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         term_frequency= n/total)

head(freq_by_rank)


rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm = lm(log10(term_frequency) ~ log10(rank), data = rank_subset)
coeff = lm$coefficients


ggplot(freq_by_rank, aes(rank, term_frequency, color = book)) + 
  geom_abline(intercept = coeff[1], slope = coeff[2], color = "gray50", linetype = 2) +
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()+
  labs(x = "Rank", y = "Term Frequency")+
  plot_theme+
  ggsave('Yimo/graph/q10.png')

}


##4.3 N-grams
q11 = function(){
library(harrypotter)

bigrams_separated <- harry_gram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)


bigram_graph <- bigram_counts %>%
  filter(n > 70) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n),edge_colour = "darkred") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), point.padding = unit(0.2, "lines"), repel = T) +
  theme_void()+
  ggsave('Yimo/graph/q11.png')


}

# generate all graphs
q1()
q2()
q3()
q4()
q5()
q6()
q7()
q8()
q9()
q10()
q11()

