# load required data
load("TT/data/harrypotter_token_clean.Rda")
load("TT/data/harrypotter_characters.rda")
load("TT/data/seriesinfo.rda")

###Q1. who is the most important charecter based on how much they were mentioned
q1 <- function() {
  harrypotter_token_clean %>%
    mutate(id = R.utils::capitalize(word)) %>%
    inner_join(harrypotter_characters, by = c(id = "FirstName")) %>%
    count(FullName, sort = TRUE)  %>%
    top_n(20) %>%
    ggplot() +
    geom_bar(
      mapping = aes(
        x = reorder(FullName, n),
        fill = FullName,
        y = n
      ),
      alpha = 0.8,
      stat = "identity"
    ) +
    labs(x = NULL, y = "Count") +
    coord_flip() + ggtitle("Harry Potter Top 20 Characters") +
    theme(legend.position = "none") +
    ggsave('TT/graph/q1.png')
}

###Q2. what is the most scariest book based on sentiment analysis ?
q2 <- function() {
  hp_negative_sentiment <- harrypotter_token_clean %>%
    inner_join(get_sentiments("bing"), by = "word") %>% # join sentiment
    mutate(Title = gsub("Harry Potter and the ", "", Title)) %>% # shorten
    group_by(Title) %>% # make each row  abook
    count(sentiment, sort = TRUE) %>% # count sentiment
    filter(sentiment == "negative")  #filter by negative sentiment
  
  hp_negative_sentiment %>%
    ggplot(aes(Title, n, fill = Title)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "Level") +
    theme(legend.position = "none",
          axis.text=element_text(size=4),
          axis.title=element_text(size=5,face="bold")) +
    coord_flip() +
    ggsave('TT/graph/q2.png')
}

###Q3. What the top ten used words in exception to stop words ?
q3 <- function() {
  word_count <- harrypotter_token_clean %>%
    count(word, sort = TRUE) %>%
    mutate(word = reorder(word, n)) %>%
    filter(n > 600)
  
  # save for future use
  save(word_count, file = "TT/data/q3.Rda")
  
  # plot using word cloud
  png("TT/graph/q3.png",  width=12,height=8, units='in', res=300)
  wordcloud(
    words = word_count$word,
    freq = word_count$n,
    min.freq = 1,
    random.order = FALSE,
    rot.per = 0.35,
    scale=c(8,.2),
    colors = brewer.pal(8, "Dark2")
  )
  dev.off()
  
}

###Q4. Analyze sentiments by books

q4 <- function() {
  hp_sentiment_by_book <- harrypotter_token_clean %>%
    # join nrc to get sentiment value
    inner_join(get_sentiments("nrc"), by = "word") %>%
    # join nrc to get sentiment score
    inner_join(get_sentiments("afinn"), by = "word") %>%
    mutate(Title = gsub("Harry Potter and the ", "", Title)) %>% # shorten
    group_by(Title, sentiment) %>%
    summarise(count = sum(score)) %>%
    arrange(desc(count))
  
  hp_sentiment_by_book %>%
    # get top 15 only
    top_n(15) %>%
    ungroup %>%
    # plot the graph
    ggplot(aes(sentiment, count, fill = Title)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "Score") +
    facet_wrap( ~ Title) +
    coord_flip() +
    theme(axis.text=element_text(size=4),
          axis.title=element_text(size=5,face="bold"))+
    ggsave('TT/graph/q4.png')
}

#5. How does negative sentiments changes over time? Is there any similarities in the 1st and last chapter?

q5<-function(){
  hp_sentiment_difference<- harrypotter_token_clean%>%
    inner_join(get_sentiments("bing"), by = "word") %>% # join sentiment
    count(Title, Chapter, sentiment, sort = TRUE)%>% # count sentiment
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  
  ggplot(hp_sentiment_difference, aes(Chapter, sentiment, fill = Title))+
    geom_line(size = 1, color = brewer.pal(3, "Set1")[3]) +
    facet_wrap(~Title, ncol = 2, scales = "free_x")
  
  ggplot(hp_sentiment_difference, aes(Chapter, sentiment, colour = Title))+
    geom_line()+
    theme(
      legend.position ="bottom")
ggsave('TT/graph/q5.png')
}

###Q6.How does sentiment changes from one chapter to the next for each book? Which book has the highest variation?
q6 <- function() {
  hp_df <- harrypotter_token_clean %>%
    mutate(Title = gsub("Harry Potter and the ", "", Title)) %>% # shorten
    inner_join(get_sentiments("bing"), by = "word") %>% # join sentiment
    count(Title, Chapter, sentiment, sort = TRUE) %>% # count sentiment
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  
  ggplot(hp_df, aes(Chapter, sentiment, fill = Title)) +
    geom_line(size = 1, color = brewer.pal(3, "Set1")[3]) +
    facet_wrap(~ Title) +
    ggsave('TT/graph/q6.png')
}

#7.What is the frequency distribution of words for each Harry Potter Book
q7 <- function(){
  words <- harrypotter_token_clean %>%
    count(Title, word, sort = TRUE) %>%
    ungroup()
  
  total_words <- words %>% 
    group_by(Title) %>% 
    summarize(total = sum(n))
  
  words <- left_join(words, total_words)
  
  head(words)
  ggplot(words, aes(n/total, fill = Title)) +
    geom_histogram(show.legend = FALSE) +
    xlim(NA, 0.0009) +
    facet_wrap(~Title, ncol = 2, scales = "free_y")
  
  # These plots exhibit similar distributions for all the books, with most of the words occuring rarely and fewer words that occur frequently.
  ggsave('TT/graph/q7.png')
}

#8. How does positive sentiment change over time. Does the author has a specific pattern of control of positive sentiment?
q8<-function(){
  # using nrc and afinn to get both sentiment and score
  hp_sentiment <- harrypotter_token_clean%>%
    inner_join(get_sentiments("nrc"), by = "word") %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(Title, Chapter,sentiment)%>%
    summarise(count=sum(score))
  hp_sentiment%>%
    filter(count>0)%>%
    ggplot( aes(x = Chapter, y = count)) + 
    geom_line(size = 1, color = brewer.pal(3, "Set1")[3]) +
    facet_grid(sentiment~.) +
    uniform_ggplot() + 
    theme(
      axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0),
      legend.position = "none") +
    labs(x = "Chapter", y = "Sentiment Value",
         title = "Sentiments Progression (Positive sentiments)")
ggsave('TT/graph/q8.png')

}
#9 How frequently did  the top 8 most popular characters appear in the last book
q9<-function(){
  top_characters <- harrypotter_token_clean %>%
    mutate(id = R.utils::capitalize(word)) %>%
    inner_join(harrypotter_characters, by =c(id = "FirstName") ) %>%
    count(FullName,word,sort= TRUE) %>%
    top_n(9)
  
  
  harrypotter_token_clean %>%
    # get the last book of HP
    filter(Title=='Harry Potter and the Deathly Hallows')%>%
    mutate(id = R.utils::capitalize(word)) %>% # create uniq id 
    # get all characters by first name
    inner_join(harrypotter_characters, by = c(id = "FirstName")) %>%
    count( Chapter, FullName,word, sort = TRUE) %>%
    filter(word %in% as.vector(top_characters$word))%>% 
    #plot the graph
    ggplot(aes(Chapter, n)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ FullName, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format()) +
    ylab("% frequency of Character")
  ggsave('TT/graph/q9.png')
  
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