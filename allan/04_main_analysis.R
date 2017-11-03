


# load required data
load("allan/data/harrypotter_clean_tokens.Rda")
load("allan/data/harrypotter_characters.rda")

###Q1. what is the most important charecter based on how much it was mentioned
generate_q1_graph <- function() {
  harrypotter_clean_tokens %>%
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
    ggsave('allan/graph/harrypotter_clean_tokens.q1.png')
}

###Q2. what is the most scariest book based on sentiment analysis ?
generate_q2_graph <- function() {
  hp_negative_sentiment <- harrypotter_clean_tokens %>%
    inner_join(get_sentiments("bing"), by = "word") %>% # join sentiment
    group_by(Title) %>% # make each row  abook
    count(sentiment, sort = TRUE) %>% # count sentiment
    filter(sentiment == "negative")  #filter by negative sentiment
  
  hp_negative_sentiment %>%
    ggplot(aes(Title, n, fill = Title)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "Level") +
    theme(legend.position = "none") +
    coord_flip() +
    ggsave('allan/graph/hp_negative_sentiment.q2.png')
}

###Q3. What the top ten used words in exception to stop words ?
generate_q3_graph <- function() {
  word_count <- harrypotter_clean_tokens %>%
    count(word, sort = TRUE) %>%
    mutate(word = reorder(word, n)) %>%
    filter(n > 600)
  
  # save for future use
  save(word_count, file = "allan/data/hp_word_count.q3.Rda")
  
  # plot using word cloud
  png("allan/graph/word_count.q3.png")
  wordcloud(
    words = word_count$word,
    freq = word_count$n,
    min.freq = 1,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
  )
  dev.off()
  
}

###Q4. sentiments by books
generate_q4_graph <- function() {
  hp_sentiment_by_book <- harrypotter_clean_tokens %>%
    # join nrc to get sentiment value
    inner_join(get_sentiments("nrc"), by = "word") %>%
    # join nrc to get sentiment score
    inner_join(get_sentiments("afinn"), by = "word") %>%
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
    ggsave('allan/graph/hp_sentiment_by_book.q4.png')
}

generate_q5_graph <- function() {
  ###Q5. sentiment by popularity based on www.theguardian.com
  hp_by_sales <- harrypotter_clean_tokens %>%
    # join nrc to get sentiment value
    inner_join(get_sentiments("nrc"), by = "word") %>%
    # join nrc to get sentiment score
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(Title) %>%
    summarise(count = sum(score)) %>%
    mutate(perc_sentiment = (count / sum(count)) * 2) %>%
    arrange(desc(perc_sentiment)) %>%
    inner_join(bookMetadata, by = "Title") %>%
    mutate(sales = as.numeric(gsub(",", "", Volume.Sales)))
  
  df <- data.frame(hp_by_sales)
  # plot the graph
  ggplot(df)  +
    geom_bar(aes(x = Title, y = sales,  fill = Title), stat = "identity") +
    geom_line(aes(x = Title, y = perc_sentiment * max(df$sales)), group =
                1) +
    geom_point(aes(
      label = perc_sentiment,
      x = Title,
      y = perc_sentiment * max(df$sales)
    ),
    colour = "brown") +
    geom_text(aes(
      label = sales,
      x = Title,
      y = 0.97 * sales
    ), colour = "black") +
    scale_y_continuous(sec.axis = sec_axis( ~ . / max(df$sales))) +
    labs(x = "Book", y = "Sales/Popularity") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    ggsave('allan/graph/hp_by_sales.q5.png')
  
}

# generate all graphs
generate_q1_graph()
generate_q2_graph()
generate_q3_graph()
generate_q4_graph()
generate_q5_graph()