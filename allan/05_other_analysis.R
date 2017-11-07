
# load required data
load("allan/data/harrypotter_clean_tokens.Rda")
load("allan/data/harrypotter_characters.rda")

###Q6.How does sentiment changes from 1 chapter to the next for each book? Which book has the highest variation?
generate_q6_graph <- function() {
  hp_df <- harrypotter_clean_tokens %>%
    mutate(Title = gsub("Harry Potter and the ", "", Title)) %>% # shorten
    inner_join(get_sentiments("bing"), by = "word") %>% # join sentiment
    count(Title, Chapter, sentiment, sort = TRUE) %>% # count sentiment
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  
  ggplot(hp_df, aes(Chapter, sentiment, fill = Title)) +
    geom_line(size = 1, color = brewer.pal(3, "Set1")[3]) +
    facet_wrap(~ Title) +
    ggsave('allan/graph/hp_sentiment_changes.q6.png')
}

###Q7. Which is the most common word pairs (ngrams) in harry potter series? Analyze their network structure of word pairs
generate_q7_graph <- function() {
  # create word pairs: word1 and word2
  harrypotterSeries_ngrams <- harrypotterSeries %>%
    unnest_tokens(bigram, sentence, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
  bigram_counts <- harrypotterSeries_ngrams %>%
    count(word1, word2, sort = TRUE)
  
  # plot and save graph
  bigram_counts %>%
    filter(n > 80) %>%
    ggplot(aes(
      x = reorder(word1, -n),
      y = reorder(word2, -n),
      fill = n
    )) +
    geom_tile(alpha = 0.8, color = "white") +
    coord_flip() +
    theme(legend.position = "right") +
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    )) +
    labs(x = "first word in pair",
         y = "second word in pair") +
    ggsave('allan/graph/bigram_counts.q7.png')
  
  # plot network tructures
  bigram_graph <- bigram_counts %>%
    filter(n > 60) %>%
    graph_from_data_frame()
  # set seed
  set.seed(2017)
  # define arrow
  a <- grid::arrow(type = "closed", length = unit(.10, "inches"))
  
  # generate graph
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(
      aes(edge_alpha = n),
      show.legend = FALSE,
      arrow = a,
      end_cap = circle(.07, 'inches')
    ) +
    geom_node_point(color = "chartreuse", size = 5) +
    geom_node_text(aes(label = name), check_overlap = T, vjust = 1, hjust = 1) +
    theme_void()+
    ggsave("allan/graph/bigram_network_model.q7.png")

}

###Q8.How often did the writer negated clauses? Examine how often sentiment-associated words are preceded by “not” or other negating words.
generate_q8_graph <- function() {
  # create hp_big_grams object
  hp_bigrams_separated  <- harrypotterSeries %>%
    unnest_tokens(bigram, sentence, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    count(word1, word2, sort = TRUE)
  
  # define negative words
  negative_words <- c("not", "no", "never", "nor","without", "hardly")
  
  #create dataframe of negative pairs
  hp_negated_words  <- hp_bigrams_separated %>%
    # filter by negative words
    filter(word1 %in% negative_words) %>%
    inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
    count(word2, score, sort = TRUE) %>%
    ungroup()
  
  # generate graph
  hp_negated_words %>%
    mutate(contribution = nn * score) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, nn * score, fill = nn * score > 0)) +
    geom_col(show.legend = FALSE) +
    xlab("Words preceded by \"not\", \"no\" ") +
    ylab("Sentiment score * number of occurrences") +
    coord_flip()+
    ggsave('allan/graph/hp_negated_words.q8.png')
 
  
}


# generate all graphs
generate_q6_graph()
generate_q7_graph()
generate_q8_graph()
