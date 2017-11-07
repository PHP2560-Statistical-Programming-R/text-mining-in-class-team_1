# load needed data 
load("Nour/data/characters_listjoin.Rda")
load("Nour/data/harrypoterclean.Rda")


##Q1 what is the most important charecters based on how much is whas mentioned aside from harry ? 
graphQ1 <- function(){
harrypoterclean %>%
  inner_join( characters_listjoin, by = "word" ) %>%
  count(Book, word,sort= TRUE) %>%
  top_n(50) %>%
  filter( word != "harry")%>%
  ggplot(aes(word, n, fill = word)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Book) +
  coord_flip() +
  theme_minimal() + # start with a minimal them and add what we need 
  theme( text = element_text(color = "gray20"),
         axis.text = element_text(face = "italic", size = 6), 
         axis.line = element_line(color = "gray50", size = 0.5), 
         axis.line.y = element_blank(), 
         panel.grid.major = element_line(color = "gray80", size = 0.5),
         panel.grid.major.y = element_blank(),
         panel.grid.minor = element_line(color = "gray10", size = 0.5)
  )
ggsave("Nour/graph/q1.png")
}

# not that joining and counting was done using first name only wich is not accurate. due to inability to use full name because  full names not always mentiond. also family name cannot be used because it is shared between many characters. 

##Q2 what is the most scariest book based on sentiment analysis ?
graphQ2 <- function(){
harrypoterclean %>%
  count(Book) %>%
  rename(total_word = n) %>%
  ungroup() %>%
  left_join(harrypoterclean, "Book") %>%
  # joining with sentiment "nrc"
  inner_join(get_sentiments("nrc")) %>%
  count( Book,total_word, sentiment , sort = TRUE) %>%
  ungroup() %>%
  mutate(percent = n/total_word)%>%
  # filtering only negative sentiments
  filter(sentiment == "fear") %>%
  arrange(desc(percent)) %>%
  
  ggplot( aes(x = Book, y = percent, fill = Book))+
  geom_col(width = .7 ,show.legend = T ) +
  theme_minimal() + # start with a minimal them and add what we need 
  theme( text = element_text(color = "gray20"),
         legend.position = c("top"), # position the legend to the upper left 
         legend.direction = "horizontal", 
         legend.justification = 0.1, # anchor point for legend position 
         legend.text = element_text(size = 7, color = "black"),
         axis.text = element_text(face = "italic"), 
         axis.text.x = element_text(size = 7),
         axis.line = element_line(color = "gray50", size = 0.5), 
         axis.line.y = element_blank(), 
         panel.grid.major = element_line(color = "gray50", size = .5),
         panel.grid.major.x = element_blank(),
         panel.grid.minor = element_line(color = "gray50", size = .5)
  )
ggsave("Nour/graph/q2.png")
}

##the most imporatnt word by frequency of use
graphQ3 <- function(){
harrypoterclean %>%
  count(Book, word, sort = TRUE) %>%
  group_by(Book) %>%
  top_n(10) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  ggplot( aes(x = word, y = n, fill = Book))+
  geom_col(show.legend = F)+
  facet_wrap(~Book, scales = "free")+
  coord_flip() +
  theme_minimal() + # start with a minimal them and add what we need 
  theme( text = element_text(color = "gray20"),
         axis.text = element_text(face = "italic"), 
         axis.text.x = element_text(size = 7),
         axis.ticks.x = element_blank(),
         axis.line = element_line(color = "gray50", size = 0.5), 
         axis.line.y   = element_blank(), 
         panel.grid.major.y = element_blank(),
         panel.grid.minor.x = element_line(color = "gray50", size = .5),
         panel.grid.major.x = element_line(color = "gray50", size = .5)
  )
  ggsave("Nour/graph/q3.png")
}

# So the most imporatnt word by frequency of use is harry 


##most mentioned character by book graph  
graphQ4 <- function(){
harrypoterclean %>%
  inner_join( characters_listjoin, by = "word" ) %>%
  count(Book, word, sort =  TRUE) %>%
  group_by(Book) %>%
  # take the top ten characters mentioned in each book 
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(paste(word, Book, sep = "__"), n)) %>%
  ggplot(aes(word, n, fill = Book)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ Book, nrow = 2, scales = "free") +
  coord_flip() +
  theme_minimal() + # start with a minimal them and add what we need 
  theme( text = element_text(color = "gray20"),
         axis.text = element_text(face = "italic"), 
         axis.text.x = element_text(size = 7),
         axis.ticks.x = element_blank(),
         axis.line = element_line(color = "gray50", size = 0.5), 
         axis.line.y   = element_blank(), 
         panel.grid.major.y = element_blank(),
         panel.grid.minor.x = element_line(color = "gray50", size = .5),
         panel.grid.major.x = element_line(color = "gray50", size = .5)
  )

ggsave("Nour/graph/q4.png")
}

##how harry mentioned changed by Book graph 
graphQ5 <- function(){
harrypoterclean %>%
  inner_join(characters_listjoin, by = "word") %>%
  count(Book, word) %>%
  filter(word == "harry") %>%
  # plotting graph by book 
  ggplot(aes(x = Book, y = n, fill = Book)) +
  geom_col(width = .7 ,show.legend = T ) +
  theme_minimal() + # start with a minimal them and add what we need 
  theme( text = element_text(color = "gray20"),
         legend.position = c("top"), # position the legend to the upper left 
         legend.direction = "horizontal", 
         legend.justification = 0.1, # anchor point for legend position 
         legend.text = element_text(size = 7, color = "black"),
         axis.text = element_text(face = "italic"), 
         axis.text.x = element_text(size = 7),
         axis.line = element_line(color = "gray50", size = 0.5), 
         axis.line.y = element_blank(), 
         panel.grid.major = element_line(color = "gray50", size = .5),
         panel.grid.major.x = element_blank(),
         panel.grid.minor = element_line(color = "gray50", size = .5)
  )
ggsave("Nour/graph/q5.png")
}

##sentiment by Book 
graphQ6 <- function(){
harrypoterclean %>%
  # filter uut confusing words 
  filter(word != "professor") %>%
  anti_join(characters_listjoin) %>%
  # joining with sentiment
  inner_join(get_sentiments("nrc")) %>%
  count(Book, sentiment, word, sort = TRUE) %>%
  group_by(Book) %>%
  # subseting top 10 mentioned words per sentiment 
  top_n(10) %>%
  ungroup() %>%
  # graph making 
  mutate(word = reorder(paste(sentiment, Book, sep = "__"), n)) %>%
  ggplot(aes(word, n, fill = Book)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~Book, nrow = 2, scales = "free") +
  coord_flip()  +
  theme_minimal() + # start with a minimal them and add what we need 
  theme( text = element_text(color = "gray20"),
         axis.text = element_text(face = "italic"), 
         axis.text.x = element_text(size = 7),
         axis.ticks.x = element_blank(),
         axis.line = element_line(color = "gray50", size = 0.5), 
         axis.line.y   = element_blank(), 
         panel.grid.major.y = element_blank(),
         panel.grid.minor.x = element_line(color = "gray50", size = .3),
         panel.grid.major.x = element_line(color = "gray50", size = .3)
  )
ggsave("Nour/graph/q6.png")
}
##frequency of harry mentioning over the books: 
graphQ7 <- function(){
harrypoterclean %>%
  filter(word == "harry") %>%
  mutate( fiftylines=  floor(line / 50)) %>%
  count(Book, fiftylines, word) %>%
  #making graph 
  ggplot(aes(fiftylines, n, color = Book)) +
  # Make facets by Book
  facet_wrap(~Book) +
  geom_line(size = .1, show.legend = FALSE) +
  expand_limits(y = 0)
ggsave("Nour/graph/q7.png")
}

##most popular characters over time by book

graphQ8 <- function(){
harrypoterclean %>%
  inner_join(characters_listjoin) %>%
  mutate( fiftylines=  floor(line / 50)) %>%
  count(Book, fiftylines, word) %>% 
  arrange(Book, fiftylines, word) %>%
  group_by(Book,fiftylines) %>%
  filter(word != "sir") %>%
  filter(n == max(n)) %>%
  ungroup() %>%
  top_n(100) %>%
  ggplot(aes(x = word , by= word, fill = word)) +
  facet_wrap(~Book) +
  geom_bar(show.legend = F) +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.major.y  = element_blank(),
    text = element_text(size = 7, color = "black")
  )
  ggsave("Nour/graph/q8.png")
}
graphQ1()
graphQ2()
graphQ3()
graphQ4()
graphQ5()
graphQ6()
graphQ7()
graphQ8()
