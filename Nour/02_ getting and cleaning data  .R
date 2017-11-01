# data importing and cleaning 

# tidying the text of the first book: 
chamber_of_secrets_tidy <- data_frame( text = chamber_of_secrets, Book = "chamber_of_secrets") %>%
  mutate(chapter = row_number() ) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(line = row_number())
# tidy the second book 
deathly_hallows_tidy <- data_frame(text = deathly_hallows , Book = "deathly_hallows" ) %>%
  mutate(chapter = row_number() ) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(line = row_number())
# the Third book
goblet_of_fire_tidy <- data_frame(text = goblet_of_fire, Book = "goblet_of_fire")%>%
  mutate(chapter = row_number() ) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(line = row_number())
# the fourth book
half_blood_prince_tidy <- data_frame(text = half_blood_prince, Book = "half_blood_prince") %>%
  mutate(chapter = row_number() ) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(line = row_number())
# the fifth book 
order_of_the_phoenix_tidy <- data_frame(text = order_of_the_phoenix, Book = "order_of_the_phoenix") %>%
  mutate(chapter = row_number() ) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(line = row_number())
# the sixth book 
philosophers_stone_tidy <- data_frame(text = philosophers_stone, Book = "philosophers_stone" ) %>%
  mutate(chapter = row_number() ) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(line = row_number())
# the seventh book
prisoner_of_azkaban_tidy <- data_frame(text = prisoner_of_azkaban, Book = "prisoner_of_azkaban") %>%
  mutate(chapter = row_number() ) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(line = row_number())

# making one dataframe for all books 
harrypoter <- rbind(chamber_of_secrets_tidy, deathly_hallows_tidy, goblet_of_fire_tidy, half_blood_prince_tidy, order_of_the_phoenix_tidy, philosophers_stone_tidy, prisoner_of_azkaban_tidy) 
harrypoter


#remove stop words that are unncessary for analysis like a, an , as ... etc. 
harrypoterclean <- harrypoter %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words)
