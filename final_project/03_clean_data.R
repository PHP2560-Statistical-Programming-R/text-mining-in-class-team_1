wrangle_and_clean <- function(){
  library(harrypotter)
  # Combine all The Harry Potter books into 1 dataframe and add column which will be used a reference "Title"
  
  book1 <- data_frame(Title="Harry Potter and the Philosopher's Stone", text=philosophers_stone)
  book2 <- data_frame(Title="Harry Potter and the Chamber of Secrets", text=chamber_of_secrets)
  book3 <- data_frame(Title="Harry Potter and the Prisoner of Azkaban", text=prisoner_of_azkaban)
  book4 <- data_frame(Title="Harry Potter and the Goblet of Fire", text=goblet_of_fire)
  book5 <- data_frame(Title="Harry Potter and the Order of the Phoenix", text=order_of_the_phoenix)
  book6 <- data_frame(Title="Harry Potter and the Half-blood Prince", text=half_blood_prince)
  book7 <- data_frame(Title="Harry Potter and the Deathly Hallows", text=deathly_hallows)
  
  # Join BookMetadata and HarryPotter dfs by "Title"
  harrypotterSeries <- rbind(book1,book2, book3, book4, book5, book6, book7) %>%
    group_by(Title) %>%
    mutate(Chapter = row_number())%>% # add chapter
    ungroup()%>%
    unnest_tokens(sentence, text, token = "sentences") %>%
    mutate(line = row_number()) # add sentences
  
  
  # Restructure it in the one-token-per-row format, 
  harrypotter_clean_tokens <- harrypotterSeries %>%
    unnest_tokens(word, sentence) %>%
    #Remove stop words; stop words are words that are not useful
    #for an analysis, typically extremely common words such as "the", "of", "to",
    anti_join(stop_words)
  
  save(harrypotter_clean_tokens, file = "final_project/data/harrypotter_clean_tokens.Rda")
  
}

wrangle_and_clean()
