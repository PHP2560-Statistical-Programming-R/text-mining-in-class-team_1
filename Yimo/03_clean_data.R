clean_data <- function(){
  library(harrypotter)
  #Write a function for data cleaning
  book_tidy = function(name, title_name){
    
    #Add chapter
    pattern = one_or_more(one_or_more(UPPER) %R% optional(SPC) %R% optional("-"))
    pattern_chapter = capture(pattern) %R% SPC %R% SPC
    chapter_name = str_extract(get(name), pattern =pattern_chapter) 
    ##Add chapter name
    chapter = tibble(text = get(name), chapter_name = chapter_name)%>%
      ##Add chapter number
      mutate(chapter = row_number())
    
    #Add sentence and sentence number
    sentence = chapter%>%
      unnest_tokens(sentence, text, token = "sentences")%>%
      mutate(sentences = row_number())
    
    #Add word  
    df = sentence %>%
      unnest_tokens(word, sentence)
    
    #Add book(book title)
    title = tibble(book = title_name)
    return(cbind(title, df))
  }
  
  #The vector "names" saves the book names of all the 7 books
  names = c("philosophers_stone",
            "chamber_of_secrets",
            "prisoner_of_azkaban",
            "goblet_of_fire",
            "order_of_the_phoenix",
            "half_blood_prince",
            "deathly_hallows")
  
  #Set the names to title format
  title_names = names%>%
    str_replace_all(patter = "_", replacement = " ")%>%
    str_to_title()
  
  #Save 7 books in two ways for further analysis
  
  
  ##First, save them in a list with each in harrypotter[[i]].
  harrypotter = rep(list(tibble()), 7)
  
  
  for(i in 1:length(names)){
    harrypotter[[i]] = book_tidy(names[i], title_names[i])
  }
  
  ##Second, save them all together in a tibble.
  harry_series = tibble()
  for(i in 1:length(names)){
    harry_series = rbind(harry_series, book_tidy(names[i], title_names[i]))
  }
  
  ###Join the tibble with bookMetadata to get publication year and sales colume
  whole_series = harry_series%>%
    inner_join(bookMetadata, by = c(book = "Title"))
  
  
  ###Relevel the book names so that they are in publication order
  for(i in 7:1){
    whole_series$book = relevel(as.factor(whole_series$book), ref = title_names[i])
  }
  
  
  
  book_to_gram = function(name, title_name){
    #Add chapter
    pattern = one_or_more(one_or_more(UPPER) %R% optional(SPC) %R% optional("-"))
    pattern_chapter = capture(pattern) %R% SPC %R% SPC
    chapter_name = str_extract(get(name), pattern =pattern_chapter)
    chapter = tibble(text = get(name), chapter_name = chapter_name)%>%
      mutate(chapter = row_number())
    #Add sentence
    sentence = chapter%>%
      unnest_tokens(sentence, text, token = "sentences")%>%
      mutate(sentences = row_number())
    #Add word  
    df = sentence %>%
      unnest_tokens(bigram, sentence, token = "ngrams", n = 2)
    #Add book
    title = tibble(book = title_name)
    return(cbind(title, df))
  }
  
  
  harry_gram = tibble()
  
  for(i in 1:length(names)){
    harry_gram = rbind(harry_gram, book_to_gram(names[i], title_names[i]))
  }
  
  save(whole_series, file = "Yimo/data/whole_series.Rda")
  save(harrypotter, file = "Yimo/data/harrypotter.Rda")
  save(harry_gram, file = "Yimo/data/harry_gram.Rda")
}

clean_data()
