
##Each book is an array in which each value in the array is a chapter 
series <- tibble()
for(i in seq_along(hp_books)) {
  temp <- tibble(book = seq_along(hp_list[[i]]),
                 text = hp_list[[i]]) %>%
    unnest_tokens(word, text) %>%
    ##Here I tokenize each chapter into words
    mutate(book = hp_books[i]) %>%
    select(book, everything())
  
  series <- rbind(series, temp)
}

## Keep books in order of publication.
series$book <- factor(series$book, levels = rev(hp_books))
series