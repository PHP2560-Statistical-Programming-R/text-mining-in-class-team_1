Text Analyis
================
Wei Wang
10/18/2017

Your mission
============

Perform text analysis.

Okay, I need more information
-----------------------------

Perform sentiment analysis or topic modeling using text analysis methods as demonstrated in the pre-class work and in the readings.

Okay, I need even more information.
-----------------------------------

Do the above. Can't think of a data source?

-   `gutenbergr`
-   `AssociatedPress` from the `topicmodels` package
-   `NYTimes` or `USCongress` from the `RTextTools` package
-   Harry Potter Complete 7 Books text \`\`\` if (packageVersion("devtools") &lt; 1.6) { install.packages("devtools") }

devtools::install\_github("bradleyboehmke/harrypotter") \`\``- [State of the Union speeches](https://pradeepadhokshaja.wordpress.com/2017/03/31/scraping-the-web-for-presdential-inaugural-addresses-using-rvest/) - Scrape tweets using [`twitteR\`\](<https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-1-extract-tweets/>)

Analyze the text for sentiment OR topic. **You do not need to do both**. The datacamp courses and [Tidy Text Mining with R](http://tidytextmining.com/) are good starting points for templates to perform this type of analysis, but feel free to *expand beyond these examples*.

Timelines and Task
==================

We will spend the next 2 weeks working on analyzing textual data in R. You will do the following:

-   Start with some text based data.
-   Clean data and prepare it for analysis
-   Ask questions about the data
-   Answer these questions with the data using tables and graphics
-   Each group member must have their own unique question that they code the answer for.

Setup
=====

``` r
# Load in packages.

library(wordcloud)
library(devtools)
library(tidyverse)      
library(stringr)        
library(tidytext)
library(dplyr)
library(reshape2)
library(igraph)
library(ggraph)
library(ggplot2)

    if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}

devtools::install_github("bradleyboehmke/harrypotter")


# Vizualization settings.
theme_set(theme_light()) # set default ggplot theme to light
fs = 12 # default plot font size
```

1.Data preparation
==================

1.1 Shape the data.
-------------------

``` r
hp_books <- c("Philosopher's Stone", 
            "Chamber of Secrets", 
            "Prisoner of Azkaban",
            "Goblet of Fire", 
            "Order of the Phoenix", 
            "Half-Blood Prince",
            "Deathly Hallows"
            )

hp_list <- list(harrypotter::philosophers_stone, 
                harrypotter::chamber_of_secrets, 
                harrypotter::prisoner_of_azkaban,
                harrypotter::goblet_of_fire, 
                harrypotter::order_of_the_phoenix, 
                harrypotter::half_blood_prince,
                harrypotter::deathly_hallows
                )
```

1.2 Place all of the books in the Harry Potter series into a tibble. Then tokenize the text into single words, strip away all punctuation and capitalization, and add columns to the tibble for the book and chapter.
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
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
```

1.3 Keep books in order of publication.
---------------------------------------

``` r
series$book <- factor(series$book, levels = rev(hp_books))
series
```

    ## # A tibble: 1,089,386 x 2
    ##                   book    word
    ##  *              <fctr>   <chr>
    ##  1 Philosopher's Stone     the
    ##  2 Philosopher's Stone     boy
    ##  3 Philosopher's Stone     who
    ##  4 Philosopher's Stone   lived
    ##  5 Philosopher's Stone      mr
    ##  6 Philosopher's Stone     and
    ##  7 Philosopher's Stone     mrs
    ##  8 Philosopher's Stone dursley
    ##  9 Philosopher's Stone      of
    ## 10 Philosopher's Stone  number
    ## # ... with 1,089,376 more rows

2. Words in books
=================

``` r
used_words <- series %>% 
  count(word, sort = TRUE)
used_words
```

    ## # A tibble: 24,475 x 2
    ##     word     n
    ##    <chr> <int>
    ##  1   the 51593
    ##  2   and 27430
    ##  3    to 26985
    ##  4    of 21802
    ##  5     a 20966
    ##  6    he 20322
    ##  7 harry 16557
    ##  8   was 15631
    ##  9  said 14398
    ## 10   his 14264
    ## # ... with 24,465 more rows

2.1 Plot word frequency per book.
---------------------------------

``` r
# PLOT WORD FREQUENCY PER BOOK
series %>%
  group_by(book, word) %>%
  anti_join(stop_words, by = "word") %>% # delete stopwords
  count() %>% # summarize count per word per book
  arrange(desc(n)) %>% # highest freq on top
  group_by(book) %>% # 
  mutate(top = seq_along(word)) %>% # identify rank within group
  filter(top <= 15) %>% # retain top 15 frequent words
  # create barplot
  ggplot(aes(x = -top, fill = book)) + 
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure words are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = word), size = fs/3, hjust = "left") +
  theme(legend.position = 'none', # get rid of legend
        text = element_text(size = fs), # determine fontsize
        axis.text.x = element_text(angle = 45, hjust = 1, size = fs/1.5), # rotate x text
        axis.ticks.y = element_blank(), # remove y ticks
        axis.text.y = element_blank()) + # remove y text
  labs(y = "Word count", x = "", # add labels
       title = "Harry Plotter: Most frequent words throughout the saga") +
  facet_grid(. ~ book) + # separate plot for each book
  coord_flip() # flip axes
```

![](text-mining_Wei_Wang_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png) \#\# 2.2 Find tf-idf.

2.3 Topic Modeling.
-------------------

3. Sentiment analysis
=====================

3.1 Sentiment analysis by word.
-------------------------------

3.2 Sentiment analysis by message.
----------------------------------

3.3 N-gram analysis.
--------------------

4. Summary
==========
