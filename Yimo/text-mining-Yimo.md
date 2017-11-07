Text Analyis
================

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
-   Harry Potter Complete 7 Books text

    ``` r
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }

    devtools::install_github("bradleyboehmke/harrypotter")
    ```

        ## Skipping install of 'harrypotter' from a github remote, the SHA1 (51f71461) has not changed since last install.
        ##   Use `force = TRUE` to force installation

-   [State of the Union speeches](https://pradeepadhokshaja.wordpress.com/2017/03/31/scraping-the-web-for-presdential-inaugural-addresses-using-rvest/)
-   Scrape tweets using [`twitteR`](https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-1-extract-tweets/)

Analyze the text for sentiment OR topic. **You do not need to do both**. The datacamp courses and [Tidy Text Mining with R](http://tidytextmining.com/) are good starting points for templates to perform this type of analysis, but feel free to *expand beyond these examples*.

Timelines and Task
==================

We will spend the next 2 weeks working on analyzing textual data in R. You will do the following:

-   Start with some text based data.
-   Clean data and prepare it for analysis
-   Ask questions about the data
-   Answer these questions with the data using tables and graphics
-   Each group member must have their own unique question that they code the answer for.

Group Question Sets
===================

``` r
## Questions for analysis: 
## 1. what is the most important charecter based on how much is whas mentioned ? 
## 2. what is the most scariest book based on sentiment analysis ?
## 3. what the top ten used words in exception to stop words ?
## 4. sentiments by books 
## 5. sentiment by popularity based on "https://www.theguardian.com/news/datablog/2012/aug/09/best-selling-books-all-time-fifty-shades-grey-compare" 
# And the sixth question is unique for every student.
```

0. Package load and environment setting.
========================================

``` r
    if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}

devtools::install_github("bradleyboehmke/harrypotter")
```

    ## Skipping install of 'harrypotter' from a github remote, the SHA1 (51f71461) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
library(devtools)
```

    ## Warning: package 'devtools' was built under R version 3.4.2

``` r
library(harrypotter)
library(rebus)
```

    ## Warning: package 'rebus' was built under R version 3.4.2

``` r
library(tidytext)
```

    ## Warning: package 'tidytext' was built under R version 3.4.2

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
```

    ## Warning: package 'stringr' was built under R version 3.4.2

    ## 
    ## Attaching package: 'stringr'

    ## The following object is masked from 'package:rebus':
    ## 
    ##     regex

``` r
library(stringi)
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.2

    ## 
    ## Attaching package: 'ggplot2'

    ## The following object is masked from 'package:rebus':
    ## 
    ##     alpha

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.4.2

    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr

    ## Warning: package 'tibble' was built under R version 3.4.2

    ## Warning: package 'tidyr' was built under R version 3.4.2

    ## Warning: package 'readr' was built under R version 3.4.2

    ## Warning: package 'purrr' was built under R version 3.4.2

    ## Conflicts with tidy packages ----------------------------------------------

    ## alpha():  ggplot2, rebus
    ## filter(): dplyr, stats
    ## lag():    dplyr, stats
    ## regex():  stringr, rebus

``` r
library(scales)
```

    ## Warning: package 'scales' was built under R version 3.4.2

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

    ## The following object is masked from 'package:rebus':
    ## 
    ##     alpha

``` r
library(wordcloud)
```

    ## Warning: package 'wordcloud' was built under R version 3.4.2

    ## Loading required package: RColorBrewer

``` r
library(igraph)
```

    ## Warning: package 'igraph' was built under R version 3.4.2

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     compose, simplify

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following object is masked from 'package:tibble':
    ## 
    ##     as_data_frame

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:rebus':
    ## 
    ##     %c%, graph

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
library(ggraph)
```

    ## Warning: package 'ggraph' was built under R version 3.4.2

1. Preperation
==============

1.1 Load the 7 novels in harry potter series. Add chapter, sentence and word column for further analysis. Remove stop\_words as text\_cleaning. Save these books in variables.
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
#Function that adds columns called chapter, sentence, word and title, where word contain individual words in the novel, while sentence and chapter refers to the correpsonding chapter and sentence that the word appears, and book referes to the name of the book.



# Fetch Book metadata from theGuardian.com e.g publisher, ranking, sales, author e.t.c 
bookMetadata <- as_tibble(read.csv("https://docs.google.com/spreadsheets/d/1dhxblR1Vl7PbVP_mNhwEa3_lfUWiF__xSODLq1W83CA/export?format=csv&id=1dhxblR1Vl7PbVP_mNhwEa3_lfUWiF__xSODLq1W83CA&gid=0"))

harry_potter = "Harry Potter and the "
bookMetadata$Title  = bookMetadata$Title%>%
  str_replace_all(pattern = or(harry_potter, "'"), "")%>%
  str_replace_all(pattern = "-", " ")%>%
  str_to_title()

bookMetadata$Volume.Sales = bookMetadata$Volume.Sales%>%
  str_replace_all(pattern = ",", "")%>%
  as.numeric()


bookMetadata$Volume.Sales[bookMetadata$Title == "Half Blood Prince"] = bookMetadata$Volume.Sales[bookMetadata$Title == "Half Blood Prince"] + bookMetadata$Volume.Sales[bookMetadata$Title == "Half Blood Prince:childrens Edition"]


date_pattern = ", " %R% capture(UPPER %R% one_or_more(WRD)) %R% SPC %R% capture(one_or_more(DGT)) %R% "," %R% SPC %R% capture(one_or_more(DGT))
bookMetadata = bookMetadata%>%
  mutate(year = as.numeric(str_match(bookMetadata$Publication.Date, pattern = date_pattern)[,4]))
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.2

``` r
book_tidy = function(name, title_name){
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
    unnest_tokens(word, sentence)
#Add book
  title = tibble(book = title_name)
  return(cbind(title, df))
}

#The vector "names" saves the book names of all the 7 books.
names = c("philosophers_stone",
         "chamber_of_secrets",
         "prisoner_of_azkaban",
         "goblet_of_fire",
         "order_of_the_phoenix",
         "half_blood_prince",
         "deathly_hallows")

title_names = names%>%
  str_replace_all(patter = "_", replacement = " ")%>%
  str_to_title()

#Save 7 books in two ways for further analysis.
## First, save 7 books individually in a list called "harrypotter".
## Second, save 7 books together in a tibble called "whole_series".

#Define the variables
harrypotter = rep(list(tibble()), 7)
harry_series = tibble()

for(i in 1:7){
  harrypotter[[i]] = book_tidy(names[i], title_names[i]) %>%
                     anti_join(stop_words)
 
}
```

    ## Joining, by = "word"
    ## Joining, by = "word"
    ## Joining, by = "word"
    ## Joining, by = "word"
    ## Joining, by = "word"
    ## Joining, by = "word"
    ## Joining, by = "word"

``` r
for(i in 1:length(names)){
  harry_series = rbind(harry_series, book_tidy(names[i], title_names[i]))
}




whole_series = harry_series%>%
  inner_join(bookMetadata, by = c(book = "Title"))

for(i in 7:1){
   whole_series$book = relevel(as.factor(whole_series$book), ref = title_names[i])
 }
```

1.2 Find the names of main characters and save them. The names of main characters were found in website
-------------------------------------------------------------------------------------------------------

``` r
#"https://en.wikipedia.org/wiki/List_of_Harry_Potter_characters", the content is simply copy and paste into local document called "characters.txt".

#Read txt.
characters_txt = readLines("harry_potter_characters.txt")
#Define pattern
pattern = "\t" %R% capture(UPPER %R% one_or_more(WRD)) %R% SPC %R% capture(UPPER %R% one_or_more(WRD))
#Extract the names in the txt.
characters = str_match(characters_txt, pattern = pattern)%>%
  as_tibble()%>%
  setNames(c("name", "first_name", "last_name"))%>%
  #Remove NA
  filter(!is.na(name))

#Save both first and second name in a vector
name = tibble(name = unique(c(characters$first_name, characters$last_name)))%>%
  #Add a column called lower, which is the names in lower case.
  mutate(lower = tolower(name))
```

2. Frequency analysis
=====================

``` r
#Look for the ten most mentioned names in each book.
name_freq = whole_series%>%

  inner_join(name, by = c(word = "lower"))%>%
  count(word)%>%
  top_n(10)%>%
  arrange(desc(n))%>%
  ungroup()%>%
  mutate(word = reorder(str_to_title(word), n))
```

    ## Selecting by n

``` r
ggplot(name_freq, aes(x = word, y = n, fill = n))+
   geom_col(show.legend = FALSE) +
   
   
  

  coord_flip()+
  ggtitle("Popular characters")
```

![](text-mining-Yimo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

``` r
#As we can see in the plot, Harry is the most important character in the novel. Also the second and third important character is Ron and Hermione.



ron_minus_herm = whole_series%>%
  filter(word %in% c("ron", "hermione"))%>%
  count(book, chapter, word)%>%
  spread(word, n)%>%
  replace_na(list(ron = 0, hermione = 0))%>%
  mutate(dif = ron - hermione, signal = dif/abs(dif))


ggplot(ron_minus_herm, aes(chapter, dif, fill = signal))+
  geom_col(show.legend = F)+
  facet_wrap(~book, scales = "free")
```

![](text-mining-Yimo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-2.png)

``` r
#word frequency
word_freq = whole_series%>%
  count(word)

wordcloud(words = word_freq$word, freq = word_freq$n, min.freq = 5, max.words = 50, random.order = F, colors=brewer.pal(12, "Paired"), rot.per = 0.2)
```

![](text-mining-Yimo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-3.png)

2. Sentiment analysis
=====================

2.1 The scarest book
--------------------

``` r
emotion = get_sentiments("nrc")
unique(emotion[,2])
```

    ## # A tibble: 10 x 1
    ##       sentiment
    ##           <chr>
    ##  1        trust
    ##  2         fear
    ##  3     negative
    ##  4      sadness
    ##  5        anger
    ##  6     surprise
    ##  7     positive
    ##  8      disgust
    ##  9          joy
    ## 10 anticipation

``` r
total = whole_series%>%
  count(book)%>%
  rename(total_word = n)


scare_emotion = whole_series%>%
  left_join(total, by = "book")%>%
  inner_join(emotion, by = "word")%>%
  count(book, sentiment, total_word)%>%
  ungroup()%>%
  mutate(percent = n/total_word)%>%
  filter(sentiment == "fear")%>%
  arrange(desc(percent))


  ggplot(scare_emotion,aes(x = book, y = percent, color = book))+
    geom_line(color = "gray50", group = 1)+
  geom_point(size = 4)+
 
scale_y_continuous(labels = percent_format())+
 theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
```

![](text-mining-Yimo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

``` r
#Based on sentiment
  harry_sentiment = whole_series%>%
    inner_join(emotion, by = "word")%>%
    filter(sentiment != "positive" & sentiment != "negative")%>%
    count(book, year, sentiment)%>%
    arrange(n)
  
  ggplot(harry_sentiment, aes(reorder(sentiment, n), n, fill = sentiment))+
           geom_col()+
    facet_wrap(~book,  scales = "free_x")+
     theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
```

![](text-mining-Yimo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-2.png)

``` r
  #Changes of anger over chapters
anger = whole_series%>%
  inner_join(total)%>%
  inner_join(emotion, by = "word")%>%
  filter(sentiment == "anger")%>%
  count(book, chapter, sentiment, total_word)%>%
  mutate(percent = n/total_word)%>%
  ungroup()
```

    ## Joining, by = "book"

``` r
ggplot(anger, aes(x = chapter, y = percent, color = book))+ 
  geom_point(show.legend = F)+
  geom_line(size = 0.7, show.legend = F)+
  scale_y_log10(labels = percent_format())+
  facet_wrap(~book, scales = "free_x")
```

![](text-mining-Yimo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-3.png)

``` r
#See how sentiment changes through chapter.
  
score = get_sentiments("afinn")

emotion_in_chap = whole_series%>%
 count(book, chapter, word)%>%
 inner_join(score, by = "word")%>%
  group_by(book, chapter)%>%
  summarise(contribution = sum(score*n)/sum(n))
  

ggplot(emotion_in_chap, aes(x = chapter, y = contribution, color = book))+ 
  geom_col(show.legend = F)+
  facet_wrap(~book,  scales = "free_x")
```

![](text-mining-Yimo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-4.png)

2.2 Sentiment by popularity
---------------------------

``` r
score = get_sentiments("afinn")


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
```

    ## Warning: Column `book`/`Title` joining factor and character vector,
    ## coercing into character vector

``` r
ggplot(sentiment_by_popularity)+
  geom_bar( aes(x = book, y = sales, fill = contribution), stat = "identity")+
  scale_x_discrete(limits = rev(sentiment_by_popularity$book))+
  coord_flip()
```

![](text-mining-Yimo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

Extra analysis
==============

Term frequency
--------------

``` r
count_word = whole_series%>%
  count(book, word, sort = T)%>%
  ungroup()

total_words = count_word%>%
  group_by(book)%>%
  summarize(total = sum(n))

freq_term = count_word%>%
  left_join(total_words)
```

    ## Joining, by = "book"

``` r
ggplot(freq_term, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book,  ncol = 2, scales = "free_y")+
  xlab("Term Frequency")+
  ylab("Number of Words")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1114 rows containing non-finite values (stat_bin).

![](text-mining-Yimo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png) \#\#Zipf's Law

``` r
freq_by_rank <- freq_term %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         term_frequency= n/total)

head(freq_by_rank)
```

    ## # A tibble: 6 x 6
    ## # Groups:   book [4]
    ##                   book  word     n  total  rank term_frequency
    ##                 <fctr> <chr> <int>  <int> <int>          <dbl>
    ## 1 Order Of The Phoenix   the 11746 258803     1     0.04538587
    ## 2      Deathly Hallows   the 10438 199700     1     0.05226840
    ## 3       Goblet Of Fire   the  9305 191884     1     0.04849284
    ## 4    Half Blood Prince   the  7531 171493     1     0.04391433
    ## 5 Order Of The Phoenix    to  6518 258803     2     0.02518518
    ## 6 Order Of The Phoenix   and  6192 258803     3     0.02392553

``` r
ggplot(freq_by_rank, aes(rank, term_frequency, color = book)) + 
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()
```

![](text-mining-Yimo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png)

``` r
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm = lm(log10(term_frequency) ~ log10(rank), data = rank_subset)
coeff = lm$coefficients



  ggplot(freq_by_rank, aes(rank, term_frequency, color = book)) + 
  geom_abline(intercept = coeff[1], slope = coeff[2], color = "gray50", linetype = 2) +
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()
```

![](text-mining-Yimo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-2.png)

N-grams and Correlation
-----------------------

``` r
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
  theme_void()
```

![](text-mining-Yimo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png)

Acknowledgments
===============

Question Template from <http://tidytextmining.com>
