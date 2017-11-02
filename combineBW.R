## This function takes the packages that our program needs. 
## It makes sure you have them on your computer before proceeding.

source("checkpackagesBW.R")
check_packages(c("dplyr", 
                 "stringr", 
                 "tidytext", 
                 "ggplot2", 
                 "RColorBrewer", 
                 "harrypotter", 
                 "tidyr",
                 "RColorBrewer",
                 "wordcloud",
                 "rebus",
                 "textreadr"))

#load all the packages required for this code
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(RColorBrewer)
library(harrypotter)
library(tidyr)
library(RColorBrewer)
library(wordcloud)
library(rebus)
library(textreadr)

#input the books
source("inputHPBW.R")


# Combine all The Harry Potter books into 1 dataframe and add column which will be used a reference "Title"
book1 <- data_frame(Title="Harry Potter and the Philosopher's Stone", text=philosophers_stone)
book2 <- data_frame(Title="Harry Potter and the Chamber of Secrets", text=chamber_of_secrets)
book3 <- data_frame(Title="Harry Potter and the Prisoner of Azkaban", text=prisoner_of_azkaban)
book4 <- data_frame(Title="Harry Potter and the Goblet of Fire", text=goblet_of_fire)
book5 <- data_frame(Title="Harry Potter and the Order of the Phoenix", text=order_of_the_phoenix)
book6 <- data_frame(Title="Harry Potter and the Half-blood Prince", text=half_blood_prince)
book7 <- data_frame(Title="Harry Potter and the Deathly Hallows", text=deathly_hallows)


# Fetch Book metadata from theGuardian.com e.g publisher, ranking, sales, author e.t.c 

bookMetadata <- read.csv("https://docs.google.com/spreadsheets/d/1dhxblR1Vl7PbVP_mNhwEa3_lfUWiF__xSODLq1W83CA/export?format=csv&id=1dhxblR1Vl7PbVP_mNhwEa3_lfUWiF__xSODLq1W83CA&gid=0")

# Join BookMetadata and HarryPotter dfs by "Title"
harrypotter_and_metadata <- rbind(book1,book2, book3, book4, book5, book6, book7) %>% inner_join(bookMetadata, by = "Title")%>%
  group_by(Title) %>%
  mutate(Chapter = row_number())%>% # add chapter
  ungroup()

harrypotterSeries <- harrypotter_and_metadata %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(line = row_number()) # add sentences

head(harrypotterSeries)


#Restructure it in the one-token-per-row format
harrypotter_tokenized <- harrypotterSeries %>%
  unnest_tokens(word, sentence)

harrypotter_tokenized %>%
  select(Title, Chapter, word) %>%
  head()


#Remove stop words
harrypotter_clean_tokens <- harrypotter_tokenized %>% 
  anti_join(stop_words)  

#display cleaned data
harrypotter_clean_tokens