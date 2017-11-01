# chracters list 

# making list of  the whole series characters: 
# after making a txt file from "https://en.wikipedia.org/wiki/List_of_Harry_Potter_characters" then importing to R 
characters_raw <- readLines("nour/characters_ weki.txt")

# extracting names only based on pattern of two names start with an upper case and space between them.
# used capture to get first and last names seperatly into the data frame. 
characters_raw2 <- str_match( characters_raw, capture (UPPER %R% one_or_more(WRD)) %R% SPC %R% capture(UPPER %R% one_or_more(WRD)))
# excluding missing rows 
characters_raw3 <- as.data.frame( characters_raw2) %>%
  na.exclude() 
# naming colums 
colnames(characters_raw3)  <- c( "full name", "first", "last") 
# making sure no repetitions and names arranged alphabeticly.
characters_raw4 <- characters_raw3 %>%
  arrange(`full name`) %>%
  unique() 
# renaming raws for ease of manual cleaning 
rownames(characters_raw4) <- seq(length=nrow(characters_raw4))
# manually cleaning for non character names 
characters_list <- characters_raw4[- c(41, 49, 58, 71, 161, 162,163), ]
# renaming raws again 
rownames(characters_list) <- seq(length=nrow(characters_list))
# making colum named word for the sake of joining and analysis
characters_list %>%
  mutate(word = first) %>%
  mutate_if(is.factor, as.character) -> characters_listjoin

characters_listjoin$word <- str_replace_all(characters_listjoin$word , pattern = one_or_more(WRD), replacement = tolower)