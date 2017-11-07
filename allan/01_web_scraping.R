

get_harry_potter_characters <- function(){
  #web scrape main characters of Harry Potter Series from
  # download harry potter wikipedia page
  hp_wikipedia_site = readLines('https://en.wikipedia.org/wiki/List_of_Harry_Potter_characters')
  #define custom stop word used in HP character names data cleaning
  custom_stop_words <-  c('Wikipedia','Wikimedia','The','Puppet','Hogwarts','Stories','Through')
  # read entire hp_wikipedia_site
  harrypotter_characters <- str_match( hp_wikipedia_site, capture(UPPER %R% one_or_more(WRD)) %R% SPC %R% capture(UPPER %R% one_or_more(WRD))) %>%
    as.data.frame() %>% # convert to data frame for easy manipulation
    na.exclude()%>% # remove missing data
    setNames(c( "FullName", "FirstName", "LastName")) %>% # set column names
    arrange(FullName) %>% # arrange
    filter(!FirstName%in%custom_stop_words) %>% # clean data by first name
    filter(!LastName%in%custom_stop_words) %>% # clean data by last name 
    unique() 
  
  save(harrypotter_characters, file="allan/data/harrypotter_characters.rda")
  
}

get_book_metadata<-function(){
  # Fetch Book metadata from theGuardian.com e.g publisher, ranking, sales, author e.t.c 
  bookMetadata <- read.csv("https://docs.google.com/spreadsheets/d/1dhxblR1Vl7PbVP_mNhwEa3_lfUWiF__xSODLq1W83CA/export?format=csv&id=1dhxblR1Vl7PbVP_mNhwEa3_lfUWiF__xSODLq1W83CA&gid=0")%>%
    mutate(Volume.Sales = as.numeric(gsub(",", "", Volume.Sales)))
  
  # Harry Potter and the Half-blood Prince has 2 versions i.e Children's Edition and main Edition
  # we need to sum up the sales for the 2 versions of "Harry Potter and the Half-blood Prince"
  merged <- bookMetadata %>%
    filter(grepl('Harry Potter and the Half-blood Prince', Title))%>%
    summarize(sum = sum(Volume.Sales))
  
  bookMetadata <- bookMetadata %>%
    mutate(Volume.Sales = ifelse(Title == "Harry Potter and the Half-blood Prince",
                                 merged$sum, Volume.Sales)) 
    
  save(bookMetadata, file="allan/data/book_metadata.rda")

}

get_harry_potter_characters()
get_book_metadata()