get_harry_potter_characters <- function(){
    #web scrape main characters of Harry Potter Series from
    # download harry potter wikipedia page
    hp_wikipedia_site = readLines('https://en.wikipedia.org/wiki/List_of_Harry_Potter_characters')
    #define custom stop word used in HP character names data cleaning
    custom_stop_words <-  c('Wikipedia','Wikimedia','The','Puppet','Hogwarts','Stories','Through', 'Quidditch','Death',"Wizarding", "Supporting", "Lego","Privacy","Fantastic")
    # read entire hp_wikipedia_site
    harrypotter_characters <- str_match( hp_wikipedia_site, capture(UPPER %R% one_or_more(WRD)) %R% SPC %R% capture(UPPER %R% one_or_more(WRD))) %>%
      as.data.frame() %>% # convert to data frame for easy manipulation
      na.exclude()%>% # remove missing data
      setNames(c( "FullName", "FirstName", "LastName")) %>% # set column names
      arrange(FullName) %>% # arrange
      filter(!FirstName%in%custom_stop_words) %>% # clean data by first name
      filter(!LastName%in%custom_stop_words) %>% # clean data by last name 
      unique() 
    
    save(harrypotter_characters, file="TT/data/harrypotter_characters.rda")
}
get_book_metadata<-function(){
  # Fetch series information from theGuardian.com e.g publisher, ranking, sales, author e.t.c 
  
  seriesinfo <- read.csv("https://docs.google.com/spreadsheets/d/1dhxblR1Vl7PbVP_mNhwEa3_lfUWiF__xSODLq1W83CA/export?format=csv&id=1dhxblR1Vl7PbVP_mNhwEa3_lfUWiF__xSODLq1W83CA&gid=0")

    
    save(seriesinfo, file="TT/data/seriesinfo.rda")
}
get_harry_potter_characters()
get_book_metadata()