#Main characters of Harry Potter Series from Harry Potter Wikipedia page
raw_wiki_names = readLines('https://en.wikipedia.org/wiki/List_of_Harry_Potter_characters')

#Define custom stop word used in data cleaning
custom_stop_words <-c('Wikipedia','Wikimedia','The','Puppet','Hogwarts','Stories', 'Quidditch','Death',"Wizarding", "Supporting", "Lego","Privacy","Fantastic")

character_pattern<-capture (UPPER %R% one_or_more(WRD)) %R% SPC %R% capture(UPPER %R% one_or_more(WRD))
harrypotter_characters <- str_match(raw_wiki_names, character_pattern) %>%
  as.data.frame() %>% # convert to data frame for easy manipulation
  na.exclude()%>% # remove missing data
  setNames(c( "FullName", "FirstName", "LastName")) %>% # set column names
  arrange(FullName) %>% # arrange
  filter(!FirstName%in%custom_stop_words) %>% # clean data by first name
  filter(!LastName%in%custom_stop_words) %>% # clean data by last name 
  unique() 