

get_harry_potter_characters <- function(){
  # Fetch Book metadata from theGuardian.com e.g publisher, ranking, sales, author e.t.c 
  bookMetadata <- as_tibble(read.csv("https://docs.google.com/spreadsheets/d/1dhxblR1Vl7PbVP_mNhwEa3_lfUWiF__xSODLq1W83CA/export?format=csv&id=1dhxblR1Vl7PbVP_mNhwEa3_lfUWiF__xSODLq1W83CA&gid=0"))
  
  #Make some changes to titles so that they match the ones in harrypotter dataset
  harry_potter = "Harry Potter and the "
  bookMetadata$Title  = bookMetadata$Title%>%
    str_replace_all(pattern = or(harry_potter, "'"), "")%>%
    str_replace_all(pattern = "-", " ")%>%
    str_to_title()
  
  #Set the volume of sales as numeric
  bookMetadata$Volume.Sales = bookMetadata$Volume.Sales%>%
    str_replace_all(pattern = ",", "")%>%
    as.numeric()
  
  #Add the volume sales of "Half Blood Prince:children's Edition" to that of "Half Blood Prince"
  bookMetadata$Volume.Sales[bookMetadata$Title == "Half Blood Prince"] = bookMetadata$Volume.Sales[bookMetadata$Title == "Half Blood Prince"] + bookMetadata$Volume.Sales[bookMetadata$Title == "Half Blood Prince:childrens Edition"]
  
  #Add the column year which describes the publication year
  date_pattern = ", " %R% capture(UPPER %R% one_or_more(WRD)) %R% SPC %R% capture(one_or_more(DGT)) %R% "," %R% SPC %R% capture(one_or_more(DGT))
  bookMetadata = bookMetadata%>%
    mutate(year = as.numeric(str_match(bookMetadata$Publication.Date, pattern = date_pattern)[,4]))
  
  #"https://en.wikipedia.org/wiki/List_of_Harry_Potter_characters" shows the names.
  
  #Read txt
  characters_txt = readLines("harry_potter_characters.txt")
  #Define names pattern
  pattern = "\t" %R% capture(UPPER %R% one_or_more(WRD)) %R% SPC %R% capture(UPPER %R% one_or_more(WRD))
  #Extract the names
  characters = str_match(characters_txt, pattern = pattern)%>%
    as_tibble()%>%
    setNames(c("name", "first_name", "last_name"))%>%
    filter(!is.na(name)) #Remove NA's
  
  #Save both first and second name in a vector
  name = tibble(name = unique(c(characters$first_name, characters$last_name)))%>%
    #Add a column called lower, which is the names in lower case.
    mutate(lower = tolower(name))  
  
  
  save(bookMetadata, file = "Yimo/data/bookMetadata.rda")
  save(name, file="Yimo/data/name.rda")
  
}

get_harry_potter_characters()