check_packages = function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org")
    
    library(name, character.only=TRUE)
  }
}
## This function takes the packages that our program needs. 
## It makes sure you have them on your computer before proceeding.
check_packages(c("wordcloud",
                 "devtools",
                 "tidyverse",
                 "stringr",
                 "tidytext",
                 "dplyr",
                 "reshape2",
                 "igraph",
                 "ggraph",
                 "ggplot2"))


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

devtools::install_github("bradleyboehmke/harrypotter", force = TRUE)


hp_books <- c("Harry Potter and the Philosopher's Stone", 
              "Harry Potter and the Chamber of Secrets", 
              "Harry Potter and the Prisoner of Azkaban",
              "Harry Potter and the Goblet of Fire", 
              "Harry Potter and the Order of the Phoenix", 
              "Harry Potter and the Half-blood Prince",
              "Harry Potter and the Deathly Hallows"
)

hp_list <- list(harrypotter::philosophers_stone, 
                harrypotter::chamber_of_secrets, 
                harrypotter::prisoner_of_azkaban,
                harrypotter::goblet_of_fire, 
                harrypotter::order_of_the_phoenix, 
                harrypotter::half_blood_prince,
                harrypotter::deathly_hallows
)