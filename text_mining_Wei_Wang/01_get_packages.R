## This function takes the packages that our program needs. 
## It makes sure you have them on your computer before proceeding.

source("check_packages.R")
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
