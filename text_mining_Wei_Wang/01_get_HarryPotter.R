## This function takes the packages that our program needs. 
## It makes sure you have them on your computer before proceeding.

source("check_packages.R")
check_packages(c("wordcloud",
                 "tidyverse",
                 "stringr",
                 "tidytext",
                 "dplyr",
                 "reshape2",
                 "igraph",
                 "ggraph",
                 "ggplot2"))

devtools::install_github("bradleyboehmke/harrypotter", force = TRUE)

