# check that all required packages are installed 
source("Yimo/check_packages.R")
check.packages(c("devtools", "harrypotter","rebus","tidytext", "dplyr", "stringr", "stringi", "ggplot2", 
                 "tidyverse","scales","wordcloud","igraph", "ggraph"))

# making sure harry potter version is uptodate
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")}
devtools::install_github("bradleyboehmke/harrypotter")

