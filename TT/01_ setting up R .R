# check that all required packages are installed 
source("Nour/check_packages.R")
check.packages(c("dplyr", "stringr", "tidytext", "SnowballC", "ggplot2", "wordcloud" , "RColorBrewer", "ggrepel"
, "igraph", "ggraph", "rebus", "R.utils", "XML", "RCurl", "htmlwidgets", "igraph", "viridis"
, "topicmodels"))
# making sure harry potter version is uptodate
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")}
devtools::install_github("bradleyboehmke/harrypotter")

