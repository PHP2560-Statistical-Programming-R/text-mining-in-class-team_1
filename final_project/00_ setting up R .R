# check that all required packages are installed 
source("final_project/check_packages.R")
check.packages(c("ggplot2", "dplyr", "stringi", "stringr", "harrypotter", "tidytext", "rebus",
                 "gutenbergr", "wordcloud", "RColorBrewer","tidyr", "ggrepel", "textreadr",
                 "igraph","ggraph","XML","RCurl","topicmodels", "scales", "tidyverse"))
# making sure harry potter version is uptodate
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")}
devtools::install_github("bradleyboehmke/harrypotter")
# R.utils
install.packages('R.utils', repos="http://cran.us.r-project.org",dependencies=TRUE)


