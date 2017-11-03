# check that all required packages are installed 
source("Nour/check_packages.R")
<<<<<<< HEAD:Nour/01_ setting up R .R
check.packages(c("ggplot2", "dplyr", "stringi", "stringr", "harrypotter", "tidytext", "rebus"))
=======
check.packages(c("dplyr", "stringr", "tidytext", "SnowballC", "ggplot2", "wordcloud" , "RColorBrewer", "ggrepel"
, "igraph", "ggraph", "rebus", "R.utils", "XML", "RCurl", "htmlwidgets", "igraph", "viridis"
, "topicmodels"))
>>>>>>> 8805b2ebe6f35cc63f5719040cc747e69f4f3775:TT/01_ setting up R .R
# making sure harry potter version is uptodate
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")}
devtools::install_github("bradleyboehmke/harrypotter")

