# check that all required packages are installed 
source("Nour/check_packages.R")
check.packages(c("ggplot2", "dplyr", "stringi", "harrypotter", "tidytext", "rebus"))
# making sure harry potter version is uptodate
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")}
devtools::install_github("bradleyboehmke/harrypotter")

