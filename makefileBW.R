## run all scripts
source("inputHPBW.R")   # Scrape data
source("combineBW.R")     # clean data
source("Question1BW.R")     # create graph
rmarkdown::render("paper_Bowei_Wei.Rmd", output_format = "html_document")

