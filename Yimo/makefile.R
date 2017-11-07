## clean all output from previous runs of scripts
unlink("Yimo/data", recursive = TRUE) 
unlink("Yimo/graph", recursive = TRUE) 

# now re-create the results directory
dir.create(file.path("Yimo/data"), showWarnings = FALSE)
dir.create(file.path("Yimo/graph"), showWarnings = FALSE)


## run all scripts
source("Yimo/00_ setting up R .R") # setting up R enviroment 
source("Yimo/01_web_scraping.R")  # importing and cleaning data
source("Yimo/03_clean_data.R") # webscraping and cleaning of characters names 
source("Yimo/04_main_analysis.R")  # analysis and questions answering 

rmarkdown::render("Yimo/paper.Rmd", output_format = "pdf_document")
