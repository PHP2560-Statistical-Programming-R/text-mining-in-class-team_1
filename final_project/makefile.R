## clean all output from previous runs of scripts
unlink("final_project/data", recursive = TRUE) 
unlink("final_project/graph", recursive = TRUE) 

# now re-create the results directory
dir.create(file.path("final_project/data"), showWarnings = FALSE)
dir.create(file.path("final_project/graph"), showWarnings = FALSE)


## run all scripts
source("final_project/00_ setting up R .R") # setting up R enviroment 
source("final_project/01_web_scraping.R")  # importing and cleaning data
source("final_project/03_clean_data.R") # webscraping and cleaning of characters names 
source("final_project/04_main_analysis.R")  # analysis and questions answering 
rmarkdown::render("final_project/paper.Rmd", output_format = "pdf_document")
