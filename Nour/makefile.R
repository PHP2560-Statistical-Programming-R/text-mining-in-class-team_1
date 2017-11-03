## clean all output from previous runs of scripts
unlink("Nour/data", recursive = TRUE) 
unlink("Nour/graph", recursive = TRUE) 

# now re-create the results directory
dir.create(file.path("Nour/data"), showWarnings = FALSE)
dir.create(file.path("Nour/graph"), showWarnings = FALSE)


## run all scripts
source("Nour/01_ setting up R .R") # setting up R enviroment 
source("Nour/02_ getting and cleaning data  .R")  # importing and cleaning data
source("Nour/03_ getting and cleaning characters list.R") # webscraping and cleaning of characters names 
source("Nour/04_ Analysis.R")  # analysis and questions answering 
rmarkdown::render("Nour/paper.Rmd", output_format = "pdf_document")
