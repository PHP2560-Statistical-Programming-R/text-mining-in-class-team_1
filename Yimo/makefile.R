## clean all output from previous runs of scripts
unlink("Yimo/data", recursive = TRUE) 
unlink("Yimo/graph", recursive = TRUE) 

# now re-create the results directory
dir.create(file.path("Yimo/data"), showWarnings = FALSE)
dir.create(file.path("Yimo/graph"), showWarnings = FALSE)


## run all scripts
source("Yimo/00_ setting up R .R") # setting up R enviroment 
source("Yimo/01_book_information.R")  # Get book information, publication year, etc
source("Yimo/02_book_content.R") #Get the content of the books
source("Yimo/03_clean_data.R") # data cleaning
source("Yimo/04_main_analysis.R")  # analysis and questions answering 

rmarkdown::render("Yimo/paper.Rmd", output_format = "html_document")
