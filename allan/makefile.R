## clean all output from previous runs of scripts
unlink("allan/data", recursive = TRUE) # where data is stored after clean
unlink("allan/graph", recursive = TRUE) # where graphs are stores

# now re-create the results directory
dir.create(file.path("allan/data"), showWarnings = FALSE)
dir.create(file.path("allan/graph"), showWarnings = FALSE)


## run all scripts
source("allan/00_helper_functions.R") # Helpers
source("allan/01_web_scraping.R")   # web Scrape data
source("allan/02_load_harry_potter_books.R") # create hp books
source("allan/03_clean_data.R") # wrangle, clean and tokenize data
source("allan/04_main_analysis.R")  # create graphs and datatable
#source("allan/05_other_analysis.R")  ## create graphs and datatable
rmarkdown::render("allan/paper.Rmd", output_format = "html_document")

