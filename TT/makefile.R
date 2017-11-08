## clean all output from previous runs of scripts
unlink("TT/data", recursive = TRUE) # where data is stored after clean
unlink("TT/graph", recursive = TRUE) # where graphs are stores

# now re-create the results directory
dir.create(file.path("TT/data"), showWarnings = FALSE)
dir.create(file.path("TT/graph"), showWarnings = FALSE)


## run all scripts
source("TT/1 set up.R") # setting up the environment
source("TT/2 collect data.R")   # cleaning and tokenizing the data
source("TT/3 load books.R") # create character list
source("TT/4 clean data.R") # create character list
source("TT/5 analysis.R") # create character list
rmarkdown::render("TT/paper.Rmd")
