
## run all scripts
source("text_mining_Wei_Wang/01_get_packages.R")  ## get packages
source("text_mining_Wei_Wang/02_shape_data.R")   ## shape data
source("text_mining_Wei_Wang/03_clean_data.R")   ## clean data
source("text_mining_Wei_Wang/04_question_#1.R")  ## create graph and answer
source("text_mining_Wei_Wang/05_question_#2.R")  ## create graph and answer
source("text_mining_Wei_Wang/06_question_#3.R")  ## create graph and answer
source("text_mining_Wei_Wang/07_question_#4.R")  ## create graph and answer
source("text_mining_Wei_Wang/08_question_#5.R")  ## create graph and answer
source("text_mining_Wei_Wang/09_question_#6.R")  ## create graph and answer
source("text_mining_Wei_Wang/10_question_#7.R")  ## create graph and answer
source("text_mining_Wei_Wang/11_question_#8.R")  ## create graph and answer
source("text_mining_Wei_Wang/12_question_#9.R")  ## create graph and answer
rmarkdown::render("text_mining_Wei_Wang/Harry_Potter.Rmd", output_format = "html_document")