
install_and_load_books <- function(){
  
  get_harry_potter_books <- function(){
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
    devtools::install_github("bradleyboehmke/harrypotter")
    library(harrypotter)
  }
}

install_and_load_books()