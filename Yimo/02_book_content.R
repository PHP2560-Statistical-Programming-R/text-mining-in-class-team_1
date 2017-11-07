# making sure harry potter version is uptodate
book_content <- function(){
  
  get_harry_potter_books <- function(){
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
    devtools::install_github("bradleyboehmke/harrypotter")
    library(harrypotter)
  }
}

book_content()