check.packages = function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org",dependencies=TRUE)
    
    library(name, character.only=TRUE)
  }
}
