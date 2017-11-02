check_packages = function(InstallAndLoad, onlyInstall=c())
{
  for(name in InstallAndLoad)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org",dependencies=TRUE)
    
    library(name, character.only=TRUE)
  }
  
  # only install but do not load
  for(name in onlyInstall)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org",dependencies=TRUE)
    
  }
  
}
