

## This function takes the packages that our program needs. 
## It makes sure you have them on your computer before proceeding.
source("allan/check_packages.R")
check_packages(c("gutenbergr","dplyr","stringr","tidytext",
                 "ggplot2", "wordcloud", "RColorBrewer","tidyr", "ggrepel",
                 "igraph","ggraph","XML","RCurl","rebus","topicmodels"),
               c("R.utils")
)

# this function returns ggplot theme definition ~ this will prevent code duplication
uniform_ggplot_theme <- function(base_size = 12, base_family = "sans"){
  theme_grey(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "navy"),
      legend.position = "bottom",
      legend.background = element_blank(),
      panel.margin = unit(.5, "lines"),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}
