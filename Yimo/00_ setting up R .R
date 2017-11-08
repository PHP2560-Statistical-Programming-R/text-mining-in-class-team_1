# check that all required packages are installed 
source("Yimo/check_packages.R")
check.packages(c("devtools","rebus","tidytext", "dplyr", "stringr", "stringi", "ggplot2", 
                 "tidyverse","scales","wordcloud","igraph", "ggraph"))

#Create plot theme
create_theme = function(){
plot_theme = theme(text = element_text(color = "darkslategrey"),
                   legend.position = c("bottom"),
                   legend.text = element_text(color = "darkblue", face = "bold", hjust = 0.5),
                   legend.background = element_rect(fill = "azure"),
                   legend.title = element_text(face = "bold", color = "darkslategrey"),
                   axis.line = element_line(color = "lightblue3"),
                   axis.text = element_text(face = "bold", color = "darkslategrey"),
                   axis.title = element_text(color="darkslategrey"),
                   axis.ticks.y = element_blank(),
                   axis.ticks.x = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.major.x = element_blank(),
                   plot.background = element_rect(fill = "azure"), 
                   panel.background = element_rect("aliceblue"),
                   plot.title = element_text(face = "bold", hjust = 0.5, vjust = 2.5),
                   plot.subtitle = element_text(face = "italic", hjust = 0.5, vjust = 2.5),
                   axis.title.x = element_text(face = "bold.italic"),
                   axis.title.y = element_text(face = "bold.italic"),
                   strip.background = element_rect(fill = "lightblue3"),
                   strip.text = element_text(face = "bold", color = "darkslategrey"),
                   panel.spacing = unit(2, "lines"))
save(plot_theme, file = 'Yimo/data/plot_theme.rda')
}

create_theme()


