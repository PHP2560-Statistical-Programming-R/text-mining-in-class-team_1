
 load("final_project/data/harrypotter_clean_tokens.Rda")
 load("final_project/data/harrypotter_characters.rda")
harrypoterclean <- harrypotter_clean_tokens

characters_listjoin <- harrypotter_characters %>%
  mutate(word = tolower(harrypotter_characters$FirstName))

  
graph.nourQ <- function(){
harrypoterclean %>%
  inner_join( characters_listjoin, by = "word" ) %>%
  count(Title, word, sort =  TRUE) %>%
  group_by(Title) %>%
  # take the top ten characters mentioned in each book 
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(paste(word, Title, sep = "__"), n)) %>%
  ggplot(aes(word, n, fill = Title)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ Title, nrow = 2, scales = "free") +
  coord_flip() +
  theme_minimal() + # start with a minimal them and add what we need 
  theme( text = element_text(color = "gray20"),
         axis.text = element_text(face = "italic"), 
         axis.text.x = element_text(size = 7),
         axis.ticks.x = element_blank(),
         axis.line = element_line(color = "gray50", size = 0.5), 
         axis.line.y   = element_blank(), 
         panel.grid.major.y = element_blank(),
         panel.grid.minor.x = element_line(color = "gray50", size = .5),
         panel.grid.major.x = element_line(color = "gray50", size = .5)
  )
  ggsave("final_project/graph/nourQ.png")
}

graph.nourQ()
