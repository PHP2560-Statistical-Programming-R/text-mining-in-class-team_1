
seriesinfo <- read.csv("https://docs.google.com/spreadsheets/d/1dhxblR1Vl7PbVP_mNhwEa3_lfUWiF__xSODLq1W83CA/export?format=csv&id=1dhxblR1Vl7PbVP_mNhwEa3_lfUWiF__xSODLq1W83CA&gid=0")
popularity<-subset(seriesinfo,Author=="Rowling, J.K." & Title != "Tales of Beedle the Bard,The")
popularity$Volume.Sales <- c("4,475,152", "4,200,654", "4,179,479", "3,583,215", "3,484,047", "3,377,906", "2,950,264", "4,103,445")
as.numeric(popularity$Volume.Sales)
colnames(popularity)[colnames(popularity) == 'Title'] <- 'book'


hp_by_sales <- series%>%
  # join nrc to get sentiment value
  inner_join(get_sentiments("nrc"), by = "word") %>%
  # join nrc to get sentiment score
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(book)%>%
  summarise(count=sum(score)) %>%
  mutate(perc_sentiment=(count/sum(count))*2) %>%
  arrange(desc(perc_sentiment))%>%
  inner_join(popularity, by = "book")%>%
  mutate(sales=as.numeric(gsub(",","",Volume.Sales)))



df<- data.frame(hp_by_sales)
# plot the graph
ggplot(df)  + 
  geom_bar(aes(x=book, y=sales,  fill=book),stat="identity", col = "lightblue")+
  geom_line(aes(x=book, y=perc_sentiment*max(df$sales)),group=1)+
  geom_point(aes(label=perc_sentiment, x=book, y=perc_sentiment*max(df$sales)), colour="brown")+
  geom_text(aes(label=sales, x=book, y=0.97*sales), colour="black")+
  scale_y_continuous(sec.axis = sec_axis(~./max(df$sales)))+
  labs(x = "Book", y = "Sales/Popularity")+
  theme_minimal()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
