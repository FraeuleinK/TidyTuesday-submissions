library(tidyverse)

acs_data<- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/table_3.csv")

income_data <- acs_data %>% 
  slice(27:36) %>% 
  rename("income" = age)

income.long<-gather(data=income_data, key='mode', value='count',
                    gather_cols=c(bike_percent, walk_percent))

income.long$count<-as.numeric(income.long$count)

position <- rev(as.character(income_data$income))


ggplot(data=income.long, aes(x=income, y=count/100, fill=factor(mode, levels=c("bike_percent", "walk_percent" )))) +
  geom_bar(stat="identity")+
  ggtitle(label = "Walking and Biking to Work in the United States by Income",
          subtitle = "Data from American Community Survey 2008-2012")+
  scale_fill_manual(values=c( "orange", "royalblue4"),
                    name = "Mode",
                    labels = c("Bike", "Walk"))+
  scale_x_discrete(limits = position)+
  scale_y_continuous(labels = scales::percent)+
  xlab("Income") + ylab("Percentage")+
  theme_classic()+
  theme(axis.title.y = element_blank())+
  coord_flip()
