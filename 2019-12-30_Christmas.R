library(tidyverse)
library(lubridate)
library(showtext)


christmas_songs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_songs.csv")

christmas_songs$weekid<-as.Date(christmas_songs$weekid, "%m/%d/%Y")
christmas_songs$song <- str_to_title(christmas_songs$song)

christmas_songs<- christmas_songs %>% mutate(performer = replace(performer, performer == "New Kids On The Block", "NKOTB" ))

popular <- christmas_songs %>% group_by(songid) %>% 
  mutate ( popularity = (100-peak_position)*weeks_on_chart) %>% 
  summarise(popularity = last(popularity)) %>%
  arrange(-popularity) %>%
  slice(1:5)

pop <- popular$songid            

songs <- christmas_songs %>% 
  filter(songid %in% pop) %>% 
  mutate(weeknr=week(weekid)) %>%
  group_by(weeknr, songid) %>%
  summarise(week_position = mean(week_position),
            song = last(song),
            performer = last(performer)) %>%
  unite("sp", song, performer,sep = " by ",remove = FALSE)



songs$weeknr <- as.character(songs$weeknr)


xaxis <- c("45", "46", "47", "48", "49", "50", "51", "52", "53", "1", "2", "3", "4","5")
colors <- c("#165B33", "forestgreen", "#F8B229", "#EA4630", "#BB2528")

font_add_google("Lobster")

showtext_auto()

#devtools::install_github("clauswilke/ggtext")
library(ggtext)

p <- ggplot(songs, aes(x=weeknr, y=week_position, group=sp, color=sp)) +
  geom_line() +
  geom_point()+
  scale_x_discrete(limits=xaxis)+
  scale_y_reverse(breaks = c(1, 25, 50, 75, 100), limits = c(100, 1))+
  scale_color_manual(values = colors)+
  geom_vline(xintercept = "52", color = "gray30")+
  labs(title = "Chart Position of Christmas Songs by Weeks of a Year",
       subtitle = "<b> </b> <b> </b> <b> </b> <b> </b> <b> </b> <b 
       style='color:#F8B229' >The</b> <br> <b> </b> <b> </b> <b> </b> mean weekly 
       <br> <b> </b> <b> </b> position of the five <br> most popular christmas 
       <br> <b> </b> <b> </b> <b> </b> <b> </b>  <b style='color:#662200'>songs</b>",
       caption = "Data by data.world",
       x = "Week of the Year",
       y = "Position")+
  geom_curve(aes(x = 7, y = 90, xend = 7.8, yend = 85), 
                                           colour = "black", 
                                           size=0.5, 
                                           curvature = -0.2,
                                           arrow = arrow(length = unit(0.02, "npc")))+
  geom_text(aes(x = 6.8, y = 94, label = "Christmas Week"), 
             hjust = 0.5, 
             vjust = 0, 
            color = "#165B33",
            lineheight = 0.8,
             size = 10,
            family = "Lobster")+
  theme_classic()+
  theme(legend.position = c(0.80,0.2),
        legend.text = element_text(size=20),
        legend.title = element_blank(),
        legend.key.height = unit(0.5, "line"),
        plot.title.position = "plot",
        plot.title = element_text(family = "Lobster", size = 48, color = "#BB2528" ),
        plot.caption = element_text(size = 20, family = "Lobster", color = "#165B33"),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30, color = "#165B33", family = "Lobster"),
        plot.subtitle = element_markdown(size = 30, color = "#165B33", family = "Lobster",
                                     lineheight = 0.2, hjust = 0)) 
p


ggsave("2019-12-30-Christmas.png", width = 16, height = 10, dpi = "retina", units = "cm")  



