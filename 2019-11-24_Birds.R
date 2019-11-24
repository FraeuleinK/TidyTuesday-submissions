library(tidyverse)


nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")


bird_sum <- nz_bird %>% group_by(date,bird_breed) %>%
  filter(!is.na(bird_breed)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(bird_breed) %>%
  mutate(cum_sum = cumsum(n)) %>%
  arrange(bird_breed)


top5 <- bird_sum %>% group_by(bird_breed) %>%
  summarize (end_vote = last(cum_sum)) %>%
  top_n(5, end_vote) %>%
  arrange(desc(end_vote))

best_birds <- top5$bird_breed

birds <- bird_sum %>% filter(bird_breed %in% best_birds)

ggplot(data = birds, aes(x = date, y = cum_sum, 
                         group = bird_breed, color = bird_breed))+
  geom_line(size = 1.2)+
  theme_minimal()+
  labs(title = "New Zealand Bird of the Year 2019",
       subtitle = "Cumulative Number of Votes for the first five Birds\n",
       caption = "Data from  New Zealand Forest and Bird Orginization")+
  ylab ("Cumulative Number of Votes")+ 
  xlab("")+
  geom_text(data = top5, aes(x = as.Date("2019-11-10")+0.2, 
                             y = end_vote, label = bird_breed),
                             hjust = 0, size = 4)+
  scale_x_date(limits = c(as.Date("2019-10-28"),as.Date("2019-11-13")),
               breaks = c(as.Date("2019-10-28"),as.Date("2019-10-30"),
                            as.Date("2019-11-01"), as.Date("2019-11-03"),
                            as.Date("2019-11-05"), as.Date("2019-11-07"),
                            as.Date("2019-11-09")),
               date_labels = "%b %d")+
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(vjust = 1),
        plot.subtitle = element_text(vjust = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())+
  scale_color_manual(values = c("peachpuff4", "gray27", "indianred4",
                                "forestgreen", "gold3"))

ggsave("birds2.png", width = 18, height = 10, dpi = "retina", units = "cm")
