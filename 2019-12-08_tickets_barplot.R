library(tidyverse)
library(ggfittext)
library(showtext)

tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")


violations <- tickets %>%
  count(violation_desc) %>%
  top_n(10, n) %>%
  arrange(desc(n))%>%
  mutate(violation_desc = str_to_title(violation_desc))

# nice labels for x axis, if there is an easier way to do this, please let me know :) 
violation_label <- violations$violation_desc 
violation_label[1] <- "Meter Expired CC"
violation_label[5] <- "Stop Prohibited CC"
violation_label[8] <- "Parking Prohibited CC"
violation_label[9] <- "Over Time Limit CC"
violation_label[10] <- "Passenger Loading Zone"

# I have no idea what I am doing here
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

options("device" = "windows")

###
ggplot(violations, aes(x=reorder(violation_desc, n), y=n)) + 
  geom_bar(stat = "identity", fill = "gray70", width = 0.75) +
  geom_bar_text(place = "right", size = 30, family = "Roboto Condensed") +
  labs ( title = "Top 10 Parking Violations in Philadelphia",
         caption = "Data by Open Data Philly")+
  scale_x_discrete(labels = rev(violation_label))+
  scale_y_continuous(expand = c(0.01, 0))+
  coord_flip()+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 32),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 52),
        plot.caption = element_text(size = 32),
        plot.title.position = "plot",
        text = element_text(family = "Roboto Condensed"))

ggsave("tickets_barplot_4.png", width = 20, height = 12, dpi = "retina", units = "cm")  

options("device" = "RStudioGD")
