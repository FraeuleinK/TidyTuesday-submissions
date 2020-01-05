library(lubridate)
library(tidyverse)
library(showtext)

schwanen <- readr::read_tsv("ParkrunGiessen.txt")
nidda <- readr::read_tsv("Parkrun_Nidda.txt")


parkrun <- rbind(schwanen, nidda)

parkrun$Datum <- as.Date(parkrun$Datum, "%d.%m.%Y")

parkrun<- parkrun %>% mutate(Standort = replace(Standort, Standort == "Nidda parkrun", "Nidda parkrun (Frankfurt)" ))
parkrun<- parkrun %>% mutate(Standort = replace(Standort, Standort == "Schwanenteich Parkrun", "Schwanenteich parkrun\n(Giessen)" ))

font_add_google("Raleway")

showtext_auto()

ggplot(parkrun, aes(x = Datum, y = Teilnehmer, group = Standort, color = Standort))+
  geom_point(size = 1)+
  geom_line(size = 0.8)+
  labs(title = "Participants of Central-Hessian Parkrun Locations in 2019",
       subtitle = "How did participant numbers of these two German parkrun locations evolve last year?",
       caption= "Data by parkrun.com",
       x = "", 
       y = "Participants")+
  scale_x_date(limits = c(as.Date("2019-01-01"),as.Date("2019-12-28")),
               breaks = c(as.Date("2019-01-01"),as.Date("2019-02-01"),as.Date("2019-03-01"),
                          as.Date("2019-04-01"),as.Date("2019-05-01"),as.Date("2019-06-01"),
                          as.Date("2019-07-01"), as.Date("2019-08-01"), as.Date("2019-09-01"),
                          as.Date("2019-10-01"), as.Date("2019-11-01"), as.Date("2019-12-01")),
               date_labels = "%b")+
  geom_curve(aes(x = as.Date("2019-07-28"), y = 186, xend = as.Date("2019-08-20"), yend = 189), 
             colour = "white", 
             size=0.5, 
             curvature = -0.2,
             arrow = arrow(length = unit(0.02, "npc")))+
  scale_color_manual(values = c("#F59709", "#00D4AF"))+
  theme_classic()+
  theme(plot.background = element_rect(fill = "#3F3E7A"),
        panel.background = element_rect(fill = "#3F3E7A"),
        legend.background = element_rect(fill = "#3F3E7A"),
        plot.title = element_text(size=40, face = "bold"),
        plot.subtitle = element_text (size = 34),
        plot.caption = element_text(size = 24),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        axis.text = element_text(color= "white", size = 22),
        axis.line = element_line(color="white"),
        axis.ticks = element_line(color="white"),
        axis.title = element_text(size = 24),
        text = element_text(family = "Raleway", color="white"),
        legend.title = element_blank(),
        legend.position = c( 0.2, 0.5),
        legend.text= element_text(lineheight = 0.25, size = 26))+
  annotate("text", x = as.Date("2019-04-22"), y = 178, label= "50th Schwanenteich parkrun:\nWinchester visits Giessen",  
           color = "white", family = "Raleway", size = 9.2, lineheight = 0.25, hjust = 0)

ggsave("2020-01-05_parkrun.png", width = 16, height = 10, dpi = "retina", units = "cm")
