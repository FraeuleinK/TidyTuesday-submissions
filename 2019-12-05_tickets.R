library(tidyverse)
library(osmdata)

tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")


streets <- getbb("Philadelphia USA") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()


small_streets <- getbb("Philadelphia USA") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service")) %>%
  osmdata_sf()

map <- ggplot(tickets) +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,          
          inherit.aes = FALSE,          
          color = "black",          
          size = .1,          
          alpha = .6) +  
  stat_density_2d( aes(x=lon, y=lat, fill = stat(level)), 
                   geom = "polygon", alpha = 0.8, bins = 50)+
  scale_fill_continuous(type = "viridis")+
  labs(title = "PHILADELPHIA PARKING TICKETS",
       subtitle = "Density map of parking tickets issued in 2017\n",
       caption = "Data by Open Data Philly",
       fill = "Number of Tickets") +
  coord_sf(xlim = c(-75.2803, -74.975),            
           ylim = c(39.8670, 40.075),           
           expand = FALSE) +
  theme_void()+
  theme(legend.position = "bottom",
        legend.title = element_text(vjust = 0.75),
        plot.title = element_text( size = 22),
        plot.subtitle = element_text( size = 14),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave("tickets_2.png", width = 15, height = 18, dpi = "retina", units = "cm")
