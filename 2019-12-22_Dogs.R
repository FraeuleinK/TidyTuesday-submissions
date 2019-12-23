library(tidyverse)
library(ggfittext)
library(showtext)
library(ggimage)

dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

dogs <- dog_descriptions %>% filter(!is.na(color_tertiary) ) %>%
  count(breed_primary) %>%
  top_n(10, n) %>%
  arrange(desc(n))
 
boxer <- "C:\\Users\\Anna\\Documents\\Verschiedenes\\TidyTuesday\\Dogs\\boxer.png"
bullterrier <- "C:\\Users\\Anna\\Documents\\Verschiedenes\\TidyTuesday\\Dogs\\bullterrier.png"
germanshep <- "C:\\Users\\Anna\\Documents\\Verschiedenes\\TidyTuesday\\Dogs\\german-sheperd.png"
beagle <- "C:\\Users\\Anna\\Documents\\Verschiedenes\\TidyTuesday\\Dogs\\beagle.png"
terrier <- "C:\\Users\\Anna\\Documents\\Verschiedenes\\TidyTuesday\\Dogs\\jack-russell-terrier.png"
retriever <- "C:\\Users\\Anna\\Documents\\Verschiedenes\\TidyTuesday\\Dogs\\plain-dog.png"
mixed <- "C:\\Users\\Anna\\Documents\\Verschiedenes\\TidyTuesday\\Dogs\\plain-dog.png"
chihuahua <- "C:\\Users\\Anna\\Documents\\Verschiedenes\\TidyTuesday\\Dogs\\chihuahua.png"



image <- c( bullterrier, retriever, mixed, chihuahua, germanshep, boxer, germanshep,beagle, mixed,
            terrier)
size <- c(0.05, 0.04,0.04,0.035,0.05,0.05,0.05,0.04,0.04,0.04)


font_add_google("Dosis")

showtext_auto()



ggplot(dogs, aes(x = reorder(breed_primary, n), y = n)) + 
  geom_bar(stat = "identity", fill = "darkred", width = 0.75) +
  geom_bar_text(place = "right", size = 34, color = "white") +
  geom_image(aes(x = breed_primary, y = -3.8, image = image), size = size)+
  labs ( title = "Most colorful dog breeds available for adoption",
         subtitle = "Primary breed of the dogs available for adoption on Petfinder.com that have three different coat colors.\nThe icons do not quite match the dog breeds but I tried.",
         caption = "Data by Petfinder.com via The Pudding | Icons made by Freepik from www.flaticon.com")+
  scale_y_continuous(expand = c(0.025, 0))+
  coord_flip()+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 34),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 60, face = "bold"),
        plot.subtitle = element_text(size = 34, lineheight = 0.3),
        plot.caption = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(family = "Dosis"),
        panel.background = element_rect(fill = "gray97"),
        plot.background = element_rect(fill = "gray97"))

ggsave("2019-12-22_dogs.png", width = 20, height = 12, dpi = "retina", units = "cm")  


