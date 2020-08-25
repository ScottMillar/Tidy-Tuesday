install.packages("tidytuesdayR")
install.packages("tvthemes")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tvthemes)

tuesdata<-tidytuesdayR::tt_load('2020-08-11')

avatar <- tuesdata$avatar
scenes <- tuesdata$scene_description

import_avatar()
library(extrafont)
loadfonts()

avatar_data <- avatar %>%
  mutate(
    book_chapt = case_when(
      book == "Water" ~ chapter_num,
      book == "Earth" ~ chapter_num + 21,
      book == "Fire" ~ chapter_num + 42
    ),
    imdb_rating = if_else(book_chapt == 20, 9.7, imdb_rating),
    allcount = ifelse(character != "Iroh", str_count(character_words, "\\btea\\b"), 0),
    irohcount = ifelse(character == "Iroh", str_count(character_words, "\\btea\\b"), 0),
  ) %>%
  group_by(book_chapt) %>% 
  summarise(tea_count = sum(allcount, na.rm=T), iroh_count = sum(irohcount)) %>% 
  unique() 

ggplot(avatar_data) +
  geom_smooth(aes(x=book_chapt, y=tea_count), fill = 'seagreen1', colour = 'darkgreen') +
  geom_smooth(aes(x=book_chapt, y=iroh_count), colour = 'orangered1', fill = 'orangered1') +
  annotate(
    geom = "text",
    x = 15,
    y = 2,
    label = "No surprise that\nIroh's tea shop\nis the talk of the town",
    family = "Slayer",
    size = 2, fontface = 2 
  ) +
  annotate(
    "curve",
    xend = 35,
    yend = 2,
    x = 20,
    y = 2,
    curvature = -.2,
    size = 1,
    arrow = arrow(type = "closed", length = unit(.5, "lines")),
    colour = "black"
  ) +
  labs(
    x = "Episode Number",
    y = "Number of times Tea is Mentioned",
    title = "Tea Flows With Iroh",
    subtitle = "Who would guess that tea is mentioned by others after Iroh"
  ) +
  theme_avatar() +
  theme(
    axis.line.x = element_blank(),
    legend.box = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "#d9cfb5",
      linetype = 3,
      size = 1
    ),
    plot.title = element_text(hjust = .5, size = 25, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 10),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.caption = element_text(
      hjust = .5,
      size = 8,
      family = "sans",
      color = "#a89567"
    )
  )
  
