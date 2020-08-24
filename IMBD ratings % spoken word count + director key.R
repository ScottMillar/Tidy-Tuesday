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
    word_count = sapply(strsplit(character_words, " "), length),
    book_chapt = case_when(
      book == "Water" ~ chapter_num,
      book == "Earth" ~ chapter_num + 21,
      book == "Fire" ~ chapter_num + 42
    ),
    imdb_rating = if_else(book_chapt == 20, 9.7, imdb_rating)
  ) %>%
  group_by(book_chapt, director, imdb_rating) %>% 
  summarise(count = sum(word_count)) %>%  
  unique()

avatar_data %>%
  ggplot(aes(x = count, y = imdb_rating, color = director)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Spoken Word Count",
    y = "IMDb Rating",
    title = "Speak Less, Bend More",
    subtitle = "Imdb Rating of Avatar as the Number of Spoken Words Increases"
  ) +
  scale_y_continuous(
    limits = c(7, 10),
    breaks = seq(7, 10, 0.5),
    sec.axis = dup_axis(name = NULL)
  )+
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
    ),
  ) +
  scale_color_manual(values = c(
    "Anthony Lioi" = "#8B5B45",
    "Ethan Spaulding" = "#00916E",
    "Joaquim Dos Santos" = "#EEB05A",
    "Michael Dante DiMartino" = "#25351C",
    "Dave Filoni" = "#a10000",
    "Giancarlo Volpe" = "#0047ab",
    "Lauren MacMullan" = "#A2CAED"
  ))


