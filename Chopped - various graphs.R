install.packages("tidytuesdayR")
install.packages("tidytext")
install.packages("widyr")
install.packages("ggraph")
install.packages("tidygraph")
library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(scales)
library(dplyr)
library(ggplot2)
library(glue)
library(widyr)
library(ggraph)
library(tidygraph)

tuesdata<-tidytuesdayR::tt_load('2020-08-25')

chopped <- tuesdata$chopped

chopped %>%
  ggplot(aes(episode_rating)) +
  geom_histogram()


chopped %>%
  arrange(episode_rating) %>%
  View()

chopped %>%
  filter(!is.na(episode_rating)) %>%
  ggplot(aes(series_episode, episode_rating)) +
  geom_line(alpha = .5, color = "gray") +
  geom_point(aes(color = factor(season))) +
  geom_text(aes(label = episode_name), hjust = 1,
            check_overlap = TRUE) +
  theme(legend.position = "none")
chopped %>%
  filter(!is.na(episode_rating)) %>%
  group_by(season) %>%
  summarize(n_episodes = n(),
            avg_rating = mean(episode_rating)) %>%
  ggplot(aes(season, avg_rating)) +
  geom_line() +
  geom_point(aes(size = n_episodes)) +
  theme(legend.position = "none") +
  labs(x = "Season",
       y = "Average Rating")

chopped %>%
  arrange(desc(episode_rating)) %>%
  head(25) %>%
  mutate(name = glue("{ season }.{season_episode} { episode_name }"),
         name = fct_reorder(name, episode_rating)) %>%
  ggplot(aes(episode_rating, name)) +
  geom_point()

ingredients <- chopped %>%
  select(season, season_episode, series_episode, episode_name,
         episode_rating, appetizer:dessert) %>%
  pivot_longer(cols = c(appetizer:dessert),
               names_to = "course",
               values_to = "ingredient") %>%
  separate_rows(ingredient, sep = ", ") %>%
  mutate(course = fct_relevel(course, c("appetizer", "entree")))

ingredients %>%
  count(course, ingredient, sort = TRUE) %>%
  filter(fct_lump(ingredient, 25, w = n) != "Other") %>%
  mutate(ingredient = fct_reorder(ingredient, n, sum),
         course = fct_rev(course)) %>%
  ggplot(aes(n, ingredient, fill = course)) +
  geom_col() +
  scale_fill_discrete(guide = guide_legend(reverse = TRUE)) +
  labs(x = "# of episodes",
       y = "",
       title = "Most common ingredients in Chopped",
       fill = "Course")

ingredients_filtered <- ingredients %>%
  add_count(ingredient) %>%
  filter(n >= 8)
ingredient_correlations <- ingredients_filtered %>%
  pairwise_cor(ingredient, series_episode, sort = TRUE)
ingredients_filtered %>%
  pairwise_count(ingredient, series_episode, sort = TRUE)

ingredient_correlations %>%
  head(75) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation)) +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE)

ingredients_filtered %>%
  unite(episode_course, series_episode, course) %>%
  pairwise_count(ingredient, episode_course, sort = TRUE)

early_late_ingredients <- ingredients_filtered %>%
  group_by(ingredient) %>%
  summarize(first_season = min(season),
            avg_season = mean(season),
            last_season = max(season),
            n_appearances = n()) %>%
  arrange(desc(avg_season)) %>%
  slice(c(1:6, tail(row_number())))


ingredients_filtered %>%
  semi_join(early_late_ingredients, by = "ingredient") %>%
  mutate(ingredient = fct_reorder(ingredient, season)) %>%
  ggplot(aes(season, ingredient)) +
  geom_boxplot(aes(fill = ingredient)) +
  scale_fill_manual(values = c(
    "passion fruit" = "darkorange",
    "shishito peppers" = "green3",
    "fava beans" = "palegreen",
    "bok choy" = "springgreen4",
    "rambutan" = "red2",
    "figs" = "plum4",
    "sour cream" = "lemonchiffon",
    "collard greens" = "darkolivegreen",
    "walnuts" = "chocolate3",
    "jumbo shrimp" = "pink2",
    "baby turnips" = "mistyrose",
    "jicama" = "burlywood"
  ))



