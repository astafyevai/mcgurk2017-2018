library(tidyverse)
library(ggridges)
df <- read_tsv("experiment_2017.csv")

df %>% 
  count(sex, order, experiment) %>% 
  full_join(df) %>% 
  ggplot(aes(x = age, y = sex, fill = sex, label = n)) +
  geom_density_ridges(show.legend = FALSE)+
  theme_bw()+
  facet_grid(experiment~order, scales = "free")+
  geom_text(aes(x = 8))+
  labs(x = "", y = "")

map <- read_csv("https://raw.githubusercontent.com/astafyevai/McGurk-effect/master/data/map.data.csv")

library(lingtypology)
map.feature(map$languages,
            latitude = map$lat,
            longitude = map$long,
            control = F,
            features = map$color,
            color = c("black", "white"),
            tile = "OpenStreetMap.BlackAndWhite",
            legend = FALSE)
