## 0. Подготовка

Ставим пакеты:
```{r}
library(stringr)
library(tidyverse)
```
Загружаем данные:
```{r}
df <- read.csv("McGurk.csv")
```

## 1. Визуализация

Проанализируем данные по времени реакции участников на стимул:

```{r}
#library(ggplot2)
#library(magrittr)
#library(dplyr)

data = read.csv(file='time-bxplt.csv')
data$stimul <- as.factor(data$stimul)
data$time <- as.numeric(as.character(data$time))
ggplot(data, aes(stimul, time))+
  geom_boxplot(aes(group = cut_width(stimul, 1)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

data$time
```
График содержит диаграммы размаха для каждого отдельно взятого стимула (9 (звонкие одинарные слоги) + 9 (звонкие дублированные слоги) + 9 (глухие одинарные слоги) + 9 (гулхие дублированные слоги) = 36). По оси абсцисс (stimul) подписаны названия стимулов, а по оси ординат (time) разброс времени реакции участников. Горизонтальной чертой на каждой отдельной диаграмме показано среднее время реакции на конкретный стимул. Черные точки означают выбросы.
Как видно из графика, распределение ответов в основном не превыщает 10 секунд. Сравнив время реакции на одинарные и дублированные стимулы, можно заметить, что в целом на распознание дублированных слогов уходит больше времени, однако стоит учитывать, что респонденты записывали ответы на листок, и время реакции могло увеличиться по этой причине. Вместе с тем, PsychoPy работает нестабильно, поэтому некоторые неожиданные результаты могут быть связаны с техническими трудностями. По графику нетрудно заметить, что стимулы, над которыми больше всего думали носители - это ak_vp_doubled, ap_at, ap_vp, ak_vt_doubled.

Посмотрим на результаты тех респондентов, которые смотрели видеозаписи со звонкими стимулами.

```{r}
df <- read.csv("McGurk.csv")
df[df$voiced == "voiced",] %>%
  count(doubled, audio, video, response, sort = TRUE) %>% 
  ggplot(aes(audio, video, size = n))+
  geom_point(aes(size = n), alpha=0.8, color="pink", show.legend =FALSE) +
  geom_text(aes(label = n), color="black", size = 4) +
  scale_size(range = c(3,20)) +
  facet_grid(doubled~response)+
  theme_bw()+
  labs(x = "audio stimulus",
       y = "video stimulus",
       title = "Voiced results grouped by double/single")
```
ОПИСАНИЕ

Посмотрим на результаты тех респондентов, которые смотрели видеозаписи с глухими стимулами.

```{r}
df <- read.csv("McGurk.csv")
df[df$voiced == "unvoiced",] %>%
  count(doubled, audio, video, response, sort = TRUE) %>% 
  ggplot(aes(audio, video, size = n))+
  geom_point(aes(size = n), alpha=0.8, color="pink", show.legend =FALSE) +
  geom_text(aes(label = n), color="black", size = 4) +
  scale_size(range = c(3,20)) +
  facet_grid(doubled~response)+
  theme_bw()+
  labs(x = "audio stimulus",
       y = "video stimulus",
       title = "Unvoiced results grouped by double/single")
```
График разделен на 4 столбца (ответы респондентов) и 2 строки (одинарные и двойные повторения стимулов). Из графика видно, что  распределение ответов, видимо, связано с аудио стимулом, а не с видео стимулом. Исключением является столбец p в эксперименте с одинарными стимулами. ДОПИСАТЬ
В ответы столбца "-" попали ....

Графики сильно отличаются друг от друга. С помощью тестов проверим, являются ли эти отличия статистически значимы:

```{r}
my_df <- read.csv("McGurk.csv")
my_df[my_df$voiced == "unvoiced" & my_df$doubled == "doubled",] %>%
  na.omit() %>% 
  count(audio,video,response, knowledge.about.McGurk) %>% 
  spread(key = knowledge.about.McGurk, value = n) %>% 
  mutate(no = ifelse(is.na(no), as.integer(0), no),
         yes = ifelse(is.na(yes), as.integer(0), yes),
         type = paste(audio,video, response)) %>% 
  ungroup() %>% 
  select(type, no, yes) ->
  my_df_UD_mcgurk_test
fisher.test(my_df_UD_mcgurk_test[,-1],simulate.p.value=TRUE,B=1e5)
```


# Зависит ли количество повторяющихся ответов на одни и те же сочетания (audio, video) от пола?
Все в совокупности: p-value 0.3722
В группе "unvoiced doubled":p-value 0.9845
В группе "voiced doubled":p-value 1
В группе "unvoiced single":p-value 0.7018
В группе "voiced single":p-value 0.6458

# Зависит ли количество повторяющихся ответов на одни и те же сочетания (audio, video) от одинарного/двойного?
В группе voiced: p-value 1
В группе unvoiced: p-value 0.5287
Во всей совокупности: p-value 0.9572 

# Зависит ли количество повторяющихся ответов на одни и те же сочетания (audio, video) от звонкости?
В группе double: p-value 0.9883
В группе single: p-value 0.1598
Во всей совокупности: p-value 0.2065

# Зависит ли количество повторяющихся ответов на одни и те же сочетания (audio, video) от знания, что такое эффект МакГурка?
Во всей совокупности: p-value 0.5017
В группе "voiced doubled": p-value 1
В группе "voiced single": p-value 0.9795
В группе "unvoiced single": p-value 0.6537
В группе "unvoiced doubled": p-value 0.9984
