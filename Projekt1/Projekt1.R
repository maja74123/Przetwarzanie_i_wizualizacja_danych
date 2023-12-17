library(readr)
library(tidyr)
library(ggplot2)

prepare_dataset_for_project1 <- function(filename, columns_to_remove){
  df <- read_csv(filename)
  # usuwamy kolumny, które nie są nam potrzebne lub zawierały zbyt dużo braków
  df <- df[ , !(names(df) %in% columns_to_remove)]
  # usuwamy wiersze, które miały brakujące wartości
  df <- drop_na(df)
  return(df)
}

df <- prepare_dataset_for_project1('apartments_pl_2023_08.csv',
                                   c('id', 'floor', 'latitude', 'longitude',
                                     'ownership','buildingMaterial', 'condition'))
View(df)

ggplot(df, aes(x = buildYear, y = squareMeters, color = type)) + # nazwy miast i typów budynków jeszcze zmienię
  geom_point(alpha = 0.5) +
  facet_wrap(~city) +
  ggtitle("Powierzchnia mieszkań w zależności od roku budowy i typu budynku")+
  labs(x = 'Rok budowy',
       y = 'Powierzchnia [m\u00B2]',
       color = 'Typ budynku') +
  theme(legend.background = element_rect(color = 'black', fill = 'grey95'),
        legend.position = c(0.87, 0.12))
