library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(cowplot)

prepare_dataset_for_project1 <- function(filename, columns_to_remove){
  df <- read_csv(filename)
  # usuwamy kolumny, które nie są nam potrzebne lub zawierały zbyt dużo braków
  df <- df[ , !(names(df) %in% columns_to_remove)]
  # usuwamy wiersze, które miały brakujące wartości
  df <- drop_na(df)
  return(df)
}

df <- prepare_dataset_for_project1('apartments_pl_2023_08.csv',
                                   c('floor', 'latitude', 'longitude',
                                     'ownership','buildingMaterial', 'condition'))
View(df)

df$city <- recode(df$city,
                  "bialystok" = "Białystok", "bydgoszcz" = "Bydgoszcz", "czestochowa" = "Częstochowa",
                  "gdansk" = "Gdańsk", "gdynia" = "Gdynia", "katowice" = "Katowice",
                  "krakow" = "Kraków", "lodz" = "Łódź", "lublin" = "Lublin",
                  "poznan" = "Poznań", "radom" = "Radom", "rzeszow" = "Rzeszów",
                  "szczecin" = "Szczecin", "warszawa" = "Warszawa", "wroclaw" = "Wrocław")

df$type <- recode(df$type,
                  "apartmentBuilding" = "Apartamentowiec",
                  "blockOfFlats" = "Blok mieszkalny",
                  "tenement" = "Kamienica")

ggplot(df, aes(x = buildYear, y = squareMeters, color = type)) + # nazwy miast i typów budynków jeszcze zmienię
  geom_point(alpha = 0.5) +
  facet_wrap(~city) +
  ggtitle("Powierzchnia mieszkań w zależności od roku budowy i typu budynku")+
  labs(x = 'Rok budowy',
       y = 'Powierzchnia [m\u00B2]',
       color = 'Typ budynku') +
  theme(legend.background = element_rect(color = 'black', fill = 'grey95'),
        legend.position = c(0.87, 0.12))

count_buildings_with_elevator <- df %>% 
  group_by(floorCount, type) %>% 
  summarise('Frequency' = sum(hasElevator == 'yes'))

count_buildings_without_elevator <- df %>%
  group_by(floorCount, type) %>%
  summarise('Frequency' = sum(hasElevator == 'no'))

buildings_with_elevator <- ggplot(as.data.frame(count_buildings_with_elevator), aes(x = floorCount, y = Frequency, fill = type)) +
  geom_col() +
  ggtitle("Liczba mieszkań posiadających windę w zależności od liczby pięter z podziałem na typ budynku") +
  labs(x = 'Liczba pięter',
       y = 'Liczba mieszkań',
       fill = 'Typ budynku') +
  xlim(0,20) +
  geom_text(aes(label = ifelse(Frequency >= 63, Frequency, "")), size = 3, position = position_stack(vjust = 0.5))

buildings_without_elevator <- ggplot(as.data.frame(count_buildings_without_elevator), aes(x = floorCount, y = Frequency, fill = type)) +
  geom_col() +
  ggtitle("Liczba mieszkań nieposiadających windy w zależności od liczby pięter z podziałem na typ budynku") +
  labs(x = 'Liczba pięter',
       y = 'Liczba mieszkań',
       fill = 'Typ budynku') +
  xlim(0,20) +
  geom_text(aes(label = ifelse(Frequency >= 130, Frequency, "")), size = 3, position = position_stack(vjust = 0.5))

plot_grid(buildings_with_elevator + theme(legend.position = "none"),
          buildings_without_elevator + theme(legend.background = element_rect(color = 'black', fill = 'grey95'),
                                             legend.position = c(0.9, 0.8)),
          ncol = 1) # TODO czy zostawić wspólną legendę, czy każdy wykres ma mieć swoją
