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

# Wykresy punktowe pokazujące zależność między powierzchnią mieszkań a rokiem budowy i typem budynku (z podziałem na miasta)
ggplot(df, aes(x = buildYear, y = squareMeters, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~city) +
  ggtitle("Powierzchnia mieszkań w zależności od roku budowy i typu budynku")+
  labs(x = 'Rok budowy',
       y = 'Powierzchnia [m\u00B2]',
       color = 'Typ budynku') +
  theme(legend.background = element_rect(color = 'black', fill = 'grey95'),
        legend.position = c(0.87, 0.12))

# Przygotowanie danych do wykresów dotyczących obecności windy
count_buildings_with_elevator <- df %>% 
  group_by(floorCount, type) %>% 
  summarise('Frequency' = sum(hasElevator == 'yes'))

total_count_with_elevator <- df %>% 
  group_by(floorCount) %>% 
  summarise('Frequency' = sum(hasElevator == 'yes'))

count_buildings_without_elevator <- df %>%
  group_by(floorCount, type) %>%
  summarise('Frequency' = sum(hasElevator == 'no'))

total_count_without_elevator <- df %>%
  group_by(floorCount) %>%
  summarise('Frequency' = sum(hasElevator == 'no'))

# Wykres kolumnowy pokazujący liczbę mieszkań posiadających windę w zależności od liczby pięter
buildings_with_elevator <- ggplot(as.data.frame(count_buildings_with_elevator), aes(x = floorCount, y = Frequency, fill = type)) +
  geom_col() +
  ggtitle('Liczba mieszkań posiadających windę w zależności od liczby pięter z podziałem na typ budynku') +
  labs(x = 'Liczba pięter',
       y = 'Liczba mieszkań',
       fill = 'Typ budynku') +
  scale_x_continuous(breaks = 1:20, limits=c(0, 21)) +
  geom_text(aes(label = ifelse(Frequency >= 58, Frequency, '')), size = 2.6, position = position_stack(vjust = 0.5)) +
  geom_text(inherit.aes = FALSE, data = total_count_with_elevator, 
            aes(x = floorCount, y = Frequency, label = Frequency), size = 2.9, vjust = -0.5)

# Wykres kolumnowy pokazujący liczbę mieszkań nieposiadających windy w zależności od liczby pięter
buildings_without_elevator <- ggplot(as.data.frame(count_buildings_without_elevator), aes(x = floorCount, y = Frequency, fill = type)) +
  geom_col() +
  ggtitle('Liczba mieszkań nieposiadających windy w zależności od liczby pięter z podziałem na typ budynku') +
  labs(x = 'Liczba pięter',
       y = 'Liczba mieszkań',
       fill = 'Typ budynku') +
  scale_x_continuous(breaks = 1:20, limits=c(0, 21)) +
  geom_text(aes(label = ifelse(Frequency >= 114, Frequency, '')), size = 2.6, position = position_stack(vjust = 0.5)) +
  geom_text(inherit.aes = FALSE, data = total_count_without_elevator, 
            aes(x = floorCount, y = Frequency, label = Frequency), size = 2.9, vjust = -0.5)

# Wyświetlanie dwóch wykresów jeden nad drugim -- wersja z legendą na wykresach (legenda nie zabiera dodatkowego miejsca)
plot_grid(buildings_with_elevator + theme(legend.background = element_rect(color = 'black', fill = 'grey95'),
                                          legend.position = c(0.9, 0.8)),
          buildings_without_elevator + theme(legend.background = element_rect(color = 'black', fill = 'grey95'),
                                             legend.position = c(0.9, 0.8)),
          ncol = 1)

# Przygotowanie danych do wykresów dotyczących ceny za metr kwadratowy
median_price_per_sqm <- as.data.frame(tapply(df$price / df$squareMeters, df$city, median))

first_interval <- paste0("[", floor(min(median_price_per_sqm)), ", ", 7000, ")")
last_interval <- paste0("[", 11500, ", ", ceiling(max(median_price_per_sqm)), ")")

median_range <- ifelse(median_price_per_sqm < 7000, first_interval,
                       ifelse(median_price_per_sqm >= 7000 & median_price_per_sqm < 9000, '[7000, 9000)',
                              ifelse(median_price_per_sqm >= 9000 & median_price_per_sqm < 11500, '[9000, 11500)',
                                     last_interval)))

df_city_and_range <- data.frame(city = rownames(median_range), range = unname(median_range))

df_city_and_range$range <- factor(df_city_and_range$range, levels = c(first_interval, '[7000, 9000)', '[9000, 11500)', last_interval))

df_for_boxplots <- merge(x = df[ , c('city', 'price', 'squareMeters')], y = df_city_and_range, by = 'city', all = TRUE)

# Wykresy pudełkowe pokazujące cenę za metr kwadratowy z podziałem na miasta
ggplot(df_for_boxplots, aes(x = price / squareMeters, y = city, fill = range)) +
  geom_boxplot(alpha = 0.8,
               outlier.size = 2) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c( 'orchid', 'goldenrod1' ,'royalblue1', 'olivedrab3')) +
  ggtitle('Cena za m\u00B2 z podziałem na miasta') +
  labs(x = 'Cena za m\u00B2 [zł]',
       y = 'Miasto',
       fill = 'Wartość mediany') +
  theme(legend.background = element_rect(color = 'black', fill = 'grey95'),
        legend.position = c(0.88,0.89))
