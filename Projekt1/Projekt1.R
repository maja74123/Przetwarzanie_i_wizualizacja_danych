library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(cowplot)
library(reshape2)

prepare_dataset_for_project1 <- function(filename, columns_to_remove){
  df <- read_csv(filename)
  # usuwamy kolumny, które nie są nam potrzebne lub zawierały zbyt dużo braków
  df <- df[ , !(names(df) %in% columns_to_remove)]
  # usuwamy wiersze, które miały brakujące wartości
  df <- drop_na(df)
  return(df)
}

df <- prepare_dataset_for_project1('apartments_pl_2023_08.csv',
                                   c('floor', 'latitude',
                                     'longitude', 'ownership',
                                     'buildingMaterial', 'condition'))
View(df)

df$city <- recode(df$city,
                  'bialystok' = 'Białystok', 'bydgoszcz' = 'Bydgoszcz',
                  'czestochowa' = 'Częstochowa','gdansk' = 'Gdańsk', 
                  'gdynia' = 'Gdynia', 'katowice' = 'Katowice',
                  'krakow' = 'Kraków', 'lodz' = 'Łódź',
                  'lublin' = 'Lublin', 'poznan' = 'Poznań',
                  'radom' = 'Radom', 'rzeszow' = 'Rzeszów',
                  'szczecin' = 'Szczecin', 'warszawa' = 'Warszawa',
                  'wroclaw' = 'Wrocław')

df$type <- recode(df$type,
                  'apartmentBuilding' = 'Apartamentowiec',
                  'blockOfFlats' = 'Blok mieszkalny',
                  'tenement' = 'Kamienica')

# Dane dotyczące liczby ludności pochodzą z
# https://stat.gov.pl/download/gfx/portalinformacyjny/pl/defaultaktualnosci/5468/7/20/1/powierzchnia_i_ludnosc_w_przekroju_terytorialnym_w_2023_roku_tablice.xlsx
data_population <- 'city population
Białystok 292600
Bydgoszcz 330038
Częstochowa 208282
Gdańsk 486345
Gdynia 242874
Katowice 280190
Kraków 803282
Lublin 331243
Łódź 658444
Poznań 541316
Radom 197848
Rzeszów 197181
Szczecin 391566
Warszawa 1861975
Wrocław 674079'

df_city_and_population <- as.data.frame(read_delim(data_population,
                                                   delim = ' ',
                                                   col_names = TRUE,
                                                   trim_ws = TRUE))

df_with_population <- merge(x = df, y = df_city_and_population,
                            by = 'city', all = TRUE)

# Tworzenie pliku zawierającego df z populacją
# write.csv(df_with_population,
# 'apartments_pl_2023_08_with_population.csv', row.names = TRUE)

# Wykresy punktowe pokazujące zależność między powierzchnią mieszkań
# a rokiem budowy i typem budynku (z podziałem na miasta)
ggplot(df, aes(x = buildYear, y = squareMeters, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~city) +
  ggtitle('Powierzchnia mieszkań w zależności od roku budowy i typu budynku') +
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

# Wykres kolumnowy pokazujący liczbę mieszkań posiadających windę
# w zależności od liczby pięter
buildings_with_elevator <- ggplot(as.data.frame(count_buildings_with_elevator),
                                  aes(x = floorCount,
                                      y = Frequency,
                                      fill = type)) +
  geom_col() +
  ggtitle('Liczba mieszkań posiadających windę w zależności od liczby pięter z podziałem na typ budynku') +
  labs(x = 'Liczba pięter',
       y = 'Liczba mieszkań',
       fill = 'Typ budynku') +
  scale_x_continuous(breaks = 1:20, limits=c(0, 21)) +
  geom_text(aes(label = ifelse(Frequency >= 39, Frequency, '')),
            size = 2.6, position = position_stack(vjust = 0.5)) +
  geom_text(inherit.aes = FALSE, data = total_count_with_elevator[1:20, ], 
            aes(x = floorCount, y = Frequency, label = Frequency),
            size = 2.9, vjust = -0.5)

# Wykres kolumnowy pokazujący liczbę mieszkań nieposiadających windy
# w zależności od liczby pięter
buildings_without_elevator <- ggplot(as.data.frame(count_buildings_without_elevator),
                                     aes(x = floorCount,
                                         y = Frequency,
                                         fill = type)) +
  geom_col() +
  ggtitle('Liczba mieszkań nieposiadających windy w zależności od liczby pięter z podziałem na typ budynku') +
  labs(x = 'Liczba pięter',
       y = 'Liczba mieszkań',
       fill = 'Typ budynku') +
  scale_x_continuous(breaks = 1:20, limits=c(0, 21)) +
  geom_text(aes(label = ifelse(Frequency >= 114, Frequency, '')),
            size = 2.6, position = position_stack(vjust = 0.5)) +
  geom_text(inherit.aes = FALSE, data = total_count_without_elevator[1:20, ], 
            aes(x = floorCount, y = Frequency, label = Frequency),
            size = 2.9, vjust = -0.5)

# Wyświetlanie dwóch wykresów jeden nad drugim -- wersja z legendą na wykresach
# (legenda nie zabiera dodatkowego miejsca)
plot_grid(buildings_with_elevator +
            theme(legend.background = element_rect(color = 'black',
                                                   fill = 'grey95'),
                                          legend.position = c(0.9, 0.8)),
          buildings_without_elevator +
            theme(legend.background = element_rect(color = 'black',
                                                   fill = 'grey95'),
                                             legend.position = c(0.9, 0.8)),
          ncol = 1)

# Przygotowanie danych do wykresów dotyczących ceny za metr kwadratowy
median_price_per_sqm <- as.data.frame(tapply(df$price / df$squareMeters,
                                             df$city,
                                             median))

first_interval <- paste0('(', floor(min(median_price_per_sqm)), ', ', 7000, ')')
last_interval <- paste0('[', 11500, ', ', ceiling(max(median_price_per_sqm)), ')')
# Skrajne wartości nie należą do przedziałów, ponieważ obliczono je jako podłogę i sufit.

median_range <- ifelse(median_price_per_sqm < 7000, first_interval,
                       ifelse(median_price_per_sqm >= 7000 & median_price_per_sqm < 9000, '[7000, 9000)',
                              ifelse(median_price_per_sqm >= 9000 & median_price_per_sqm < 11500, '[9000, 11500)',
                                     last_interval)))

df_city_and_range <- data.frame(city = rownames(median_range),
                                range = unname(median_range))

df_city_and_range$range <- factor(df_city_and_range$range,
                                  levels = c(first_interval,
                                             '[7000, 9000)',
                                             '[9000, 11500)',
                                             last_interval))

df_for_boxplots <- merge(x = df[ , c('city', 'price', 'squareMeters')],
                         y = df_city_and_range,
                         by = 'city', all = TRUE)

# Wykresy pudełkowe pokazujące cenę za metr kwadratowy z podziałem na miasta
ggplot(df_for_boxplots, aes(x = price / squareMeters,
                            y = city,
                            fill = range)) +
  geom_boxplot(alpha = 0.8,
               outlier.size = 2) +
  scale_x_continuous(breaks = 5000 * (1:6)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c( 'orchid',
                                'goldenrod1',
                                'royalblue1',
                                'olivedrab3')) +
  ggtitle('Cena za m\u00B2 z podziałem na miasta') +
  labs(x = 'Cena za m\u00B2 [zł]',
       y = 'Miasto',
       fill = 'Wartość mediany') +
  theme(legend.background = element_rect(color = 'black',
                                         fill = 'grey95'),
        legend.position = c(0.88,0.89))

# Histogramy pokazujące odległość od mieszkań do najbliższych POI
# Określenie zakresów osi x i y
xlim_range <- c(0, 5)
ylim_range <- c(0, 5500)

# Wektor z nazwami POI
distance_columns <- c('schoolDistance', 'clinicDistance',
                      'postOfficeDistance', 'kindergartenDistance',
                      'restaurantDistance', 'collegeDistance',
                      'pharmacyDistance')

# Utworzenie nowej tabeli zawierającej wybrane kolumny
selected_distances <- df[, distance_columns]

# Nazwy dla histogramów
hist_titles <- c('Szkoła', 'Przychodnia', 'Poczta',
                 'Przedszkole', 'Restauracja',
                 'Uniwersytet', 'Apteka')

# Tworzenie wielu histogramów na jednym wykresie
par(mfrow = c(3, 3), mar = c(5, 4, 2, 0), oma = c(0, 0, 4, 0))

# Tworzenie histogramów
for (i in 1:length(distance_columns)) {
  hist_result <- hist(selected_distances[[i]], 
                      main = paste(hist_titles[i]),
                      xlab = paste('km'),  
                      ylab = 'Liczba mieszkań',
                      col = 'lightblue',
                      xlim = xlim_range,
                      ylim = ylim_range)
  
  # Dodanie wartości z osi y nad słupkami
  text(hist_result$mids,
       hist_result$counts,
       labels = hist_result$counts,
       pos = 3,
       col = '#0000CD',
       cex = 0.8)
}

# Tytuł główny
mtext('Odległość od mieszkań do najbliższych POI (Points Of Interest)',
      outer = TRUE, line = 1.2, cex = 1.8)

# Wykres typu Lollipop pokazujący liczbę ofert na tysiąc mieszkańców w poszczególnych miastach
# Wyznaczanie liczby ofert przypadającej na tysiąc mieszkańców
df_offers_per_1000_people <- df_with_population %>%
  group_by(city) %>%
  summarise(total_offers = length(id),
            total_population = first(population)) %>%
  mutate(offers_per_1000_people = round((1000 * total_offers / total_population), 2))

# Wykres typu Lollipop
df_offers_per_1000_people %>%
  arrange(offers_per_1000_people) %>%
  mutate(city = factor(city, city)) %>%
  ggplot(aes(x = offers_per_1000_people, y = city)) +
  geom_segment(aes(x = 0, xend = offers_per_1000_people,
                   y = city, yend = city),
               color = 'grey', linewidth = 1.5) +
  geom_point(size = 4, color = '#69b3a2') +
  xlim(0, floor(max(df_offers_per_1000_people$offers_per_1000_people)) + 1) +
  geom_text(aes(x = offers_per_1000_people,
                y = city,
                label = offers_per_1000_people),
            hjust = -0.4, vjust = 0.3) + # vjust - odległość wartości od lollipopa (góra/dół)
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title.position = 'plot',
    plot.title = element_text(hjust = 0.6)) +
  xlab('Liczba ofert na 1000 mieszkańców') +
  ylab('Miasto') +
  ggtitle('Liczba ofert na 1000 mieszkańców w poszczególnych miastach')

# Mapa cieplna macierzy korelacji
correlation_matrix <- cor(df[sapply(df, is.numeric)])
correlation_matrix_half <- correlation_matrix
correlation_matrix_half[lower.tri(correlation_matrix_half)] <- NA

Polish_names <- c('Powierzchnia', 'Liczba pokoi',
                  'Liczba pięter', 'Rok budowy',
                  'Odległość od centrum', 'Liczba POI',
                  'Odległość od szkoły', 'Odległość od przychodni',
                  'Odległość od poczty', 'Odległość od przedszkola',
                  'Odległość od restauracji', 'Odległość od uczelni',
                  'Odległość od apteki', 'Cena')

row.names(correlation_matrix_half) <- Polish_names
colnames(correlation_matrix_half) <- Polish_names

correlation_matrix_half <- correlation_matrix_half[, colnames(correlation_matrix_half) != 'Rok budowy']
correlation_matrix_half <- correlation_matrix_half[row.names(correlation_matrix_half) != 'Rok budowy', ]

df_for_heatmap <- melt(correlation_matrix_half, na.rm = TRUE)
rounded_values <- round(df_for_heatmap$value, 2)

ggplot(df_for_heatmap, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white', 
                       midpoint = 0, limit = c(-1,1), space = 'Lab', 
                       name = 'Współczynnik korelacji Pearsona') +
  labs(title = 'Mapa cieplna macierzy korelacji') +
  geom_text(aes(label = rounded_values), color = 'black', size = 3.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.9, 0.15),
        legend.direction = 'horizontal') +
  guides(fill = guide_colorbar(barwidth = 13, barheight = 1,
                               title.position = 'top', title.hjust = 0.5)) +
  coord_fixed()
