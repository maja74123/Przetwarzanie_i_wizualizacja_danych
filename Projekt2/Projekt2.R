library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)

prepare_dataset_for_project2 <- function(filename, columns_to_remove){
  df <- read_csv(filename)
  # usuwamy kolumny, które nie są nam potrzebne lub zawierały zbyt dużo braków
  df <- df[ , !(names(df) %in% columns_to_remove)]
  # usuwamy wiersze, które miały brakujące wartości
  df <- drop_na(df)
  return(df)
}

df <- prepare_dataset_for_project2('apartments_pl_2023_08.csv',
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

# Interaktywne wykresy pudełkowe pokazujące cenę za metr kwadratowy z podziałem na miasta
plot_ly(data = df_for_boxplots,
        x = ~price / squareMeters,
        y = ~city,
        fillcolor = ~range,
        type = "box",
        hoverinfo = "x") %>%
  layout(title = "Cena za m\u00B2 z podziałem na miasta",
         xaxis = list(title = "Cena za m\u00B2 [zł]",
                      tickformat = "digits"),
         yaxis = list(title = list(text = "Miasto",
                                   standoff = 0),
                      autorange = 'reversed'),
         legend = list(title = list(text='Wartość mediany'),
                       bordercolor = 'grey',
                       borderwidth = 1,
                       x = 0.785,
                       y = 0.98),
         colorway = c("orchid", "goldenrod1", "royalblue1", "olivedrab3"))
