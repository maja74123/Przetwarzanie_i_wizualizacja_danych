library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)

prepare_dataset_for_project2 <- function(filename, columns_to_remove){
  df <- read_csv(filename)
  # usuwamy kolumny, które nie są nam potrzebne lub zawierały zbyt dużo braków
  df <- df[ , !(names(df) %in% columns_to_remove)]
  # usuwamy wiersze, które miały brakujące wartości
  df <- drop_na(df)
  return(df)
}

df <- prepare_dataset_for_project2('apartments_pl_2023_08.csv',
                                   c('floor', 'ownership',
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
                  'apartmentBuilding' = 'apartamentowiec',
                  'blockOfFlats' = 'blok mieszkalny',
                  'tenement' = 'kamienica')

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
        type = 'box',
        hoverinfo = 'x') %>%
  layout(title = 'Cena za m\u00B2 z podziałem na miasta',
         xaxis = list(title = 'Cena za m\u00B2 [zł]',
                      tickformat = 'digits'),
         yaxis = list(title = list(text = 'Miasto',
                                   standoff = 0),
                      autorange = 'reversed'),
         legend = list(title = list(text='Wartość mediany'),
                       bordercolor = 'grey',
                       borderwidth = 1,
                       x = 0.785,
                       y = 0.98),
         colorway = c('orchid', 'goldenrod1', 'royalblue1', 'olivedrab3'))

# Przygotowanie kolumny z udogodnieniami
amenities_columns <- c('hasElevator', 'hasParkingSpace', 'hasBalcony', 'hasSecurity', 'hasStorageRoom')
amenities_names <- c('winda', 'parking', 'balkon', 'ochrona', 'komórka lokatorska')

prepare_amenities_strings <- function(amenities_names, boolean_selector) {
  amenities_strings <- c()
  for(i in 1:nrow(boolean_selector)) {
    if(any(boolean_selector[i, ])) {
      amenities_strings <- c(amenities_strings, paste(amenities_names[boolean_selector[i, ]], collapse = ', '))
    } else {
      amenities_strings <- c(amenities_strings, 'brak')
    }
  }
  return(amenities_strings)
}

df$amenities <- prepare_amenities_strings(amenities_names, df[, amenities_columns] == 'yes')

# Przygotowanie różnokolorowych znaczników
# sposób został zaczerpnięty z:
# https://stackoverflow.com/questions/32940617/change-color-of-leaflet-marker
icons_colors <- iconList('apartamentowiec' = makeIcon('icons/marker_icon_red.png', iconWidth = 24, iconHeight =32),
                       'kamienica' = makeIcon('icons/marker_icon_blue.png', iconWidth = 24, iconHeight =32),
                       'blok mieszkalny' = makeIcon('icons/marker_icon_green.png', iconWidth = 24, iconHeight =32))

# Przygotowanie legendy (kolor znacznika)
# sposób został zaczerpnięty z:
# https://stackoverflow.com/questions/37862467/leaflet-legend-for-custom-markers-in-r
html_legend <- "<img src='https://raw.githubusercontent.com/maja74123/Przetwarzanie_i_wizualizacja_danych/main/Projekt2/icons/marker_icon_red.png'>apartamentowiec<br/>
<img src='https://raw.githubusercontent.com/maja74123/Przetwarzanie_i_wizualizacja_danych/main/Projekt2/icons/marker_icon_green.png'>blok mieszkalny<br/>
<img src='https://raw.githubusercontent.com/maja74123/Przetwarzanie_i_wizualizacja_danych/main/Projekt2/icons/marker_icon_blue.png'>kamienica"

# Interaktywna mapa Polski z zaznaczonymi mieszkaniami i wyskakującymi informacjami o nich
# (znaczniki-pinezki, kolory zależą od typu budynku)
leaflet(data = df) %>% 
  addTiles() %>%
  addMarkers(~longitude, ~latitude,
             icon = ~icons_colors[type],
             popup = ~paste('Miasto:', city,
                            '<br>Typ budynku:', type,
                            '<br>Cena:', price / 1000, 'tys. zł',
                            '<br>Powierzchnia:', squareMeters, 'm\u00B2',
                            '<br>Cena za m\u00B2:', round((price / squareMeters), 2), 'zł',
                            '<br>Rok budowy:', buildYear,
                            '<br>Piętro:', floorCount,
                            '<br>Liczba pokoi:', rooms,
                            '<br>Odległość od centrum:', centreDistance, 'km',
                            '<br>Udogodnienia:', amenities),
             popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE),
             label = ~paste('Cena za m\u00B2:', round((price / squareMeters), 2), 'zł'),
             clusterOptions = markerClusterOptions()) %>% 
  addControl(html = html_legend, position = "bottomright")

# Interaktywna mapa Polski z zaznaczonymi mieszkaniami i wyskakującymi informacjami o nich
# (okrągłe znaczniki -- kolory zależą od typu budynku)
leaflet(data = df) %>% 
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude,
                   popup = ~paste('Miasto:', city,
                                  '<br>Typ budynku:', type,
                                  '<br>Cena:', price / 1000, 'tys. zł',
                                  '<br>Powierzchnia:', squareMeters, 'm\u00B2',
                                  '<br>Cena za m\u00B2:', round((price / squareMeters), 2), 'zł',
                                  '<br>Rok budowy:', buildYear,
                                  '<br>Piętro:', floorCount,
                                  '<br>Liczba pokoi:', rooms,
                                  '<br>Odległość od centrum:', centreDistance, 'km',
                                  '<br>Udogodnienia:', amenities),
                   label = ~paste('Cena za m\u00B2:', round((price / squareMeters), 2), 'zł'),
                   color = ~ ifelse(type == 'kamienica', 'blue',
                                    ifelse(type == 'apartamentowiec', 'red', 'green')),
                   clusterOptions = markerClusterOptions())
