# TODO
# round latitude and longitude
# 

library(shiny)
library(shinythemes)
library(DT)
library(data.table)
library(ggplot2)
library(plotly)
library(leaflet)
library(tidyr)
library(dplyr)
library(readr)


amenities_columns <- c('hasElevator', 'hasParkingSpace', 'hasBalcony',
                       'hasSecurity', 'hasStorageRoom')
amenities_names <- c('winda', 'parking', 'balkon',
                     'ochrona', 'komórka lokatorska')


prepare_amenities_strings <- function(amenities_names, boolean_selector) {
  amenities_strings <- c()
  for(i in 1:nrow(boolean_selector)) {
    if(any(boolean_selector[i, ])) {
      amenities_strings <- c(amenities_strings,
                             paste(amenities_names[boolean_selector[i, ]], collapse = ', '))
    } else {
      amenities_strings <- c(amenities_strings, 'brak')
    }
  }
  return(amenities_strings)
}


# Przygotowanie zbioru danych do projektu 3
prepare_dataset_for_project3 <- function(filename, columns_to_remove=c('floor', 'ownership', 'buildingMaterial', 'condition')) {
  df <- read_csv(filename)
  
  # usuwamy kolumny, które nie są nam potrzebne lub zawierały zbyt dużo braków
  df <- df[ , !(names(df) %in% columns_to_remove)]
  
  # usuwamy wiersze, które miały brakujące wartości
  df <- drop_na(df)
  
  # dodajemy kolumnę z udogodnieniami
  df$amenities <- prepare_amenities_strings(amenities_names,
                                            df[, amenities_columns] == 'yes')
  return(df)
}

# Wczytywanie zbiorów danych dla poszczególnych miesięcy
df_august <- prepare_dataset_for_project3('apartments_pl_2023_08_with_population.csv')
df_september <- prepare_dataset_for_project3('apartments_pl_2023_09_with_population.csv')
df_october <- prepare_dataset_for_project3('apartments_pl_2023_10_with_population.csv')
df_november <- prepare_dataset_for_project3('apartments_pl_2023_11_with_population.csv')
df_december <- prepare_dataset_for_project3('apartments_pl_2023_12_with_population.csv')
df_january <- prepare_dataset_for_project3('apartments_pl_2024_01_with_population.csv')
df <- df_august

# Przygotowanie różnokolorowych znaczników (do mapy leaflet)
# sposób został zaczerpnięty z:
# https://stackoverflow.com/questions/32940617/change-color-of-leaflet-marker
icons_colors <- iconList('apartamentowiec' = makeIcon('icons/marker_icon_red.png',
                                                      iconWidth = 24,
                                                      iconHeight =32),
                         'kamienica' = makeIcon('icons/marker_icon_blue.png',
                                                iconWidth = 24,
                                                iconHeight =32),
                         'blok mieszkalny' = makeIcon('icons/marker_icon_orange.png',
                                                      iconWidth = 24,
                                                      iconHeight =32))

# niebieski znacznik został pobrany z:
# https://unpkg.com/browse/leaflet@1.3.1/dist/images/marker-icon.png
# pozostałe kolory znaczników przygotowano w programie GIMP

# Przygotowanie legendy (kolor znacznika)
# sposób został zaczerpnięty z:
# https://stackoverflow.com/questions/37862467/leaflet-legend-for-custom-markers-in-r
html_legend <- '<img src="https://raw.githubusercontent.com/maja74123/Przetwarzanie_i_wizualizacja_danych/main/Projekt2/icons/marker_icon_red.png">apartamentowiec<br/>
<img src="https://raw.githubusercontent.com/maja74123/Przetwarzanie_i_wizualizacja_danych/main/Projekt2/icons/marker_icon_orange.png">blok mieszkalny<br/>
<img src="https://raw.githubusercontent.com/maja74123/Przetwarzanie_i_wizualizacja_danych/main/Projekt2/icons/marker_icon_blue.png">kamienica'


### ui ###

comparison_plot_features <- c(
  "city",
  "type",
  "squareMeters",
  "rooms",
  "floorCount",
  "buildYear",
  "latitude",
  "longitude",
  "centreDistance",
  "poiCount",
  "schoolDistance",
  "clinicDistance",
  "postOfficeDistance",
  "kindergartenDistance",
  "restaurantDistance",
  "collegeDistance",
  "pharmacyDistance",
  "hasParkingSpace",
  "hasBalcony",
  "hasElevator",
  "hasSecurity",
  "hasStorageRoom",
  "price",
  "population"
)

datasets_months_options <- c("Sierpień 2023", "Wrzesień 2023", "Październik 2023",
                             "Listopad 2023", "Grudzień 2023", "Styczeń 2024")


project_description_string <- "
  Ten dashboard pokazuje informacje
  o ofertach mieszkaniowych z 15 największych
  (pod względem liczby ludności) miast w Polsce.
  Zbiór danych pochodzi z platformy Kaggle:
  https://www.kaggle.com/datasets/krzysztofjamroz/apartment-prices-in-poland
"



#########################################################

ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage("Oferty mieszkaniowe",
             tabPanel("O projekcie",
                      sidebarPanel(helpText(tags$h2("O projekcie"),
                                            HTML("<p>Korzystamy ze zbioru danych
                                        <a href='https://www.kaggle.com/datasets/krzysztofjamroz/apartment-prices-in-poland'>
                                        Apartment Prices in Poland </a>
                                        pochodzącego z platformy
                                        <a href='https://www.kaggle.com/'> Kaggle</a>.
                                        Zawiera on oferty sprzedaży mieszkań z 15 największych miast w Polsce
                                        (pod względem liczby ludności) oraz najważniejsze informacje na ich temat.
                                        <br> Ze zbioru usunęliśmy kolumny zawierające dużo braków.
                                        <br> Dołączyliśmy dane dotyczące populacji miast. Pochodzą one ze strony internetowej
                                        <a href='https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/powierzchnia-i-ludnosc-w-przekroju-terytorialnym-w-2023-roku,7,20.html'>
                                        Głównego Urzędu Statystycznego</a>.
                                        <br> Kod do tej aplikacji jest dostępny na
                                        <a href='https://github.com/maja74123/Przetwarzanie_i_wizualizacja_danych/tree/main/Projekt3'>
                                        GitHub</a>.
                                        <br> Wybraliśmy różne atrybuty i pokazaliśmy różne zależności na wykresach i mapach,
                                        zarówno statycznych, jak i interaktywnych.</p>")
                      ))    
             ),
             
             tabPanel("Mapa interaktywna",
                      tabsetPanel(selectInput("leaflet_dataset", label = "Wybierz zbiór danych", datasets_months_options)),
                      mainPanel(leafletOutput("interactive_map"))
             ),
             
             tabPanel("Porównanie",
                      sidebarPanel(
                        selectInput("comparison_dataset", label = "Wybierz zbiór danych", datasets_months_options),
                        selectInput("xaxis", label = "Oś x", comparison_plot_features),
                        selectInput("yaxis", label = "Oś y", comparison_plot_features)
                      ),
                      mainPanel(plotOutput("compare_plot"))
             ),
             
             tabPanel("Zbiór danych",
                      tabsetPanel(selectInput("table_dataset", label = "Wybierz zbiór danych", datasets_months_options)),
                      mainPanel(dataTableOutput("dataset_table"))
             ),
  )
)

#########################################################

### server ###

server <- function(input, output) {
  
  output$compare_plot <- renderPlot({
    
    df <- switch(input$comparison_dataset,
                 "Sierpień 2023" = df_august,
                 "Wrzesień 2023" = df_september,
                 "Październik 2023" = df_october,
                 "Listopad 2023" = df_november,
                 "Grudzień 2023" = df_december,
                 "Styczeń 2024" = df_january)
    
    ggplot(df, mapping = aes_string(x = input$xaxis, y = input$yaxis)) +
      geom_point(size = 3)
  }, height = 800)
  
  output$interactive_map <- renderLeaflet({
    df <- switch(input$leaflet_dataset,
                 "Sierpień 2023" = df_august,
                 "Wrzesień 2023" = df_september,
                 "Październik 2023" = df_october,
                 "Listopad 2023" = df_november,
                 "Grudzień 2023" = df_december,
                 "Styczeń 2024" = df_january)
    
    leaflet(data = df) %>% 
      addTiles() %>%
      addMarkers(~longitude, ~latitude,
                 icon = ~icons_colors[type],
                 popup = ~paste('Miasto:', city,
                                '<br>Typ budynku:', type,
                                '<br>Cena:', ifelse(price < 10^6, price / 1000, price / (10^6)),
                                ifelse(price < 10^6, 'tys. zł', 'mln zł'),
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
      addControl(html = html_legend, position = 'bottomright')
  })
  
  output$dataset_table <- DT::renderDataTable({
    df <- switch(input$table_dataset,
                 "Sierpień 2023" = df_august,
                 "Wrzesień 2023" = df_september,
                 "Październik 2023" = df_october,
                 "Listopad 2023" = df_november,
                 "Grudzień 2023" = df_december,
                 "Styczeń 2024" = df_january)
    
    df_for_table <- df[ , !(names(df) %in% c("...1", "id", "schoolDistance", "clinicDistance","postOfficeDistance",
                                             "kindergartenDistance", "restaurantDistance", "pharmacyDistance", "hasParkingSpace",
                                             "hasBalcony", "hasElevator", "hasSecurity", "hasStorageRoom"))]
    
    datatable(df_for_table, colnames = c("Miasto", "Typ budynku", "Powierzchnia [m\u00B2]", "Liczba pokoi", "Liczba pięter", "Rok budowy",
                                         "Szerokość geograficzna", "Długość gegraficzna", "Odległość od centrum [km]", "Liczba POI",
                                         "Odległość od uczelni [km]", "Cena [zł]", "Populacja", "Udogodnienia"))
  })
  
  
  
}

#########################################################

### Uruchamianie aplikacji ###

shinyApp(ui = ui, server = server)
