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
library(reshape2)


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
df_february <- prepare_dataset_for_project3('apartments_pl_2024_02_with_population.csv')
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


prepare_median_ranges_for_boxplots <- function(df) {
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
  
  return(df_for_boxplots)
}

df_august_for_boxplots <- prepare_median_ranges_for_boxplots(df_august)



### ui ###

comparison_plot_features <- c(
  "Miasto" = "city",
  "Typ budynku" = "type",
  "Powierzchnia"= "squareMeters",
  "Liczba pokoi" = "rooms",
  "Liczba pięter" = "floorCount",
  "Rok budowy" = "buildYear",
  "Długość geograficzna" = "latitude",
  "Szerokość geograficzna" = "longitude",
  "Odległość od centrum" = "centreDistance",
  "Liczba POI" = "poiCount",
  "Odległość od szkoły" = "schoolDistance",
  "Odległość od przychodni" = "clinicDistance",
  "Odległość od poczty" = "postOfficeDistance",
  "Odległość od przedszkola" = "kindergartenDistance",
  "Odległość od restauracji" = "restaurantDistance",
  "Odległość od uczelni" = "collegeDistance",
  "Odległość od apteki" = "pharmacyDistance",
  "Parking" = "hasParkingSpace",
  "Balkon" = "hasBalcony",
  "Winda" = "hasElevator",
  "Ochrona" = "hasSecurity",
  "Komórka lokatorska" = "hasStorageRoom",
  "Cena" = "price",
  "Populacja" = "population"
)

datasets_months_options <- c("Sierpień 2023", "Wrzesień 2023", "Październik 2023",
                             "Listopad 2023", "Grudzień 2023", "Styczeń 2024", "Luty 2024")


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
             tabPanel("O projekcie", icon = icon("circle-info"),
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
             
             tabPanel("Mapa interaktywna", icon = icon("location-dot"),
                      tabsetPanel(selectInput("leaflet_dataset", label = "Wybierz zbiór danych", datasets_months_options)),
                      mainPanel(leafletOutput("interactive_map", height='80vh'))
             ),
             
             tabPanel("Porównanie",
                      sidebarPanel(
                        selectInput("comparison_dataset", label = "Wybierz zbiór danych", datasets_months_options),
                        selectInput("xaxis", label = "Oś x", comparison_plot_features),
                        selectInput("yaxis", label = "Oś y", comparison_plot_features)
                      ),
                      mainPanel(plotOutput("compare_plot"))
             ),
             
             tabPanel("Wykresy pudełkowe", icon = icon("magnifying-glass-chart"),
                      tabsetPanel(
                        selectInput("boxplot_dataset", label = "Wybierz zbiór danych", datasets_months_options)
                      ),
                      mainPanel(plotlyOutput("boxplot"))
             ),
             
             tabPanel("Mapa cieplna",
                      # sidebarPanel(
                      #   selectInput("boxplot_dataset", label = "Wybierz zbiór danych", datasets_months_options)
                      # ),
                      mainPanel(plotOutput("heatmap"))
             ),
             
             tabPanel("Zbiór danych", icon = icon("table"),
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
                 "Styczeń 2024" = df_january,
                 "Luty 2024" = df_february)
    
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
                 "Styczeń 2024" = df_january,
                 "Luty 2024" = df_february)
    
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
                 "Styczeń 2024" = df_january,
                 "Luty 2024" = df_february)
    
    df_for_table <- df[ , !(names(df) %in% c("...1", "id", "schoolDistance", "clinicDistance","postOfficeDistance",
                                             "kindergartenDistance", "restaurantDistance", "pharmacyDistance", "hasParkingSpace",
                                             "hasBalcony", "hasElevator", "hasSecurity", "hasStorageRoom"))]
    
    df_for_table$latitude <- round(df_for_table$latitude, 2)
    df_for_table$longitude <- round(df_for_table$longitude, 2)
    
    datatable(df_for_table, colnames = c("Miasto", "Typ budynku", "Powierzchnia [m\u00B2]", "Liczba pokoi", "Liczba pięter", "Rok budowy",
                                         "Szerokość geograficzna", "Długość gegraficzna", "Odległość od centrum [km]", "Liczba POI",
                                         "Odległość od uczelni [km]", "Cena [zł]", "Populacja", "Udogodnienia"))
  })
  
  
  
  output$boxplot <- renderPlotly({
    
    df_for_boxplots <- switch(input$boxplot_dataset,
                 "Sierpień 2023" = prepare_median_ranges_for_boxplots(df_august),
                 "Wrzesień 2023" = prepare_median_ranges_for_boxplots(df_september),
                 "Październik 2023" = prepare_median_ranges_for_boxplots(df_october),
                 "Listopad 2023" = prepare_median_ranges_for_boxplots(df_november),
                 "Grudzień 2023" = prepare_median_ranges_for_boxplots(df_december),
                 "Styczeń 2024" = prepare_median_ranges_for_boxplots(df_january),
                 "Luty 2024" = prepare_median_ranges_for_boxplots(df_february)
    )
    
    # Interaktywne wykresy pudełkowe pokazujące cenę za metr kwadratowy z podziałem na miasta
    plot_ly(data = df_for_boxplots,
            x = ~price / squareMeters,
            y = ~city,
            fillcolor = ~range,
            type = 'box',
            hoverinfo = 'x') %>%
      layout(title = list(text = 'Cena za m\u00B2 z podziałem na miasta', y = 0.99),
             xaxis = list(title = 'Cena za m\u00B2 [zł]',
                          tickformat = 'digits'),
             yaxis = list(title = list(text = 'Miasto',
                                       standoff = 0),
                          autorange = 'reversed'),
             legend = list(title = list(text='Wartość mediany'),
                           bordercolor = 'grey',
                           borderwidth = 1,
                           x = 0.8,
                           y = 0.35),
             colorway = c('orchid', 'goldenrod1', 'royalblue1', 'olivedrab3'))
  })
  
  output$heatmap <- renderPlot({
    df_for_heatmap <- df[ , !(names(df) %in% c("...1", "population", "latitude", "longitude"))]
    
    
    # Mapa cieplna macierzy korelacji
    correlation_matrix <- cor(df_for_heatmap[sapply(df_for_heatmap, is.numeric)])
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
      geom_text(aes(label = rounded_values), color = 'black', size = 5) +
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
  },
  height = 850, width = 850)
  
}

#########################################################

### Uruchamianie aplikacji ###

shinyApp(ui = ui, server = server)
