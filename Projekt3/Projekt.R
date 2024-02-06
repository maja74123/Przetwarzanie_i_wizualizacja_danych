library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(shiny)

prepare_dataset_for_project1 <- function(filename, columns_to_remove){
  df <- read.csv(filename)
  df <- df[ , !(names(df) %in% columns_to_remove)]
  df <- drop_na(df)
  return(df)
}

df <- prepare_dataset_for_project1('C:/Users/dawid/Desktop/projektdane.csv',
                                   c('floor', 'latitude',
                                     'longitude', 'ownership',
                                     'buildingMaterial', 'condition'))

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
                  'tenement' = 'Kamienica'
                  )
df <- df[df$type != 0, ]

wykres_poczatkowy <- ggplot(df, aes(x = squareMeters, y = price, color = type)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~city) +
  ggtitle('Cena mieszkań w zależności od powierzchni ') +
  labs(x = 'Powierzchnia [m\u00B2]', y = 'Cena', color = 'Typ budynku') +
  theme(legend.background = element_rect(color = 'black', fill = 'grey95'),
        legend.position = c(0.87, 0.12),
        panel.grid = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        strip.background = element_rect(color = 'black', fill = 'grey80'))




wykres_poczatkowy_plotly <- ggplotly(wykres_poczatkowy, width = 1000, height = 800) %>%
  config(scrollZoom = FALSE, displayModeBar = TRUE)


shinyApp(
  ui = fluidPage(
    tags$style(type = "text/css",
               ".facet-label { cursor: pointer; border: 1px solid black; background: grey80; }"),
    fluidRow(
      column(width = 9,
             plotlyOutput("wykres_poczatkowy")
      ),
      column(width = 3,
             selectInput("selected_city", "Wybierz miasto", choices = c("Wybierz wykres", unique(df$city))),
             actionButton("cofnij_button", "Cofnij")
      )
    ),
    plotlyOutput("wykres_wybranego_miasta")
  ),
  server = function(input, output, session) {
    wybrany_miasto <- reactiveVal(NULL)
    
    output$wykres_poczatkowy <- renderPlotly({
      if (is.null(wybrany_miasto())) {
        wykres_poczatkowy_plotly
      }
    })
    
    observeEvent(input$selected_city, {
      selected_city <- input$selected_city
      if (selected_city != "Wybierz wykres") {
        wybrany_miasto(selected_city)
      } else {
        wybrany_miasto(NULL)
      }
    })
    
    output$wykres_wybranego_miasta <- renderPlotly({
      req(!is.null(wybrany_miasto()))
      selected_data <- df[df$city == wybrany_miasto(), ]
      nowy_wykres <- ggplot(selected_data, aes(x = squareMeters, y = price, color = type)) +
        geom_point(alpha = 0.7) +
        ggtitle(paste('Cena mieszkań w zależności od powierzchni -', wybrany_miasto())) +
        labs(x = 'Powierzchnia [m\u00B2]', y = 'Cena', color = 'Typ budynku') +
        theme(legend.background = element_rect(color = 'black', fill = 'grey95'),
              legend.position = c(0.87, 0.12),
              panel.grid = element_blank(),
              panel.border = element_rect(color = 'black', fill = NA, size = 1)) +
        geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = 'white', color = NA)
      ggplotly(nowy_wykres, width = 1000, height = 800) %>%
        config(scrollZoom = FALSE, displayModeBar = TRUE)
    })
    
    observeEvent(input$cofnij_button, {
      wybrany_miasto(NULL)
    })
  }
)