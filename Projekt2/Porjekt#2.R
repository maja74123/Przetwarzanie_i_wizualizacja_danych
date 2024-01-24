library(tidyr)
library(plotly)
library(dplyr)


przygotuj_dane_do_projektu1 <- function(plik, kolumny_do_usuniecia){
  df <- read.csv(plik)
  df <- df[, !(names(df) %in% kolumny_do_usuniecia)]
  df <- drop_na(df)
  return(df)
}

df <- przygotuj_dane_do_projektu1('C:/Users/dawid/Desktop/projektdane.csv',
                                  c('floor', 'latitude',
                                    'longitude', 'ownership',
                                    'buildingMaterial', 'condition'))

df$miasto <- recode(df$city,
                    'bialystok' = 'Białystok', 'bydgoszcz' = 'Bydgoszcz',
                    'czestochowa' = 'Częstochowa', 'gdansk' = 'Gdańsk', 
                    'gdynia' = 'Gdynia', 'katowice' = 'Katowice',
                    'krakow' = 'Kraków', 'lodz' = 'Łódź',
                    'lublin' = 'Lublin', 'poznan' = 'Poznań',
                    'radom' = 'Radom', 'rzeszow' = 'Rzeszów',
                    'szczecin' = 'Szczecin', 'warszawa' = 'Warszawa',
                    'wroclaw' = 'Wrocław')
df <- df %>% mutate(type = ifelse(is.na(type) | type == "", "Unknown", type))
df$type <- recode(df$type,
                  'trace 0'= ' Unknown',
                 'apartmentBuilding' = 'Apartamentowiec',
                 'blockOfFlats' = 'Blok mieszkalny',
                 'tenement' = 'Kamienica')

data_wykres <- data.frame(df,
  miasto = df$miasto,
  cena = df$price,
  metraz = df$squareMeters,
  type = df$type
)

#Tutaj możemy sobie wybrać miasto lub jak usuniemy tą linijkę to wykres będzie dla wszystkich miast
data_wykres_poznan <- data_wykres %>% filter(miasto == "Poznań")



wykres <- plot_ly(data_wykres_poznan, x = ~metraz, y = ~cena, color= ~type,
                  text = ~paste("Cena: ", cena, "<br>Metraż: ", metraz,"<br>Miasto: ",miasto,"<br>Typ: ",type),
                  hoverinfo = "text", mode = "markers", type = "scatter", 
                  marker = list(size = 10))%>%

layout(
  title = "Zależność ceny od metrażu",
  xaxis = list(title = "Metry kwadratowe"),
  yaxis = list(title = "Cena")
)
                  
                 
wykres

























