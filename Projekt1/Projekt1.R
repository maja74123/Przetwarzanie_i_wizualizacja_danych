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
