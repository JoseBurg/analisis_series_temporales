library(fpp3)
library(readxl)

# Cargar datos
# Selecionar los datos de la serie de tiempo
imae <- read_excel("Recursos/datos/imae_ready.xlsx") |> 
  mutate(fecha = as.Date(fecha)) |> 
  select(fecha, year, mes,indice_original)

# Ver la estructura de los datos
imae |> 
  glimpse()

# Funciones del tidyverse para manipular datos

