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

# Crear columnas

imae_variacion <- imae |> 
  mutate(
    variacion_mensual = indice_original - lag(indice_original),
    variacion_anual = indice_original - lag(indice_original, 12)
  )

# ver las primeras observaciones
head(imae_variacion)

# Gráfico de linea

imae_variacion |> 
  ggplot(aes(x = fecha, y = indice_original)) +
  geom_line() +
  labs(title = "IMAE",
       subtitle = "Índice Mensual de Actividad Económica",
       x = "Fecha",
       y = "Índice") +
  theme_minimal()


# Gráfico de variación mensual
imae_variacion |> 
  ggplot(aes(x = fecha, y = variacion_mensual)) +
  geom_line() +
  labs(title = "Variación mensual",
       subtitle = "IMAE",
       x = "Fecha",
       y = "Variación") +
  theme_minimal()

# Gráfico de variación anual
imae_variacion |> 
  ggplot(aes(x = fecha, y = variacion_anual)) +
  geom_line() +
  labs(title = "Variación anual",
       subtitle = "IMAE",
       x = "Fecha",
       y = "Variación") +
  theme_minimal()

# Descomposición de la serie
imae_descomposicion <- imae |> 
  select(fecha, indice_original) |>
  mutate(Month = yearmonth(fecha)) |> 
  select(-c(fecha)) |>
  as_tsibble(index = Month) |>
  relocate(Month) |>
  model(STL(indice_original))


# Gráfico de la serie descompuesta
components(imae_descomposicion) |> 
  autoplot()


# Serie original y tendencia
components(imae_descomposicion) |>
  as_tsibble() |>
  autoplot(indice_original, colour="gray", size = 0.7)+
  geom_line(aes(y=trend), colour = "blue", size = 1) +
  labs(
    y = "IMAE",
    title = "Índice Mensual de Actividad Económica",
    x = "Fecha") +
  theme_minimal()

# Efecto covid-19
components(imae_descomposicion) |>
  as_tsibble() |>
  autoplot(indice_original, colour="gray", size = 0.7)+
  geom_line(aes(y=trend), colour = "blue", size = 1) +
  labs(
    y = "IMAE",
    title = "Índice Mensual de Actividad Económica",
    x = "Fecha") +
  annotate(
    "rect",
    xmin = as.Date("2019-11-01"),
    xmax = as.Date("2020-12-01"), 
    ymin = -Inf, ymax = Inf, 
    alpha = 0.2) +
  theme_minimal()


