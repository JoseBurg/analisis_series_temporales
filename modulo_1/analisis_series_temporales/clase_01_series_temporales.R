library(tidyverse)
library(fpp3)
library(readxl)

# Cargando IMAE
imae <- read_excel("./imae.xlsx") %>% 
  mutate(fecha = as.Date(fecha))

# Graficando el IMAE con todo
imae %>% 
  ggplot(aes(fecha, indice_original)) +
  geom_line()

# Excluir efectos del covid-19
imae_2010_2019 <- imae %>% 
  filter(year >= 2010 & year <= 2019)

# Gráfico IMAE 2010 - 2019 
imae_2010_2019 %>% 
  ggplot(aes(fecha, indice_original)) +
  geom_line()


# Crear variación mensual y anual
imae_variacion <- imae_2010_2019 %>% 
  mutate(
    variacion_mensual = indice_original - lag(indice_original),
    variacion_anual = indice_original - lag(indice_original, 12)
  )

# Graficar variaciones
imae_variacion %>% 
  ggplot(aes(fecha)) +
  geom_line(aes(y = variacion_mensual), color = "red") +
  geom_line(aes(y = variacion_anual))


imae_pivot <- imae_variacion %>% 
  filter(!is.na(variacion_anual)) %>% 
  pivot_longer(
    c(variacion_mensual, variacion_anual), 
    names_to = "variacion",
    values_to = "valor"
  )

imae_pivot %>% 
  ggplot(aes(fecha)) +
  geom_line(aes(y = valor, color = variacion)) 

imae_pivot %>% 
  ggplot(aes(fecha)) +
  geom_line(aes(y = valor)) +
  facet_wrap(~variacion)



# Descomposicion de la serie ----------------------------------------------

imae_ts <- imae_2010_2019 %>% 
  mutate(month_year = yearmonth(fecha)) %>% 
  select(month_year, indice_original) %>% 
  as_tsibble(index = month_year) %>% 
  model(
    STL(indice_original)
  )

# Gráfico de los componente del IMAE
imae_ts %>% 
  components() %>% 
  autoplot()

# Gráfico de la serie original y desestacionalizada


components(imae_ts) |>
  as_tsibble() |>
  autoplot(indice_original, 
           colour="gray50", size = 0.7) +
  geom_line(aes(y=season_adjust), colour = "blue", size = 1) +
  labs(
    y = "IMAE",
    title = "Índice Mensual de Actividad Económica",
    x = "Fecha") +
  theme_minimal()

library(openxlsx)
book <- openxlsx::createWorkbook()

openxlsx::addWorksheet(book, "imae_dest")

openxlsx::writeData(book, "imae_dest", imae_pivot)


openxlsx::saveWorkbook(book, "imae_variacion.xlsx")



