# EJEMPLO:
# -------------Descomposición de series de datos de la ENCUESTA MENSUAL DE OPINIÓN EMPRESARIAL ----------

# actualizar datos eoe mensual --------------------------------------------
# eoe <- bcdata::get_em_eoe()
# saveRDS(eoe, file = "datos_eoe_mensual.rds")

# Paquetes ----------------------------------------------------------------

library(fpp3)
library(dplyr)

eoe <- readRDS("datos_eoe_mensual.rds")


to_tsible_saldo <- function(data, variable = "situacion_economica"){
  checkmate::assert_choice(variable, c("situacion_economica", 
                                        "nivel_de_inventario", "produccion",
                                        "pedidos", "empleo", "expectativa_produccion",
                                        "expectativa_precios", "expectativa_situacion_economica",
                                        "expectativa_empleo", "indice_de_confianza_industrial", 
                                        "indice_de_clima_empresarial"))
  
  data |> 
    tidyr::pivot_longer(
      cols = -periodo, names_to = "variables", values_to = "saldo") |> 
    dplyr::filter(variables == variable) |> 
    dplyr::mutate(Month =  tsibble::yearmonth(periodo)) |> 
    dplyr::select(-c(periodo, variables)) |> 
    dplyr::relocate(Month) |> 
    tsibble::as_tsibble(index = Month)
    
    
  
  
}


# Gráfico normal

situacion_economica <- eoe |> 
  to_tsible_saldo(variable = "situacion_economica") |> 
  mutate(variacion = saldo - lag(saldo))

  
autoplot(situacion_economica, variacion) +
  labs(title = "Situación económica", 
       subtitle = "Variación mensual",
       y = "Saldo de opinión",
       x = "Periodo") +
  theme_minimal()

# Descomposición de la serie
componentes_situacion_economica <- eoe |> 
  to_tsible_saldo(variable = "expectativa_situacion_economica") |> 
  model(stl = STL(saldo))

# Saldo y el componente de tendencia
componentes_situacion_econ <- components(componentes_situacion_economica) |> 
  tsibble::as_tsibble()

componentes_situacion_econ |> 
  autoplot(saldo, color = "gray50") +
  ggplot2::geom_line(aes(y = trend), color = "blue", size = 1)
  

# Gráfico por componente
components(componentes_situacion_economica) |> 
  autoplot()



# Datos desestacionalizados -----------------------------------------------

componentes_situacion_econ |> 
  autoplot(saldo) + 
  geom_line(aes(y = season_adjust), color = "blue")
  



























