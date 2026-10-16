

library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------------------------------------------------------
# 0. PREPARACIÓN Y CONVERSIÓN DE FORMATO (WIDE A LONG)
# ------------------------------------------------------------------------------

#' Transforma el dataset del FMI a formato largo y limpia fechas
#' Excluye las columnas de identificación y los índices agregados al final
preparar_datos_largos <- function(df_precios) {
  # Columnas que son índices agregados del FMI (no productos individuales)
  indices_agregados <- c("general_commodities", "no_energeticos", "alimentos_bebidas", 
                         "alimentos", "bebidas", "insumos_industriales", 
                         "agropecuarios_no_comestibles", "metales", "energia", "petroleo")
  substr
  df_precios %>%
    # Convertimos indice_tiempo fecha y anio numerico
    mutate(
      fecha = as.Date(substr(indice_tiempo, 1, 10)),
      year  = as.numeric(substr(indice_tiempo, 1, 4))
    ) %>%
    # Filtra hasta 2016 para tomar años completos
    filter(year <= 2016) %>%
    # Excluimos _id, indice_tiempo e índices agregados
    select(-any_of(c("_id", "indice_tiempo", indices_agregados))) %>%
    # Pasamos las columnas de productos a formato largo 
    pivot_longer(
      cols      = -c(fecha, year),
      names_to  = "producto",
      values_to = "precio"
    ) %>%
    filter(!is.na(precio))
}


# ------------------------------------------------------------------------------
# 1. FUNCIONES PARA PREGUNTA 1: CORRELACIONES DE PRECIOS
# ------------------------------------------------------------------------------

#' Obtiene todos los pares de correlación ordenados de mayor a menor
extraer_pares_correlacion <- function(df_precios, columnas_productos) {
  matriz_cor <- cor(df_precios[, columnas_productos], use = "pairwise.complete.obs")
  matriz_cor[lower.tri(matriz_cor, diag = TRUE)] <- NA
  
  as.data.frame(as.table(matriz_cor)) %>%
    filter(!is.na(Freq)) %>%
    rename(Producto_1 = Var1, Producto_2 = Var2, Correlacion = Freq) %>%
    arrange(desc(Correlacion))
}

#' Grafica el mapa de calor para una canasta determinada
graficar_matriz_correlacion <- function(cor_matrix, titulo) {
  cor_df <- as.data.frame(as.table(cor_matrix))
  colnames(cor_df) <- c("Producto_1", "Producto_2", "Correlacion")
  
  ggplot(cor_df, aes(x = Producto_1, y = Producto_2, fill = Correlacion)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Correlacion, 2)), color = "black", size = 3) +
    scale_fill_gradient2(low = "#d73027", mid = "#ffffbf", high = "#1a9850", 
                         midpoint = 0, limit = c(-1, 1), name = "r") +
    labs(title = titulo, x = "", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



# ------------------------------------------------------------------------------
# 2. FUNCIONES PARA PREGUNTA 2: REACCIÓN ANTE CRISIS GLOBALES
# ------------------------------------------------------------------------------

#' Calcula el precio promedio anual por producto y su variación interanual (%)
calcular_inflacion_anual <- function(df_largo) {
  df_largo %>%
    group_by(producto, year) %>%
    summarise(precio_promedio = mean(precio, na.rm = TRUE), .groups = "drop") %>%
    arrange(producto, year) %>%
    group_by(producto) %>%
    mutate(
      inflacion_anual = (precio_promedio - lag(precio_promedio)) / lag(precio_promedio) * 100
    ) %>%
    filter(!is.na(inflacion_anual)) %>%
    ungroup()
}


#' Calcula la variacion en el precio durante el año de la crisis frente al año anterior
analizar_crisis <- function(df_inf, anio_crisis) {
  df_inf %>%
    filter(year == anio_crisis) %>%
    select(producto, inflacion_anual) %>%
    rename(variacion_crisis = inflacion_anual) %>%
    arrange(variacion_crisis)
}

#' Grafica los productos con mayor caída y los que mas crecieron en una crisis
graficar_impacto_crisis <- function(df_crisis, titulo_crisis, n_extremos = 5) {
  top_caidas <- head(df_crisis, n_extremos)
  top_subas  <- tail(df_crisis, n_extremos)
  df_extremos <- bind_rows(top_caidas, top_subas) %>%
    mutate(tipo = ifelse(variacion_crisis < 0, "Caída / Sensible", "Suba / Resiliente"))
  
  ggplot(df_extremos, aes(x = reorder(producto, variacion_crisis), y = variacion_crisis, fill = tipo)) +
    geom_col(width = 0.7) +
    coord_flip() +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
    scale_fill_manual(values = c("Caída / Sensible" = "#d73027", "Suba / Resiliente" = "#1a9850")) +
    labs(
      title = titulo_crisis,
      subtitle = paste("Top", n_extremos, "mayores caídas y mayores subas del período"),
      x = "Producto", y = "Variación Anual (%)", fill = "Comportamiento"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


# ------------------------------------------------------------------------------
# 3. FUNCION PARA PREGUNTA 3: AJUSTE DE FORMATO DE LOS DATOS
# ------------------------------------------------------------------------------
#' Convierte el dataset de inflación a formato ancho para calcular correlaciones
preparar_inflacion_ancha <- function(df_inflacion) {
  df_inflacion %>%
    # Me quedo solo con las columnas necesarias
    select(year, producto, inflacion_anual) %>%
    # Cada columna es un producto
    pivot_wider(names_from = producto, values_from = inflacion_anual)
}


# ------------------------------------------------------------------------------
# 4. FUNCIONES PARA NUEVAS PREGUNTAS (INFLACIÓN Y PERIODOS)
# ------------------------------------------------------------------------------

#' Filtra el dataset de precios por un rango de años, promedia y lo pasa a formato ancho
filtrar_y_ensanchar_precios <- function(df_largo, anio_inicio, anio_fin) {
  df_largo %>%
    # Filtro los anios que quiero
    filter(year >= anio_inicio & year <= anio_fin) %>%
    # Calculo el precio promedio de cada anio
    group_by(year, producto) %>%
    summarise(precio = mean(precio, na.rm = TRUE), .groups = "drop") %>%
    # Pongo los precios en la columna
    pivot_wider(names_from = producto, values_from = precio)
}
