set.seed(2026)
library(dplyr)
library(ggplot2)
library(tidyr)

# Defino bien el directorio universalmente, para que no haya que establecerlo manualmente
# Uso source para que traiga y sean utilizables las funciones definidas en "funciones.R"

if (file.exists("funciones.R")) {
  source("funciones.R")
  ruta_csv <- "data/precios_commodities_fmi.csv"
} else {
  source("analisis_datos/funciones.R")
  ruta_csv <- "analisis_datos/data/precios_commodities_fmi.csv"
}

# Obtengo los datos del dataset de precios del FMI
# En el archivo de datos hay faltantes escritos como "-"
Datos_Precios <- read.csv(ruta_csv, check.names = TRUE, na.strings = c("", "NA", "-"))


# Transformación a formato largo (tidy) usando la función definida para ajustar los datos
Datos_Largo <- preparar_datos_largos(Datos_Precios)

# Cálculo de series de inflación anual (promedio anual de precios y su variación porcentual)
Datos_Inflacion <- calcular_inflacion_anual(Datos_Largo)



#### PREGUNTA 1: ¿Cuáles son las correlaciones más importantes entre precios? ####

# Identificamos las columnas de productos individuales
Productos_Cols <- unique(Datos_Largo$producto)

# Extraemos todos los pares de correlación posibles
Pares_Correlacion <- extraer_pares_correlacion(Datos_Precios, Productos_Cols)

cat("\n=== TOP 10 CORRELACIONES POSITIVAS MÁS FUERTES ===\n")
print(head(Pares_Correlacion, 10))

cat("\n=== CORRELACIONES POSITIVAS ENTRE SECTORES DISTINTOS (INESPERADAS/ESTRUCTURALES) ===\n")
print(
  Pares_Correlacion %>% 
    filter(
      (Producto_1 %in% c("petroleo_brent", "cobre", "mineral_hierro") & 
         Producto_2 %in% c("poroto_soja", "maiz", "carne_bovina")) |
        (Producto_2 %in% c("petroleo_brent", "cobre", "mineral_hierro") & 
           Producto_1 %in% c("poroto_soja", "maiz", "carne_bovina"))
    )
)

cat("\n=== TOP 10 CORRELACIONES NEGATIVAS (DESACOPLE / SUSTITUCIÓN) ===\n")
print(tail(Pares_Correlacion, 10))

# Matriz visual de correlación sobre una canasta representativa de commodities (uso "pairwise.complete.obs", por 2 datos faltantes del carbon de Sudafrica)
Canasta_Clave <- c("petroleo_brent", "carbon_sudafrica", "cobre", "mineral_hierro", 
                   "poroto_soja", "aceite_soja", "maiz", "trigo", "naranja", "langostino")

Matriz_Canasta <- cor(Datos_Precios[, Canasta_Clave], use = "pairwise.complete.obs")
Grafico_Correlaciones <- graficar_matriz_correlacion(Matriz_Canasta, "Correlaciones entre Commodities Seleccionados")
print(Grafico_Correlaciones)



#### PREGUNTA 2: ¿Cuáles productos reaccionan más a crisis globales (2001, 2008) y cómo? ####

# Análisis del shock en la Crisis de las Puntocom / Recesión 2001
Crisis_2001 <- analizar_crisis(Datos_Inflacion, anio_crisis = 2001)
Grafico_Crisis_2001 <- graficar_impacto_crisis(Crisis_2001, "Impacto en Precios: Crisis Global 2001")
print(Grafico_Crisis_2001)

# Análisis del shock en la Gran Crisis Financiera Global 2008/2009
Crisis_2009 <- analizar_crisis(Datos_Inflacion, anio_crisis = 2009)
Grafico_Crisis_2009 <- graficar_impacto_crisis(Crisis_2009, "Impacto en Precios: Crisis Financiera Global 2009 vs 2008")
print(Grafico_Crisis_2009)



#### PREGUNTA 3: Matriz de correlación de las INFLACIONES ####

# Reordeno los datos, cambiando su formato para que no ocurran errores tecnicos
Datos_Inflacion_Q3 <- preparar_inflacion_ancha(Datos_Inflacion)

# Calculo la correlacion, para la misma canasta que la pregunta 1
Matriz_Inflacion <- cor(Datos_Inflacion_Q3[, Canasta_Clave], use = "pairwise.complete.obs")

Grafico_Corr_Inflacion <- graficar_matriz_correlacion(Matriz_Inflacion, "Correlaciones de la Inflación Anual de Commodities")
print(Grafico_Corr_Inflacion)


#### PREGUNTA 4: Comparar matriz de correlaciones entre PRECIOS de distintos periodos ####

# Divido los datos en 3 periodos "simetricos", con hechos historicos importantes en cada uno. 
# Período 1: 1980-1991
Precios_P1 <- filtrar_y_ensanchar_precios(Datos_Largo, anio_inicio = 1980, anio_fin = 1991)

# Período 2: 1992-2003
Precios_P2 <- filtrar_y_ensanchar_precios(Datos_Largo, anio_inicio = 1992, anio_fin = 2003)

# Período 3: 2004-2016
Precios_P3 <- filtrar_y_ensanchar_precios(Datos_Largo, anio_inicio = 2004, anio_fin = 2016)

# Calculo las correlaciones en cada periodo, para los mismos productos que la canasta de la pregunta 1
Matriz_P1 <- cor(Precios_P1[, Canasta_Clave], use = "pairwise.complete.obs")
Matriz_P2 <- cor(Precios_P2[, Canasta_Clave], use = "pairwise.complete.obs")
Matriz_P3 <- cor(Precios_P3[, Canasta_Clave], use = "pairwise.complete.obs")

# Grafico las tres matrices
Grafico_Matriz_P1 <- graficar_matriz_correlacion(Matriz_P1, "Correlaciones: 1980 - 1991")
Grafico_Matriz_P2 <- graficar_matriz_correlacion(Matriz_P2, "Correlaciones: 1992 - 2003")
Grafico_Matriz_P3 <- graficar_matriz_correlacion(Matriz_P3, "Correlaciones: 2004 - 2016")

print(Grafico_Matriz_P1)
print(Grafico_Matriz_P2)
print(Grafico_Matriz_P3)

