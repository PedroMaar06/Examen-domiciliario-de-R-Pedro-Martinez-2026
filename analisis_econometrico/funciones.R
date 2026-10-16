library(dplyr)
library(ggplot2)
library(tidyr)

# Calculo la Raíz del Error Cuadrático Medio (RMSE)
#' @param actual Vector numérico con valores reales de la muestra
#' @param predicted Vector numérico con valores que el modelo predice
calc_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

#' Calculo el Error Absoluto Medio (MAE)
#' @param actual Vector numérico con valores reales de la muestra
#' @param predicted Vector numérico con valores que el modelo predice
calc_mae <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}

#' Grafica la serie temporal observada con polinomios 
#' @param df_train Data frame con el conjunto de entrenamiento
#' @param df_test Data frame con el conjunto de prueba
#' @param predictions_df Data frame con predicciones de cada modelo en todo el dominio temporal
graficar_ajustes_polinomios <- function(df_train, df_test, predictions_df) {
  ggplot() +
    geom_line(data = rbind(df_train, df_test), 
              aes(x = year, y = income_per_person, color = "Datos Observados"), 
              linewidth = 0.8) +
    geom_point(data = df_train, 
               aes(x = year, y = income_per_person, shape = "Train"), 
               size = 1.8, color = "black") +
    geom_point(data = df_test, 
               aes(x = year, y = income_per_person, shape = "Test"), 
               size = 2.2, color = "red") +
    geom_line(data = predictions_df, 
              aes(x = year, y = Prediccion_Lineal, color = "Grado 1 (Lineal)"), 
              linetype = "dashed", linewidth = 0.9) +
    geom_line(data = predictions_df, 
              aes(x = year, y = Prediccion_PolinomioGrado2, color = "Grado 2 (Cuadrático)"), 
              linetype = "dotdash", linewidth = 0.9) +
    geom_line(data = predictions_df, 
              aes(x = year, y = Prediccion_PolinomioGrado10, color = "Grado 10"), 
              linewidth = 0.9) +
    scale_color_manual(name = "Ajuste / Datos",
                       values = c("Datos Observados" = "gray40",
                                  "Grado 1 (Lineal)" = "#1f77b4",
                                  "Grado 2 (Cuadrático)" = "#2ca02c",
                                  "Grado 10" = "#d62728")) +
    scale_shape_manual(name = "Partición",
                       values = c("Train" = 16, "Test" = 17)) +
    labs(title = "Evolución y Ajustes de Regresión: Income per Person en Argentina",
         x = "Año",
         y = "Income per Person (USD ajustado)") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

#' Grafico de correlaciónes
#' @param cor_matrix Matriz de correlación cuadrada
#' @param titulo Título del gráfico
graficar_matriz_correlacion <- function(cor_matrix, titulo) {
  cor_df <- as.data.frame(as.table(cor_matrix))
  colnames(cor_df) <- c("Pais1", "Pais2", "Correlacion")
  
  ggplot(cor_df, aes(x = Pais1, y = Pais2, fill = Correlacion)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Correlacion, 2)), color = "black", size = 4) +
    scale_fill_gradient2(low = "#d73027", mid = "#ffffbf", high = "#1a9850", 
                         midpoint = 0, limit = c(-1, 1), name = "r") +
    labs(title = titulo, x = "", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}