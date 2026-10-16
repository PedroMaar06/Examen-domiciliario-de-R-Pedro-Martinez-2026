#### Configuración inicial ####

set.seed(2026)         # Elijo esta semilla arbitrariamente
library(dplyr)
library(ggplot2)
library(tidyr)

# Defino bien el directorio universalmente, para que no haya que establecerlo manualmente
# Uso source para que traiga y sean utilizables las funciones definidas en "funciones.R"

if (file.exists("funciones.R")) {
  source("funciones.R")
  ruta_csv <- "data/gapminder.csv"
} else {
  source("analisis_econometrico/funciones.R")
  ruta_csv <- "analisis_econometrico/data/gapminder.csv"
}

# Obtengo los datos del dataset gapminder
# En el archivo de datos hay faltantes escritos como "-", los paso a NA para que
# variables como life_expectancy_female se lean correctamente como numéricas
gapminder_datos <- read.csv(ruta_csv, check.names = TRUE, na.strings = c("", "NA", "-"))


#### PARTE 1: Ingreso por persona ####


# Inciso 1.1: 
# Graficar la evolucion a traves del tiempo de "income_per_person" en Argentina
gapminder_argentina <- gapminder_datos %>%
  filter(country == "Argentina") %>%
  arrange(year) %>%
  filter(!is.na(income_per_person))

# Grafico (Graf), del ingreso por persona (IPP) en Argentina (Arg)
Graf_IPPArg <- ggplot(gapminder_argentina, aes(x = year, y = income_per_person)) +
  geom_line(color = "#0072B2", linewidth = 1) +
  geom_point(color = "#0072B2", size = 1.5) +
  labs(title = "Evolución Temporal del Ingreso por Persona en Argentina",
       x = "Año", y = "Income per Person") +
  theme_minimal()
print(Graf_IPPArg)


# Inciso 1.2: 
# Regresiones de Income per person
# Separo los datos de Train y Test 

TotalDeAñosArg <- nrow(gapminder_argentina)
DatosDeTestArg <- tail(gapminder_argentina, 10)              # Muestra los valores del final del vector
DatosDeTrainArg <- head(gapminder_argentina, TotalDeAñosArg - 10)     


# Cambio los años observados para que sea mas visible la evolución del ingreso
# Sería el equivalente a hacer un "zoom" en los años que estamos observando
# Estandarizo la variable tiempo t para evitar problemas numéricos con grado los polinomios de mayor grado:
# Defino:  t = year - min(year) 
t_base <- min(gapminder_argentina$year)
DatosDeTrainArg <- DatosDeTrainArg %>% mutate(t = year - t_base)
DatosDeTestArg  <- DatosDeTestArg  %>% mutate(t = year - t_base)
gapminder_argentina  <- gapminder_argentina  %>% mutate(t = year - t_base)

# Estimación de los 3 modelos solicitados, sobre el conjunnto de entrenamiento:
# (a) Modelo lineal: (B1)*t + B0 + e
Mod_Lineal <- lm(income_per_person ~ t, data = DatosDeTrainArg)

# (b) Modelo cuadrático: (B2)*(t^2) + (B1)*t + B0 + e
Mod_PolinomioGrado2  <- lm(income_per_person ~ poly(t, 2, raw = TRUE), data = DatosDeTrainArg)

# (c) Modelo polinómico de grado 10: (B10)*(t^10) + (B9)*(t^9) + (B8)*(t^8) + (B7)*(t^7) + (B6)*(t^6) + (B5)*(t^5) + (B4)*(t^4) + (B3)*(t^3) + (B2)*(t^2) + (B1)*t + B0 + e
Mod_PolinomioGrado10 <- lm(income_per_person ~ poly(t, 10, raw = TRUE), data = DatosDeTrainArg)

summary(Mod_Lineal)
summary(Mod_PolinomioGrado2)
summary(Mod_PolinomioGrado10)

# Generar predicciones en todo el rango para visualizar curvas
Precicciones <- data.frame(
  year = gapminder_argentina$year,
  t = gapminder_argentina$t,
  Prediccion_Lineal = predict(Mod_Lineal, newdata = gapminder_argentina),
  Prediccion_PolinomioGrado2  = predict(Mod_PolinomioGrado2,  newdata = gapminder_argentina),
  Prediccion_PolinomioGrado10 = predict(Mod_PolinomioGrado10, newdata = gapminder_argentina)
)

# Visualización comparativa de los ajustes
Ajustes <- graficar_ajustes_polinomios(DatosDeTrainArg, DatosDeTestArg, Precicciones)
print(Ajustes)

# Cuantificación de resultados sobre conjunto de testeo

TrainPredicciones <- data.frame(
  Real             = DatosDeTrainArg$income_per_person,
  Lineal           = fitted(Mod_Lineal),
  PolinomioGrado2  = fitted(Mod_PolinomioGrado2),
  PolinomioGrado10 = fitted(Mod_PolinomioGrado10)
)

TestPredicciones <- data.frame(
  Real = DatosDeTestArg$income_per_person,
  Lineal = predict(Mod_Lineal, newdata = DatosDeTestArg),
  PolinomioGrado2  = predict(Mod_PolinomioGrado2, newdata = DatosDeTestArg),
  PolinomioGrado10 = predict(Mod_PolinomioGrado10, newdata = DatosDeTestArg)
)

Resultados_Test <- data.frame(
  Modelo = c("Lineal", "Polinómico Grado 2 ", "Polinómico Grado 10"),
  RMSE_Train = c(calc_rmse(TrainPredicciones$Real, TrainPredicciones$Lineal),
                 calc_rmse(TrainPredicciones$Real, TrainPredicciones$PolinomioGrado2),
                 calc_rmse(TrainPredicciones$Real, TrainPredicciones$PolinomioGrado10)),
  RMSE_Test = c(calc_rmse(TestPredicciones$Real, TestPredicciones$Lineal),
                calc_rmse(TestPredicciones$Real, TestPredicciones$PolinomioGrado2),
                calc_rmse(TestPredicciones$Real, TestPredicciones$PolinomioGrado10)),
  MAE_Test  = c(calc_mae(TestPredicciones$Real, TestPredicciones$Lineal),
                calc_mae(TestPredicciones$Real, TestPredicciones$PolinomioGrado2),
                calc_mae(TestPredicciones$Real, TestPredicciones$PolinomioGrado10))
)
print(Resultados_Test)

# En el caso del polinomio de grado 10, tiene mucho overfitting al conjunto de entrenamiento
# que cuando se intenta evaluar sobre los datos de testeo, diverge exponencialmente. 


# 1.3 Análisis con cuatro países sudamericanos adicionales
Paises_Sudamericanos <- c("Argentina", "Brazil", "Chile", "Colombia", "Uruguay")

# Extraigo unicamente el ingreso per capita
Datos_Sudamericanos <- gapminder_datos %>%
  filter(country %in% Paises_Sudamericanos) %>%
  select(country, year, income_per_person) %>%
  pivot_wider(names_from = country, values_from = income_per_person) %>%
  arrange(year) %>%
  drop_na()


# (a) Matriz de correlaciones en niveles
Matriz_Correlaciones <- cor(Datos_Sudamericanos %>% select(-year))
print("Matriz de Correlaciones de Ingresos en Niveles:")
print(round(Matriz_Correlaciones, 3))
print(graficar_matriz_correlacion(Matriz_Correlaciones, "Correlación de Ingresos (Niveles)"))

# (b) Matriz de correlaciones de tasas de crecimiento interanual (Delta Y / Y)
Datos_Crecimiento <- Datos_Sudamericanos %>%
  arrange(year) %>%
  mutate(
    Argentina = (Argentina - lag(Argentina)) / lag(Argentina),
    Brazil    = (Brazil    - lag(Brazil))    / lag(Brazil),
    Chile     = (Chile     - lag(Chile))     / lag(Chile),
    Colombia  = (Colombia  - lag(Colombia))  / lag(Colombia),
    Uruguay   = (Uruguay   - lag(Uruguay))   / lag(Uruguay)
  ) %>%
  filter(!is.na(Argentina))

Matriz_Correlaciones_Crecimiento <- cor(Datos_Crecimiento %>% select(-year))
print("Matriz de Correlaciones de Variaciones Porcentuales (Crecimiento):")
print(round(Matriz_Correlaciones_Crecimiento, 3))
print(graficar_matriz_correlacion(Matriz_Correlaciones_Crecimiento, "Correlación de Tasas de Crecimiento Anual"))


#### PARTE 2: Esperanza de vida y genero ####


# Filtramos un año representativo reciente con datos completos
Año_Analisis <- 2006
Datos_2006 <- gapminder_datos %>%
  filter(year == Año_Analisis) %>%
  select(country, life_expectancy, life_expectancy_female, income_per_person) %>%
  drop_na()

# 2.5 Gráfico de dispersión: life expectancy frente a life expectancy female
Comparacion_Expectativa_Vida <- ggplot(Datos_2006, aes(x = life_expectancy_female, y = life_expectancy)) +
  geom_point(alpha = 0.7, color = "#009E73", size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste("Esperanza de Vida Total vs. Femenina (Año", Año_Analisis, ")"),
       subtitle = "Línea roja discontinua representa la identidad y = x",
       x = "Life Expectancy Female",
       y = "Life Expectancy (Población general)") +
  theme_minimal()
print(Comparacion_Expectativa_Vida)

# 2.6 Regresión simple
Reg_Simple_Exp_Vida <- lm(life_expectancy ~ life_expectancy_female, data = Datos_2006)
SummaryDeRegSimple <- summary(Reg_Simple_Exp_Vida)
print("Resumen de la Regresión Simple:")
print(SummaryDeRegSimple)
cat("R-cuadrado Simple:", SummaryDeRegSimple$r.squared, "\n")

# 2.7 Test de hipótesis:
# H0: life_expectancy_female = life_expectancy
# H1: life_expectancy_female > life_expectancy
# Es equivalente a testear si la diferencia dif = female - total > 0
Test_Exp_Vida <- t.test(Datos_2006$life_expectancy_female, 
                     Datos_2006$life_expectancy, 
                     paired = TRUE, 
                     alternative = "greater")
print(Test_Exp_Vida)

# 2.8 Regresión múltiple incorporando income per person
Reg_Mult_Exp_Vida <- lm(life_expectancy ~ life_expectancy_female + income_per_person, data = Datos_2006)
SummaryDeRegMult <- summary(Reg_Mult_Exp_Vida)
print("Resumen de la Regresión Múltiple:")
print(SummaryDeRegMult)
cat("R-cuadrado Simple:  ", SummaryDeRegSimple$r.squared, "\n")
cat("R-cuadrado Múltiple:", SummaryDeRegMult$r.squared, "\n")
cat("R-cuadrado Ajustado Simple:  ", SummaryDeRegSimple$adj.r.squared, "\n")
cat("R-cuadrado Ajustado Múltiple:", SummaryDeRegMult$adj.r.squared, "\n")

# Inciso 2.9: Modelo lineal de máximo 3 covariables excluyendo life_expectancy_female
# Justificación de exclusiones:
# Excluyo life_expectancy_female (por consigna) y también life_expectancy_male porque la 
# esperanza de vida general es el promedio ponderado entre hombres y mujeres (incluirla sería 
# intentar explicar la variable consigo misma, dando un R^2 artificialmente cercano a 1).
# Cree log_income = log(income_per_person) para capturar el efecto no lineal de los ingresos
# (rendimientos marginales decrecientes del ingreso en la salud, curva de Preston).
# Elijo 3 covariables clave del dataset:
# log_income: nivel de desarrollo económico y recursos de los hogares.
# child_mortality: proxy directa de la calidad y cobertura del sistema sanitario.
# children_per_woman: tasa de fertilidad, vinculada a la transición demográfica.

Datos_2006_Mod <- gapminder_datos %>%
  filter(year == Año_Analisis) %>%
  filter(!is.na(life_expectancy) & !is.na(income_per_person) & 
           !is.na(child_mortality) & !is.na(children_per_woman)) %>%
  mutate(log_income = log(income_per_person))

Mod_3Cov <- lm(life_expectancy ~ log_income + child_mortality + children_per_woman, data = Datos_2006_Mod)

print("Resumen del Modelo Final Propuesto (3 covariables):")
print(summary(Mod_3Cov))