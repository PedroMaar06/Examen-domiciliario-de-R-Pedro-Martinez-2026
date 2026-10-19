# 📊 Exploración, Econometría y Simulación en R

¡Bienvenido! Este repositorio contiene la resolución del examen domiciliario de R para la materia **Laboratorio para el Análisis de Datos Economicos y Financieros** (Universidad Torcuato Di Tella). 

El proyecto explora tres grandes áreas del análisis cuantitativo, divididas en módulos independientes:

### 📈 1. Análisis de Datos: Dinámica de Commodities Globales
* **¿Qué hacemos?** Exploramos cómo interactúan los precios internacionales de las materias primas a lo largo de las décadas y cómo reaccionan ante crisis financieras globales.
* **¿Cómo lo hacemos?** Utilizamos `tidyverse` para estructurar datos históricos del FMI, calculamos matrices de correlación cruzada (ej. energía vs agro) y evaluamos la resiliencia de distintos sectores durante los shocks recesivos de 2001 y 2008.

### 🧮 2. Análisis Econométrico: Ingresos y Esperanza de Vida
* **¿Qué hacemos?** Investigamos la evolución del crecimiento económico y descubrimos qué factores determinan la esperanza de vida de una nación.
* **¿Cómo lo hacemos?** Usando el dataset `gapminder`, ajustamos y evaluamos modelos de regresión polinómica (demostrando empíricamente los peligros del *overfitting*), aplicamos Tests de Hipótesis empíricos y diseñamos un modelo de regresión múltiple robusto basado en la teoría económica (Curva de Preston).

### 🃏 3. Simulación: La Matemática detrás del Truco
* **¿Qué hacemos?** Calculamos las probabilidades exactas y simulamos escenarios estratégicos del clásico juego de cartas argentino "Truco", específicamente para la fase del "Envido".
* **¿Cómo lo hacemos?** Programamos el motor del juego desde cero. Utilizamos análisis combinatorio riguroso para obtener probabilidades deterministas en duelos 1vs1, y aplicamos métodos de simulación condicionada (muestreo por rechazo) para analizar probabilidades con información asimétrica en partidas de 3vs3.

---
**🛠️ Herramientas utilizadas:** R base, `dplyr`, `tidyr`, `ggplot2`.

**🚀 Cómo probarlo:** Simplemente abrí el archivo `.Rproj` en RStudio para setear el entorno y ejecutá los scripts principales de cada carpeta. Todos los módulos cuentan con una semilla fijada (`set.seed`) para garantizar su total reproducibilidad.
