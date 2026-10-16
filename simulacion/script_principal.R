set.seed(2026)
source("simulacion/funciones.R")


# Inciso 1: Creamos el mazo
# (dimension 40x2, cada fila es una carta, las columnas indican numero y palo)

Palos <- c("Espada", "Basto", "Oro", "Copa")
Numeros <- c(1:7, 10:12)
Mazo <- expand.grid(Numero = Numeros, Palo = Palos, stringsAsFactors = FALSE)

# 2. Extraer cartas al azar (Repartir) y graficar histograma del valor medio

N_manos <- 10000
Medias_de_las_manos <- numeric(N_manos)

for (i in 1:N_manos) {
  Extraccion_Inc2 <- Repartir(Mazo, 1)
  Medias_de_las_manos[i] <- mean(as.numeric(Extraccion_Inc2$Manos$Jugador_1$Numero))  # Calcula la media del valor de la carta
}
hist(Medias_de_las_manos, freq = FALSE,
     main = "Histograma: Valor medio de la mano extraída",
     xlab = "Media del número de las 3 cartas",
     col = "steelblue", border = "white", breaks = 15)


# Inciso 3: Funcion de contar tantos. (La funcion esta definida en "funciones")
# Hago una prueba para k jugadores

k <- 4  # Cantidad de jugadores

Extraccion_Inc3 <- Repartir(Mazo, k) 
Tantos_todos_los_jugadores <- numeric(k) # Creamos un vector vacío para guardar los k puntajes

for (i in 1:k) {
  nombre_jugador <- paste0("Jugador_", i)                        # Armamos el nombre dinámicamente ("Jugador_1", "Jugador_2", etc.)
  mano_actual <- Extraccion_Inc3$Manos[[nombre_jugador]]              # Calculamos el tanto de ese jugador específico
  Tantos_todos_los_jugadores[i] <- contar_tantos(mano_actual)
}

print(Tantos_todos_los_jugadores) # Te devuelve un vector con los k valores de envido


# Inciso 4: Probabilidad exacta de perder el envido en un 1vs1 (Para una observacion dada)

# Hacemos una extracción nueva solo para 1 jugador
Extraccion_Inc4 <- Repartir(Mazo, 1)

Mi_Mano_Prueba <- Extraccion_Inc4$Manos$Jugador_1
Mazo_Sobrante <- Extraccion_Inc4$Mazo_Restante

# Vemos cuanto sumamos nosotros
Mis_Tantos <- contar_tantos(Mi_Mano_Prueba)

# Probabilidad si somos mano
prob_pierdo_siendo_mano <- probabilidad_exacta_oponente(Mi_Mano_Prueba, Mazo_Sobrante, soy_mano = TRUE)
cat("Probabilidad de que el rival gane (Siendo yo mano):", prob_pierdo_siendo_mano * 100, "%\n")

# Probabilidad si NO somos mano
prob_pierdo_no_mano <- probabilidad_exacta_oponente(Mi_Mano_Prueba, Mazo_Sobrante, soy_mano = FALSE)
cat("Probabilidad de que el rival gane (Sin ser mano):", prob_pierdo_no_mano * 100, "%\n")


# Inciso 5: Simulación de partida 3 vs 3
# Ejecutamos la simulación con 2000 casos válidos (hago simulaciones HASTA que llegue a 2000 que me sirvan)

Extraccion_Inc5 <- Repartir(Mazo,1)
Mi_Mano_3v3 <- Extraccion_Inc5$Manos$Jugador_1
Mazo_para_los_5 <- Extraccion_Inc5$Mazo_Restante

resultados_3v3_siendo_mano <- simular_truco_6(Mi_Mano_3v3, Mazo_para_los_5, mi_equipo_es_mano = TRUE, N_sim = 2000)
cat("Mis tantos son:",contar_tantos(Mi_Mano_3v3), "\n")
cat("Probabilidad ESTIMADA de ganar el envido en 3v3 (Siendo mano):", 
    round(resultados_3v3_siendo_mano$probabilidad * 100, 2), "%\n")

resultados_3v3_sin_ser_mano <- simular_truco_6(Mi_Mano_3v3, Mazo_para_los_5, mi_equipo_es_mano = FALSE, N_sim = 2000)
cat("Mis tantos son:",contar_tantos(Mi_Mano_3v3) , "\n")
cat("Probabilidad ESTIMADA de ganar el envido en 3v3 (Sin ser mano):", 
    round(resultados_3v3_sin_ser_mano$probabilidad * 100, 2), "%\n")



# Inciso 6 (Opcional): Histograma del equipo rival
# Extraemos el historial de los puntajes
maximos_del_rival <- resultados_3v3_siendo_mano$historico_rivales

# 1. Definimos los cortes (breaks) para que cada columna ocupe exactamente 1 valor
cortes <- seq(min(maximos_del_rival) - 0.5, max(maximos_del_rival) + 0.5, by = 1)

# 2. Dibujamos el histograma sin el eje X por defecto (xaxt = "n")
hist(maximos_del_rival, freq = FALSE,
     breaks = cortes,
     main = "Máximo Envido alcanzado por el Equipo Rival (3 jugadores)",
     xlab = "Puntos de Envido",
     ylab = "Densidad",
     col = "darkgreen", border = "white",
     xaxt = "n") 

# 3. Dibujamos nuestro propio eje X con todos los números (las marcas)
axis(1, at = min(maximos_del_rival):max(maximos_del_rival), cex.axis = 0.8)

