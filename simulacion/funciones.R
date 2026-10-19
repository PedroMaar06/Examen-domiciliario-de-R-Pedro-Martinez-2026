# Función para repartir cartas a N jugadores (sin reposición)
Repartir <- function(mazo_actual, n_jugadores) {
  if(n_jugadores * 3 > nrow(mazo_actual)) {
    stop("No hay suficientes cartas en el mazo para repartir.")
  }
  Cartas_que_tocaron <- sample(1:nrow(mazo_actual), n_jugadores * 3 , replace = FALSE)
  
  Cartas_repartidas <- mazo_actual[Cartas_que_tocaron, ]
  mazo_restante <- mazo_actual[-Cartas_que_tocaron, ]
  
  Manos_jugadores <- list()
  for(i in 1:n_jugadores) {
    fila_inicio <- (i - 1) * 3 + 1
    fila_fin <- i * 3
    Manos_jugadores[[paste0("Jugador_", i)]] <- Cartas_repartidas[fila_inicio:fila_fin, ]
  }
  
  return(list(Manos = Manos_jugadores, Mazo_Restante = mazo_restante))
}

sacar_al_azar <- function(mazo) {
  Repartir(mazo, n_jugadores = 1)
}

# 3. Función contar_tantos
contar_tantos <- function(mano) {
  valores <- ifelse(mano$Numero >= 10, 0, mano$Numero)    # Las cartas del 1 al 7 valen su número, 10, 11 y 12 valen 0
  palos_freq <- table(mano$Palo)                          # Contamos cuántas cartas hay de cada palo
  
  if (any(palos_freq >= 2)) {                             # Si hay al menos dos cartas del mismo palo, identificamos cuál es el palo repetido
    palo_envido <- names(palos_freq)[palos_freq >= 2]     # Me quedo solo con las cartas que suman para el envido
    valores_palo <- valores[mano$Palo == palo_envido]
    top2 <- sort(valores_palo, decreasing = TRUE)[1:2]    # Ordenamos de mayor a menor y sumamos las dos más altas (por si hay 3 del mismo palo)
    return(20 + sum(top2))
  } else {
    return(max(valores))                                  # Si son de distinto palo, el tanto es la carta de mayor valor
  }
}

# 4. Función para calcular la probabilidad exacta de perder el envido
# Recibe nuestra mano, el mazo restante y si somos mano
probabilidad_exacta_oponente <- function(mi_mano, mazo_restante, soy_mano = TRUE) {

  mis_tantos <- contar_tantos(mi_mano)                     # Calculamos nuestros propios tantos con la función que ya armamos
  combinaciones <- combn(1:nrow(mazo_restante), 3)         # Generamos todas las combinaciones posibles de a 3 cartas. crea una matriz donde cada columna tiene 3 índices distintos.
  total_combinaciones <- ncol(combinaciones)               # hay exactamente 7770 combinaciones posibles.
  victorias_oponente <- 0                                  # Chequeamos el ganador para cada combinación posible
  for (i in 1:total_combinaciones) {
    indices_rival <- combinaciones[, i]
    mano_rival <- mazo_restante[indices_rival, ]
    tantos_rival <- contar_tantos(mano_rival)              # Calculamos cuántos tantos tiene el rival dada cada posible mano

    if (tantos_rival > mis_tantos) {                       # Evaluamos quién gana
      victorias_oponente <- victorias_oponente + 1
      
    } else if (tantos_rival == mis_tantos) {               # El rival solo gana el empate si yo NO soy mano.
      if (soy_mano == FALSE) {
        victorias_oponente <- victorias_oponente + 1
      }
    }
  }
  
  # 5. Calculamos la probabilidad: casos favorables al rival / casos totales
  prob_perder <- victorias_oponente / total_combinaciones
  
  return(prob_perder)
}



# 5. Simulación de partida 3 vs 3 (Nuestra mano fija, compañeros < 27)
simular_truco_6 <- function(mi_mano, mazo_restante, mi_equipo_es_mano = TRUE, N_sim = 2000) {
  mis_tantos <- contar_tantos(mi_mano)
  victorias_mi_equipo <- 0
  simulaciones_validas <- 0
  maximos_rivales <- numeric(N_sim) 
  while (simulaciones_validas < N_sim) {
    extraccion <- Repartir(mazo_restante, n_jugadores = 5)              # Repartimos solo a los 5 jugadores restantes desde el mazo sobrante
    comp1 <- extraccion$Manos$Jugador_1                                 # Asignamos las manos de los compañeros (Jugador 1 y 2 de esta extracción)
    comp2 <- extraccion$Manos$Jugador_2
    tantos_comp1 <- contar_tantos(comp1)
    tantos_comp2 <- contar_tantos(comp2)
    
    if (tantos_comp1 < 27 && tantos_comp2 < 27) {                       # Me quedo solo con simulaciones en las que mis compañeros tienen menos de 27 cada uno
      simulaciones_validas <- simulaciones_validas + 1
      
      riv1 <- extraccion$Manos$Jugador_3                                # Las manos de los 3 rivales (Jugadores 3, 4 y 5)
      riv2 <- extraccion$Manos$Jugador_4
      riv3 <- extraccion$Manos$Jugador_5

      tantos_rivales <- c(contar_tantos(riv1), contar_tantos(riv2), contar_tantos(riv3))
      max_rival <- max(tantos_rivales)                                  # Calculamos el mejor envido del equipo rival
      maximos_rivales[simulaciones_validas] <- max_rival                # Para cada simulacion valida
      max_mi_equipo <- max(mis_tantos, tantos_comp1, tantos_comp2)      # Calculo nuestro mejor envido
      
      if (max_mi_equipo > max_rival) {                                  # Vemos quien gana
        victorias_mi_equipo <- victorias_mi_equipo + 1
      } else if (max_mi_equipo == max_rival && mi_equipo_es_mano) {
        victorias_mi_equipo <- victorias_mi_equipo + 1
      }
    }
  }
  
  prob_ganar <- victorias_mi_equipo / N_sim
  
  return(list(probabilidad = prob_ganar, historico_rivales = maximos_rivales))
}















