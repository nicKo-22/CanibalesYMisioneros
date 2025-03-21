# =========================================================================
# Implementación de las funciones para el problema de misioneros y caníbales
# =========================================================================

# Esta función debe devolver una lista con la información necesaria para resolver el problema.
initialize.problem <- function(canibales, misioneros, capacidadBarca) {
  problem <- list()
  
  problem$name <- "Misioneros y Caníbales"
  
  # Estado inicial: todos en la orilla izquierda (izquierda = 0, derecha = 1)
  problem$state_initial <- c(canibales, misioneros, 0, capacidadBarca)
  
  # Estado final: todos en la orilla derecha
  problem$state_final <- c(0, 0, 1, capacidadBarca)
  
  problem$misioneros <- misioneros
  problem$canibales <- canibales
  problem$capacidadBarca <- capacidadBarca
  
  #Acciones posibles 
  acciones <- list()
  for (m in 0:capacidadBarca) {
    for (c in 0:capacidadBarca) {
      if (m + c <= capacidadBarca && m + c > 0) {
        acciones[[length(acciones) + 1]] <- c(m, c, 1)
        acciones[[length(acciones) + 1]] <- c(m, c, -1)
      }
    }
  }
  
  problem$actions_possible <- do.call(rbind, acciones)
  
  return(problem)
}

# Verifica si una acción es aplicable en el estado dado
is.applicable <- function (state, action, problem) {
  
  result <- FALSE
  
  c_izq <- as.numeric(state[1])
  m_izq <- as.numeric(state[2])
  orillaBarca <- state[3]
  capacidadBarca <- as.numeric(state[4])
  
  c_der <-  problem$canibales - c_izq
  m_der <-  problem$misioneros - m_izq
  
  if (is.na(action[1]) || is.na(action[2]) || is.na(action[3])) {
    return(FALSE)
  }
  
  if(orillaBarca== 0 && action[3] == -1){
    if(action[1] > m_izq || action[2] > c_izq){
      return(FALSE)
    }
    numCanibalIzqFin <- c_izq - action[2]
    numCanibalDchaFin <- c_der + action[2]
    numMisioneroIzqFin <- m_izq - action[1]
    numMisioneroDchaFin <- m_der + action[1]
    
    if ((numMisioneroIzqFin > 0 && numCanibalIzqFin > numMisioneroIzqFin) || 
        (numMisioneroDchaFin > 0 && numCanibalDchaFin > numMisioneroDchaFin)) {
      return(FALSE)
    }
    
    return(TRUE)
  }else{
    if(action[1] > m_der || action[2] > c_der){
      return(FALSE)
    }
    numCanibalIzqFin <- c_izq + action[2]
    numCanibalDchaFin <- c_der - action[2]
    numMisioneroIzqFin <- m_izq + action[1]
    numMisioneroDchaFin <- m_der - action[1]
    
    if ((numMisioneroIzqFin > 0 && numCanibalIzqFin > numMisioneroIzqFin) || 
        (numMisioneroDchaFin > 0 && numCanibalDchaFin > numMisioneroDchaFin)) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state 
  
  c_izq <- as.numeric(state[1])
  m_izq <- as.numeric(state[2])
  orillaBarca <- state[3]
  capacidadBarca <- problem$capacidadBarca    
  
  
  if(orillaBarca == 0 && action[3] == -1){
    c_izq <- c_izq - action[2]
    m_izq <- m_izq - action[1]
    orillaBarca <- 1
  }else{
    c_izq <- c_izq + action[2]
    m_izq <- m_izq + action[1]
    orillaBarca <- 0
  }
  
  return(c(c_izq, m_izq, orillaBarca, capacidadBarca))
  
}

# Comprueba si un estado es final
is.final.state <- function(state, final_state, problem) {
  c_izq <- as.numeric(state[1])
  m_izq <- as.numeric(state[2])
  orilla_barca <- 1
  
  return(c_izq == 0 && m_izq == 0 && orilla_barca == 1)
}

# Representa un estado como una cadena
to.string <- function(state, problem=NULL) {
  m_izq <- as.numeric(state[1])
  c_izq <- as.numeric(state[2])
  
  orillaBarca <- state[3]
  
  m_der <- problem$misioneros - m_izq
  c_der <- problem$canibales - c_izq
  
  if(orillaBarca == 0){
    orillaBarca <- "Izquierda"
  }else{
    orillaBarca <- "Derecha"
  }
  
  return(paste("(Izq: ", c_izq, " C, ", m_izq, " M)-[Barca: ", orillaBarca, "]- (Der: ", c_der, " C ", m_der, " y M)", sep=""))
}


# Devuelve el costo de aplicar una acción
#get.cost <- function(action, state, problem) {
#  return(1) # Costo fijo de 1
#}

# Versión alternativa del costo basado en el tiempo
get.cost<- function(action, state, problem) {
  tiempo_base <- 15
  retraso_misioneros <- tiempo_base * 0.1 * action[1]
  retraso_canibales <- tiempo_base * 0.05 * action[2]
  tiempo_subir_bajar <- (action[1] + action[2]) * 1
  
  return(tiempo_base + retraso_misioneros + retraso_canibales + tiempo_subir_bajar)
}

# Función heurística para búsqueda informada
get.evaluation <- function(state, problem) {
  return(state[1] + state[2])
}

# Heurística alternativa
get.evaluation <- function(state, problem) {
  total_izq <- as.numeric(state[1]) + as.numeric(state[2])
  capacidadBarca <- as.numeric(state[4])
  
  return(ceiling(total_izq / capacidadBarca)) # Estimación de viajes necesarios
}
