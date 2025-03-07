# =========================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the
# search algorithms. If you modify any headers the algorithms may not work.
# =========================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(num_misioneros = 3, num_canibales = 3, capacidad_barca = 2) {
  problem <- list() # Default value is an empty list.

  # This attributes are compulsory
  problem$name <- paste0("Misioneros Y Canibales (", num_misioneros, "M, ", num_canibales, "C, Barca:", capacidad_barca, ")")
  
  
  # Estado inicial: todos en la orilla izquierda
  # [Misioneros_izq, Canibales_izq, Posición_barca, Misioneros_der, Canibales_der]
  # Posición barca: 0 -> izquierda, 1 -> derecha
  problem$state_initial <- c(num_misioneros, num_canibales, 0, 0, 0)
  
  # Estado final: todos en la orilla derecha
  problem$state_final <- c(0, 0, 1, num_misioneros, num_canibales)
  
  
  
  
  
  generate_moves <- function(capacidad_barca) {
    movimientos <- list()
    
    for (misionero in 0:capacidad_barca) {
      for (canibal in 0:capacidad_barca) {
        # Al menos una persona debe moverse y no más que la capacidad de la barca
        if (misionero + canibal > 0 && misionero + canibal <= capacidad_barca) {
          movimientos <- c(movimientos, list(c(misioenro, canibal)))
        }
      }
    }
    
    return(movimientos)
  }
  
  # Obtener todas las combinaciones posibles
  possible_moves <- generate_moves(capacidad_barca)
  
  # Función auxiliar para verificar si un estado es válido
  is_valid_state <- function(state) {
    misionero_izq <- state[1]
    canibal_izq <- state[2]
    misionero_der <- state[4]
    canibal_der <- state[5]
    
    # Verificar que no hay números negativos
    if (any(state < 0)) {
      return(FALSE)
    }
    
    # Verificar la condición de los misioneros
    # Si hay misioneros en la orilla izquierda, el número de caníbales no debe superarlos
    if (misionero_izq > 0 && canibal_izq > misionero_izq) {
      return(FALSE)
    }
    
    # Si hay misioneros en la orilla derecha, el número de caníbales no debe superarlos
    if (misionero_der > 0 && canibal_der > misionero_der) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  # Crear acciones para cruzar de izquierda a derecha
  actions_left_to_right <- list()
  for (i in seq_along(possible_moves)) {
    move <- possible_moves[[i]]
    misionero_move <- move[1]
    canibal_move <- move[2]
    
    action_name <- paste0("Mover ", misionero_move, "Misionero(s) y ", canibal_move, "Canibal(es) de Izquierda a Derecha")
    
    action_function <- function(state, misionero_move = misionero_move, canibal_move = canibal_move) {
      # Solo aplicable si la barca está en la orilla izquierda
      if (state[3] != 0) {
        return(NULL)
      }
      
      # Verificar si hay suficientes misioneros y caníbales en la orilla izquierda
      if (state[1] < misionero_move || state[2] < canibal_move) {
        return(NULL)
      }
      
      # Calcular nuevo estado
      new_state <- c(
        state[1] - misionero_move,       # Misioneros izquierda
        state[2] - canibal_move,         # Caníbales izquierda
              1,                         # Barca en derecha
        state[4] + misionero_move,       # Misioneros derecha
        state[5] + canibal_move          # Caníbales derecha
      )
      
      # Verificar si el nuevo estado es válido
      if (is_valid_state(new_state)) {
        return(new_state)
      } else {
        return(NULL)
      }
    }
    
    actions_left_to_right[[action_name]] <- action_function
  }
  
  # Crear acciones para cruzar de derecha a izquierda
  actions_right_to_left <- list()
  for (i in seq_along(possible_moves)) {
    move <- possible_moves[[i]]
    misionero_move <- move[1]
    canibal_move <- move[2]
    
    action_name <- paste0("Mover ", misionero_move, "Misionero(s) y ", canibal_move, "Canibal(es) de Derecha a Izquierda")
    
    action_function <- function(state, misionero_move = misionero_move, canibal_move = canibal_move) {
      # Solo aplicable si la barca está en la orilla derecha
      if (state[3] != 1) {
        return(NULL)
      }
      
      # Verificar si hay suficientes misioneros y caníbales en la orilla derecha
      if (state[4] < misionero_move || state[5] < canibal_move) {
        return(NULL)
      }
      
      # Calcular nuevo estado
      new_state <- c(
        state[1] + m_move,         # Misioneros izquierda
        state[2] + c_move,         # Caníbales izquierda
        0,                         # Barca en izquierda
        state[4] - m_move,         # Misioneros derecha
        state[5] - c_move          # Caníbales derecha
      )
      
      # Verificar si el nuevo estado es válido
      if (is_valid_state(new_state)) {
        return(new_state)
      } else {
        return(NULL)
      }
    }
    
    actions_right_to_left[[action_name]] <- action_function
  }
  
  problem$actions_possible <- c(actions_left_to_right, actions_right_to_left)

  # You can add additional attributes
  # problem$<additional_attribute>  <- <INSERT CODE HERE>

  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  result <- FALSE # Default value is FALSE.

  # <INSERT CODE HERE TO CHECK THE APPLICABILITY OF EACH ACTION>

  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.

  # <INSERT YOUR CODE HERE TO MODIFY CURRENT STATE>

  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  result <- FALSE # Default value is FALSE.

  # <INSERT YOUR CODE HERE TO CHECK WHETHER A STATE IS FINAL OR NOT>

  return(result)
}

# Transforms a state into a string
to.string = function (state, problem) {
  # <INSERT YOUR CODE HERE TO GENERATE A STRING THAT REPRESENTS THE STATE>
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  # <INSERT YOUR CODE HERE TO RETURN THE COST OF APPLYING THE ACTION ON THE STATE>

  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>

	return(1) # Default value is 1.
}
