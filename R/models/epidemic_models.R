# R/models/epidemic_models.R

library(R6)
library(deSolve)

# Clase base para modelos epidemiológicos
EpidemicModel <- R6Class("EpidemicModel",
                         public = list(
                           parameters = NULL,
                           initial_conditions = NULL,
                           
                           initialize = function(parameters = list(), initial_conditions = list()) {
                             self$parameters <- parameters
                             self$initial_conditions <- initial_conditions
                           },
                           
                           run = function(times) {
                             stop("Método abstracto - debe ser implementado por las subclases")
                           }
                         )
)

# Modelo SIR básico
SIRModel <- R6Class("SIRModel",
                    inherit = EpidemicModel,
                    public = list(
                      run = function(times) {
                        out <- ode(
                          y = self$initial_conditions,
                          times = times,
                          func = sir_equations,
                          parms = self$parameters
                        )
                        return(as.data.frame(out))
                      }
                    )
)

# Modelo SIR estocástico
SIRStochasticModel <- R6Class("SIRStochasticModel",
                              inherit = EpidemicModel,
                              public = list(
                                run = function(times, n_simulations = 50) {
                                  results_list <- vector("list", n_simulations)
                                  
                                  for(i in 1:n_simulations) {
                                    out <- ode(
                                      y = self$initial_conditions,
                                      times = times,
                                      func = sir_stochastic_equations,
                                      parms = self$parameters
                                    )
                                    sim_df <- as.data.frame(out)
                                    sim_df$simulation <- i
                                    results_list[[i]] <- sim_df
                                  }
                                  
                                  return(do.call(rbind, results_list))
                                }
                              )
)

# Modelo SEIRS
SEIRSModel <- R6Class("SEIRSModel",
                      inherit = EpidemicModel,
                      public = list(
                        run = function(times) {
                          out <- ode(
                            y = self$initial_conditions,
                            times = times,
                            func = seirs_equations,
                            parms = self$parameters
                          )
                          return(as.data.frame(out))
                        }
                      )
)

# Funciones de ecuaciones diferenciales
sir_equations <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

sir_stochastic_equations <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Añadir componente estocástico
    beta_stoch <- max(0, beta + rnorm(1, 0, beta_sd))
    gamma_stoch <- max(0, gamma + rnorm(1, 0, gamma_sd))
    mu_stoch <- max(0, mu + rnorm(1, 0, mu_sd))
    v_stoch <- max(0, vaccination_rate + rnorm(1, 0, v_sd))
    
    # Calcular N total
    N <- S + I + R
    
    # Ecuaciones diferenciales
    dS <- mu_stoch * N - beta_stoch * S * I/N - mu_stoch * S - v_stoch * S
    dI <- beta_stoch * S * I/N - gamma_stoch * I - mu_stoch * I
    dR <- gamma_stoch * I - mu_stoch * R + v_stoch * S
    
    list(c(dS, dI, dR))
  })
}

seirs_equations <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I + omega * R
    dE <- beta * S * I - sigma * E
    dI <- sigma * E - gamma * I
    dR <- gamma * I - omega * R
    list(c(dS, dE, dI, dR))
  })
}

# Función para ejecutar simulaciones
run_simulation <- function(model_type, parameters, times, initial_conditions = NULL) {
  # Crear modelo según el tipo
  model <- switch(model_type,
                  "SIR" = SIRModel$new(parameters, initial_conditions),
                  "SIR_STOCH" = SIRStochasticModel$new(parameters, initial_conditions),
                  "SEIRS" = SEIRSModel$new(parameters, initial_conditions)
  )
  
  # Ejecutar simulación
  return(model$run(times))
}

# Función para crear gráficos epidémicos
create_epidemic_plot <- function(data, model_type) {
  numeric_columns <- c("S", "E", "I", "R")
  for (col in numeric_columns) {
    if (col %in% names(data)) {
    data[[col]] <- as.numeric(data[[col]])
    }
  }
  
  data[is.na(data)] <-
  
  # Colores para cada compartimento
  colors <- c(
    S = "#1f77b4",  # Azul para Susceptibles
    E = "#2ca02c",  # Verde para Expuestos
    I = "#ff7f0e",  # Naranja para Infectados
    R = "#d62728"   # Rojo para Recuperados
  )
  
  
  # Crear gráfico base
  p <- plot_ly(data, x = ~time)
  
  # Agregar trazas solo si la columna correspondiente existe en los datos
  if ("S" %in% names(data)) {
    p <- p %>%
      add_trace(y = ~S, name = "Susceptibles",
                type = "scatter", mode = "lines",
                line = list(color = colors["S"]))
  }
  if ("E" %in% names(data)) {
    p <- p %>%
      add_trace(y = ~E, name = "Expuestos",
                type = "scatter", mode = "lines",
                line = list(color = colors["E"]))
  }
  if ("I" %in% names(data)) {
    p <- p %>%
      add_trace(y = ~I, name = "Infectados",
                type = "scatter", mode = "lines",
                line = list(color = colors["I"]))
  }
  if ("R" %in% names(data)) {
    p <- p %>%
      add_trace(y = ~R, name = "Recuperados",
                type = "scatter", mode = "lines",
                line = list(color = colors["R"]))
  }
  
  # Configurar diseño
  p <- p %>% layout(
    title = paste("Modelo", model_type),
    xaxis = list(title = "Tiempo"),
    yaxis = list(title = "Proporción de la población",
                 range = c(0, 1)),
    hovermode = "x unified"
  )
  
  return(p)
}
