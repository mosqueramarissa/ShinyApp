# global.R

# Cargar librerías
library(shiny)
library(shinydashboard)
library(plotly)
library(deSolve)
library(markdown)
library(dplyr)
library(tidyr)
library(R6)


# Cargar Ecuaciones
source("R/models/epidemic_models.R")

# Cargar módulos
source("R/modules/intro_module.R")
source("R/modules/sir_module.R")
source("R/modules/sir_stoch_module.R")
source("R/modules/seirs_module.R")
source("R/modules/parameters_module.R")

# Definición
run_simulation <- function(model_type, parameters, times, initial_conditions) {
  stop("La función 'run_simulation' no está implementada.")
}


# Configuraciones globales
options(scipen = 999)

# Validar parámetros de entrada
validate_parameters <- function(parameters) {
  if (!is.list(parameters)) stop("Los parámetros deben ser una lista.")
  required_fields <- c("beta", "gamma", "mu", "vaccination_rate")
  missing <- setdiff(required_fields, names(parameters))
  if (length(missing) > 0) {
    stop(paste("Faltan los siguientes campos en los parámetros:", paste(missing, collapse = ", ")))
  }
}

# Paleta de colores para los gráficos
colors <- list(
  S = "#1f77b4",  # Azul para Susceptibles
  E = "#2ca02c",  # Verde para Expuestos
  I = "#ff7f0e",  # Naranja para Infectados
  R = "#d62728"   # Rojo para Recuperados
)
