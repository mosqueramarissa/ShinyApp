# global.R

# Cargar librerías
library(shiny)
library(shinydashboard)
library(plotly)
library(deSolve)
library(markdown)


# Cargar Ecuaciones
source("R/models/epidemic_models.R")

# Cargar módulos
source("R/modules/intro_module.R")
source("R/modules/sir_module.R")
source("R/modules/sir_stoch_module.R")
source("R/modules/seirs_module.R")
source("R/modules/parameters_module.R")


# Configuraciones globales
options(scipen = 999)

# Constantes globales
SIMULATION_TIME <- 200
TIME_STEP <- 0.1
INITIAL_CONDITIONS <- list(
  sir = c(S = 0.99, I = 0.01, R = 0),
  seir = c(S = 0.99, E = 0.01, I = 0, R = 0),
  seirs = c(S = 0.99, E = 0.01, I = 0, R = 0)
)

# Paleta de colores para los gráficos
MODEL_COLORS <- list(
  S = "#1f77b4",  # Azul para Susceptibles
  E = "#2ca02c",  # Verde para Expuestos
  I = "#ff7f0e",  # Naranja para Infectados
  R = "#d62728"   # Rojo para Recuperados
)