#server.R

server <- function(input, output, session) {
  
  # Llamar a los módulos (nueva sintaxis)
  introServer("intro")
  sirServer("sir1")
  sirStochServer("sirStoch")
  seirsServer("seirs1")
  parametersServer("params")
  
  if (!exists("run_simulation")) {
    stop("La función 'run_simulation' no está definida o no está disponible.")
  }
  
  # Valores reactivos compartidos
  times <- reactive({
    seq(0, 200, by = 0.1)
  })
  
  # Gráfico comparativo
  output$comparison_plot <- renderPlotly({
    req(input$beta_sir, input$gamma_sir,
        input$beta_stoch, input$gamma_stoch, input$mu_stoch, input$vaccination_rate_stoch,
        input$beta_seirs, input$sigma_seirs, input$gamma_seirs, input$omega_seirs)
    
    # Obtener datos de los modelos SIR y SIR Estocástico
    tryCatch({
    sir_data <- run_simulation("SIR", get_sir_parameters(), times())
    stoch_data <- run_simulation("SIR_STOCH", get_stoch_parameters(), times())
    seirs_data <- run_simulation("SEIRS", get_seirs_parameters(), times())
    
    plot_ly() %>%
      add_trace(data = sir_data, x = ~time, y = ~I, 
                name = "SIR", type = "scatter", mode = "lines") %>%
      add_trace(data = stoch_data, x = ~time, y = ~I, 
                name = "SIR Estocástico", type = "scatter", mode = "lines") %>%
      add_trace(data = seirs_data, x = ~time, y = ~I, 
                name = "SEIRS", type = "scatter", mode = "lines") %>%
      layout(
        title = "Comparación de Modelos",
        xaxis = list(title = "Tiempo"),
        yaxis = list(title = "Proporción de Infectados")
      )
    }, error = function(e) {
      showNotification(
        paste("Error al generar el gráfico comparativo: ", e$message),
        type = "error"
      )
      return(NULL)
    })
  })
  
  # Observador para validación de parámetros
  observe({
    req(input$sidebar_collapsed)
    tryCatch({
      validate_parameters()
    }, error = function(e) {
      showNotification(
        "Error en la validación de parámetros. Por favor, revise los valores ingresados.",
        type = "error"
      )
    })
  })
  
  # Funciones auxiliares
  get_sir_parameters <- reactive({
    req(input$beta_sir, input$gamma_sir)
    list(
      beta = as.numeric(input$beta_sir),
      gamma = as.numeric(input$gamma_sir)
    )
  })
  
  get_stoch_parameters <- reactive({
    req(input$beta_stoch, input$gamma_stoch, input$mu_stoch, input$vaccination_rate_stoch)
    list(
      beta = as.numeric(input$beta_stoch),
      gamma = as.numeric(input$gamma_stoch),
      mu = as.numeric(input$mu_stoch),
      vaccination_rate = as.numeric(input$vaccination_rate_stoch)
    )
  })
  
  get_seirs_parameters <- reactive({
    req(input$beta_seirs, input$sigma_seirs, input$gamma_seirs, input$omega_seirs)
    list(
      beta = input$beta_seirs,
      sigma = input$sigma_seirs,
      gamma = input$gamma_seirs,
      omega = input$omega_seirs
    )
  })
  
  
  # Función para calcular R0
  calculate_R0 <- function(model) {
    req(input[[paste0("beta_", model)]], input[[paste0("gamma_", model)]])
    as.numeric(input[[paste0("beta_", model)]]) / as.numeric(input[[paste0("gamma_", model)]])
  }
  
  # Función para validar parámetros
  validate_parameters <- function() {
    models <- c("sir", "sirStoch", "seirs1")
    for (model in models) {
      r0 <- calculate_R0(model)
      if (r0 > 10) {
        showNotification(
          sprintf("El valor de R₀ para el modelo %s es muy alto (%.2f). Considere ajustar los parámetros.",
                  toupper(model), r0),
          type = "warning",
          duration = 5
        )
      }
    }
  }
  
  # Cerrar conexiones al finalizar la sesión
  session$onSessionEnded(function() {
    # Limpieza de recursos
  })
}
