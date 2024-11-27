# R/modules/sir_stoch_module.R

# UI del módulo
sirStochUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4,
             wellPanel(
               # Parámetros básicos
               numericInput(ns("beta_stoch"), "Tasa de infección (β):",
                            value = 0.3, min = 0, max = 1, step = 0.01),
               numericInput(ns("gamma_stoch"), "Tasa de recuperación (γ):",
                            value = 1/14, min = 0, max = 1, step = 0.01),
               
               # Parámetros de vacunación y demografía
               numericInput(ns("vaccination_rate_stoch"), "Tasa de vacunación:",
                            value = 0.001, min = 0, max = 1, step = 0.001),
               numericInput(ns("mu_stoch"), "Tasa de nacimiento/muerte:",
                            value = 1/(76*365), min = 0, max = 0.01, step = 0.0001),
               
               # Parámetros de variabilidad
               hr(),
               h4("Parámetros de variabilidad"),
               numericInput(ns("beta_sd_stoch"), "Desviación estándar de β:",
                            value = 0.03, min = 0, max = 0.1, step = 0.01),
               numericInput(ns("gamma_sd_stoch"), "Desviación estándar de γ:",
                            value = 0.01, min = 0, max = 0.1, step = 0.01),
               numericInput(ns("mu_sd_stoch"), "Desviación estándar de μ:",
                            value = 0.0001, min = 0, max = 0.01, step = 0.0001),
               numericInput(ns("v_sd_stoch"), "Desviación estándar de vacunación:",
                            value = 0.0001, min = 0, max = 0.01, step = 0.0001),
               
               # Parámetros de simulación
               hr(),
               h4("Configuración de simulación"),
               numericInput(ns("n_simulations"), "Número de simulaciones:",
                            value = 50, min = 1, max = 100, step = 1),
               numericInput(ns("t_max"), "Tiempo máximo (días):",
                            value = 365, min = 1, max = 1000),
               numericInput(ns("population"), "Tamaño de población:",
                            value = 10000, min = 100, max = 1000000),
               
               actionButton(ns("run_sim"), "Ejecutar Simulación",
                            class = "btn-primary btn-block")
             )
      ),
      column(8,
             plotlyOutput(ns("stoch_plot"), height = "600px"),
             hr(),
             h4("Estadísticas de la simulación"),
             verbatimTextOutput(ns("sim_stats"))
      )
    )
  )
}

# Server del módulo
sirStochServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Función reactiva para obtener parámetros
    get_stoch_parameters <- reactive({
      list(
        beta = input$beta_stoch,
        gamma = input$gamma_stoch,
        mu = input$mu_stoch,
        vaccination_rate = input$vaccination_rate,
        beta_sd = input$beta_sd_stoch,
        gamma_sd = input$gamma_sd_stoch,
        mu_sd = input$mu_sd_stoch,
        v_sd = input$v_sd_stoch,
        n_simulations = input$n_simulations
      )
    })
    
    # Función reactiva para ejecutar simulación
    sim_results <- eventReactive(input$run_sim, {
      # Mostrar mensaje de progreso
      withProgress(message = 'Ejecutando simulaciones', value = 0, {
        
        params <- get_stoch_parameters()
        times <- seq(0, input$t_max, by = 0.1)
        
        # Estado inicial
        initial_state <- c(
          S = input$population - 1,
          I = 1,
          R = 0
        )
        
        # Ejecutar simulaciones
        results <- run_simulation(
          model_type = "SIR_STOCH",
          parameters = params,
          times = times,
          initial_conditions = initial_state
        )
        
        return(results)
      })
    })
    
    # Generar gráfico
    output$stoch_plot <- renderPlotly({
      req(sim_results())
      
      data <- sim_results()
      
      # Calcular estadísticas sumarias
      summary_data <- data %>%
        group_by(time) %>%
        summarise(
          S_mean = mean(S),
          I_mean = mean(I),
          R_mean = mean(R),
          S_lower = quantile(S, 0.025),
          S_upper = quantile(S, 0.975),
          I_lower = quantile(I, 0.025),
          I_upper = quantile(I, 0.975),
          R_lower = quantile(R, 0.025),
          R_upper = quantile(R, 0.975)
        )
      
      # Crear gráfico con plotly
      plot_ly() %>%
        # Bandas de confianza
        add_ribbons(data = summary_data, x = ~time, 
                    ymin = ~S_lower, ymax = ~S_upper,
                    name = "S IC 95%", fillcolor = "rgba(31,119,180,0.2)", 
                    line = list(width = 0), showlegend = FALSE) %>%
        add_ribbons(data = summary_data, x = ~time, 
                    ymin = ~I_lower, ymax = ~I_upper,
                    name = "I IC 95%", fillcolor = "rgba(255,127,14,0.2)", 
                    line = list(width = 0), showlegend = FALSE) %>%
        add_ribbons(data = summary_data, x = ~time, 
                    ymin = ~R_lower, ymax = ~R_upper,
                    name = "R IC 95%", fillcolor = "rgba(44,160,44,0.2)", 
                    line = list(width = 0), showlegend = FALSE) %>%
        # Líneas medias
        add_trace(data = summary_data, x = ~time, y = ~S_mean, 
                  name = "Susceptibles", type = "scatter", mode = "lines",
                  line = list(color = "rgb(31,119,180)", width = 2)) %>%
        add_trace(data = summary_data, x = ~time, y = ~I_mean, 
                  name = "Infectados", type = "scatter", mode = "lines",
                  line = list(color = "rgb(255,127,14)", width = 2)) %>%
        add_trace(data = summary_data, x = ~time, y = ~R_mean, 
                  name = "Recuperados", type = "scatter", mode = "lines",
                  line = list(color = "rgb(44,160,44)", width = 2)) %>%
        layout(
          title = "Simulación SIR Estocástica con Vacunación",
          xaxis = list(title = "Tiempo (días)"),
          yaxis = list(title = "Número de individuos"),
          hovermode = "x unified"
        )
    })
    
    # Mostrar estadísticas
    output$sim_stats <- renderPrint({
      req(sim_results())
      
      data <- sim_results()
      
      # Calcular estadísticas relevantes
      stats <- data %>%
        group_by(time) %>%
        summarise(
          I_max = max(I_mean),
          tiempo_pico = time[which.max(I_mean)],
          total_infectados = max(R_mean),
          prop_no_infectados = last(S_mean)/sum(c(last(S_mean), last(I_mean), last(R_mean)))
        ) %>%
        unique()
      
      cat("Estadísticas de la epidemia:\n")
      cat("Pico máximo de infectados:", round(stats$I_max[1]), "individuos\n")
      cat("Tiempo hasta el pico:", round(stats$tiempo_pico[1]), "días\n")
      cat("Total de infectados al final:", round(stats$total_infectados[1]), "individuos\n")
      cat("Proporción que escapó a la infección:", round(stats$prop_no_infectados[1] * 100, 1), "%\n")
    })
    
  })
}
