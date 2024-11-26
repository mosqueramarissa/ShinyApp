# R/modules/sir_module.R

# UI del módulo SIR
sirUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Panel de parámetros
      box(
        title = "Parámetros del Modelo SIR",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        
        # Parámetros
        sliderInput(ns("beta"), 
                    "Tasa de transmisión (β):",
                    min = 0, max = 1, value = 0.3, step = 0.01),
        
        sliderInput(ns("gamma"), 
                    "Tasa de recuperación (γ):",
                    min = 0, max = 1, value = 0.1, step = 0.01),
        
        # Condiciones iniciales
        hr(),
        h4("Condiciones Iniciales"),
        
        sliderInput(ns("S0"), 
                    "Proporción inicial de Susceptibles:",
                    min = 0, max = 1, value = 0.99, step = 0.01),
        
        sliderInput(ns("I0"),
                    "Proporción inicial de Infectados:",
                    min = 0, max = 1, value = 0.01, step = 0.01),
        
        # Información calculada
        hr(),
        h4("Información del Modelo"),
        "$R_0$ = ", textOutput(ns("r0"), inline = TRUE),
        br(),
        "Duración media de la infección = ", 
        textOutput(ns("infection_duration"), inline = TRUE), " días",
        
        # Botones de acción
        hr(),
        div(
          class = "text-center",
          actionButton(
            ns("calculate"),
            "Calcular",
            icon = icon("calculator"),
            class = "btn-primary",
            style = "margin-right: 10px;"
          ),
          downloadButton(ns("download"), "Descargar Datos"),
          actionButton(ns("reset"), "Restablecer Valores")
        )
      ),
      
      # Panel de gráfico
      box(
        title = "Simulación del Modelo SIR",
        width = 8,
        status = "primary",
        solidHeader = TRUE,
        
        plotlyOutput(ns("plot")),
        
        hr(),
        fluidRow(
          column(4, 
                 h4("Pico de la Epidemia"),
                 textOutput(ns("peak_info"))),
          column(4, 
                 h4("Tamaño Final"),
                 textOutput(ns("final_size"))),
          column(4,
                 h4("Tiempo al Pico"),
                 textOutput(ns("time_to_peak")))
        )
      )
    ),
    
    # Panel de ecuaciones
    fluidRow(
      box(
        title = "Ecuaciones del Modelo SIR",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        
        withMathJax(
          HTML("
            <div style='font-size: 16px;'>
              <p>El modelo SIR está definido por el siguiente sistema de ecuaciones diferenciales:</p>
              \\[
              \\begin{align*}
              \\frac{dS}{dt} &= -\\beta SI \\\\
              \\frac{dI}{dt} &= \\beta SI - \\gamma I \\\\
              \\frac{dR}{dt} &= \\gamma I
              \\end{align*}
              \\]
              donde:
              <ul>
                <li>S(t) es la proporción de individuos susceptibles</li>
                <li>I(t) es la proporción de individuos infectados</li>
                <li>R(t) es la proporción de individuos recuperados</li>
                <li>$\beta$ es la tasa de transmisión</li>
                <li>γ es la tasa de recuperación</li>
              </ul>
              <p>El número reproductivo básico R₀ se calcula como:</p>
              \\[
              R_0 = \\frac{\\beta}{\\gamma}
              \\]
            </div>
          ")
        )
      )
    )
  )
}

# Server del módulo SIR
sirServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Valores reactivos que dependen del botón calcular
    results <- eventReactive(input$calculate, {
      print("Botón Calcular presionado")
      
      # Validar inputs antes de calcular
      req(input$beta, input$gamma, input$S0, input$I0)
      print(paste("Beta:", input$beta))
      print(paste("Gamma:", input$gamma))
      print(paste("S0:", input$S0))
      print(paste("I0:", input$I0))
      
      
      # Realizar cálculos
      params <- list(
        beta = input$beta,
        gamma = input$gamma
      )
      
      init_cond <- c(
        S = input$S0,
        I = input$I0,
        R = 1 - as.numeinput$S0 - input$I0
      )
      
      # Calcular R0
      r0 <- params$beta / params$gamma
      
      # Ejecutar simulación
      sim_data <- run_simulation(
        model_type = "SIR",
        parameters = params,
        times = seq(0, 200, by = 0.1),
        initial_conditions = init_cond
      )
      
      print("Resultados de simulación:")
      print(head(sim_results))
      
      return(sim_results)
      
      list(
        sim_data = sim_data,
        r0 = r0,
        infection_duration = 1/input$gamma
      )
    })

    
    # Outputs que dependen de los resultados
    output$r0 <- renderText({
      req(results())
      sprintf("%.2f", results()$r0)
    })
    
    output$infection_duration <- renderText({
      req(results())
      sprintf("%.1f", results()$infection_duration)
    })
    
    output$plot <- renderPlotly({
      print("Intentando generar gráfico")
      req(results())
      print("Datos disponibles para gráfico:")
      print(head(results()))
      
      p <- create_epidemic_plot(results(), "SIR")
      print("Gráfico creado")
      return(p)
    })
    
    
    # Información epidémica
    output$peak_info <- renderText({
      req(results())
      data <- results()$sim_data
      max_I <- max(data$I)
      sprintf("%.1f%%", max_I * 100)
    })
    
    output$final_size <- renderText({
      req(results())
      data <- results()$sim_data
      final_R <- tail(data$R, 1)
      sprintf("%.1f%%", final_R * 100)
    })
    
    output$time_to_peak <- renderText({
      req(results())
      data <- results()$sim_data
      peak_time <- data$time[which.max(data$I)]
      sprintf("%.1f días", peak_time)
    })
    
    # Descarga de datos
    output$download <- downloadHandler(
      filename = function() {
        paste("sir_simulation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
      },
      content = function(file) {
        req(results())
        write.csv(results()$sim_data, file, row.names = FALSE)
      }
    )
    
    # Restablecer valores
    observeEvent(input$reset, {
      updateSliderInput(session, "beta", value = 0.3)
      updateSliderInput(session, "gamma", value = 0.1)
      updateSliderInput(session, "S0", value = 0.99)
      updateSliderInput(session, "I0", value = 0.01)
    })
    
    # Mostrar mensaje cuando se presiona calcular
    observeEvent(input$calculate, {
      showNotification(
        "Calculando resultados...",
        type = "message",
        duration = 2
      )
    })
    
    # Validaciones
    observe({
      # Asegurarnos que los inputs existen
      req(input$beta, input$gamma, input$S0, input$I0)
      
      # Validar R0
      if (R0() > 10) {
        showNotification(
          "El valor de $R_0$ es muy alto. Considere ajustar los parámetros.",
          type = "warning"
        )
      }
      
      # Validar condiciones iniciales
      total <- input$S0 + input$I0
      if (total > 1) {
        showNotification(
          "La suma de las proporciones iniciales no puede ser mayor que 1",
          type = "error"
        )
      }
    })
  })
}