# R/modules/seirs_module.R

# UI del módulo SEIRS
seirsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Panel de parámetros
      box(
        title = "Parámetros del Modelo SEIRS",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        withMathJax(),
        
        # Parámetros
        sliderInput(ns("beta"), 
                    "Tasa de transmisión (\\(\\beta\\)):",
                    min = 0, max = 1, value = 0.3, step = 0.01),
        
        sliderInput(ns("sigma"), 
                    "Tasa de incubación (\\(\\sigma\\)):",
                    min = 0, max = 1, value = 0.2, step = 0.01),
        
        sliderInput(ns("gamma"), 
                    "Tasa de recuperación (\\(\\gamma\\)):",
                    min = 0, max = 1, value = 0.1, step = 0.01),
        
        sliderInput(ns("omega"), 
                    "Tasa de pérdida de inmunidad (\\(\\omega\\)):",
                    min = 0, max = 0.1, value = 0.01, step = 0.001),
        
        # Condiciones iniciales
        hr(),
        h4("Condiciones Iniciales"),
        
        sliderInput(ns("S0"), 
                    "Proporción inicial de Susceptibles:",
                    min = 0, max = 1, value = 0.99, step = 0.01),
        
        sliderInput(ns("E0"),
                    "Proporción inicial de Expuestos:",
                    min = 0, max = 1, value = 0.01, step = 0.01),
        
        sliderInput(ns("I0"),
                    "Proporción inicial de Infectados:",
                    min = 0, max = 1, value = 0, step = 0.01),
        
        # Información calculada
        hr(),
        h4("Información del Modelo"),
        "\\(\\mathcal{R}_0\\) = ", textOutput(ns("r0"), inline = TRUE),
        br(),
        "Período de incubación = ", 
        textOutput(ns("latent_period"), inline = TRUE), " días",
        br(),
        "Duración media de la infección = ", 
        textOutput(ns("infection_duration"), inline = TRUE), " días",
        br(),
        "Duración media de la inmunidad = ",
        textOutput(ns("immunity_duration"), inline = TRUE), " días",
        
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
        title = "Simulación del Modelo SEIRS",
        width = 8,
        status = "primary",
        solidHeader = TRUE,
        
        plotlyOutput(ns("plot")),
        
        hr(),
        fluidRow(
          column(3, 
                 h4("Pico de Expuestos"),
                 textOutput(ns("peak_exposed"))),
          column(3, 
                 h4("Pico de Infectados"),
                 textOutput(ns("peak_infected"))),
          column(3,
                 h4("Tiempo al Pico"),
                 textOutput(ns("time_to_peak"))),
          column(3,
                 h4("Estado Endémico"),
                 textOutput(ns("endemic_state")))
        )
      )
    ),
    
    # Panel de ecuaciones
    fluidRow(
      box(
        title = "Ecuaciones del Modelo SEIRS",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        
        withMathJax(
          HTML("
            <div style='font-size: 16px;'>
              <p>El modelo SEIRS extiende el modelo SEIR permitiendo la pérdida de inmunidad. Las ecuaciones son:</p>
              \\[
              \\begin{align*}
              \\frac{dS}{dt} &= -\\beta SI + \\omega R \\\\
              \\frac{dE}{dt} &= \\beta SI - \\sigma E \\\\
              \\frac{dI}{dt} &= \\sigma E - \\gamma I \\\\
              \\frac{dR}{dt} &= \\gamma I - \\omega R
              \\end{align*}
              \\]
              donde:
              <ul>
                <li>S(t) es la proporción de individuos susceptibles</li>
                <li>E(t) es la proporción de individuos expuestos</li>
                <li>I(t) es la proporción de individuos infectados</li>
                <li>R(t) es la proporción de individuos recuperados</li>
                <li>β es la tasa de transmisión</li>
                <li>σ es la tasa de incubación</li>
                <li>γ es la tasa de recuperación</li>
                <li>ω es la tasa de pérdida de inmunidad</li>
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

# Server del módulo SEIRS
seirsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Valores reactivos que dependen del botón calcular
    results <- eventReactive(input$calculate, {
      browser()
      # Validar inputs antes de calcular
      req(input$beta, input$sigma, input$gamma, input$omega)
      
      # Realizar cálculos
      parameters <- list(
        beta = input$beta,
        sigma = input$sigma,
        gamma = input$gamma,
        omega = input$omega
      )
      
      init_cond <-c(
        S = input$S0, 
        E = input$E0,
        I = input$I0,
        R = 1 - input$S0 - input$E0 - input$I0
      )
      
      # Calcular R0
      r0 <- params$beta / params$gamma
      
      # Simulación
      sim_data <- run_simulation(
          model_type = "SEIRS",
          parameters = params,
          times = seq(0, 500, by = 0.1), # Tiempo más largo para ver comportamiento endémico
          initial_conditions = init_cond
        )
      
      list(
        sim_data = sim_data,
        r0 = r0,
        infection_duration = 1/input$gamma
      )
    })
  
    
    # Outputs
    output$r0 <- renderText({
      req(results())
      sprintf("%.2f", results()$r0)
    })
    
    output$latent_period <- renderText({
      req(results())
      sprintf("%.1f", 1/results()$input$sigma)
    })
    
    output$infection_duration <- renderText({
      req(results())
      sprintf("%.1f", 1/results()$input$gamma)
    })
    
    output$immunity_duration <- renderText({
      req(results())
      sprintf("%.1f", 1/results()$input$omega)
    })
    output$plot <- renderPlotly({
      req(results())
      create_epidemic_plot(results()$sim_data, "SEIRS")
    })
    
    # Información epidémica
    output$peak_exposed <- renderText({
      req(results()) 
      data <- results()$sim_data
      max_E <- max(data$E)
      sprintf("%.1f%%", max_E * 100)
    })
    
    output$peak_infected <- renderText({
      req(results()) 
      data <- results()$sim_data
      max_I <- max(data$I)
      sprintf("%.1f%%", max_I * 100)
    })
    
    output$time_to_peak <- renderText({
      req(results()) 
      data <- results()$sim_data
      peak_time <- data$time[which.max(data$I)]
      sprintf("%.1f días", peak_time)
    })
    
    output$endemic_state <- renderText({
      req(results()) 
      data <- results()$sim_data
      # Tomar el promedio de los últimos 10% de puntos para el estado endémico
      n_points <- nrow(data)
      last_points <- tail(data, round(n_points * 0.1))
      endemic_I <- mean(last_points$I)
      sprintf("%.1f%%", endemic_I * 100)
    })
    
    # Descarga de datos
    output$download <- downloadHandler(
      filename = function() {
        paste("seirs_simulation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
      },
      content = function(file) {
        req(results())
        write.csv(results()$sim_data, file, row.names = FALSE)
      }
    )
    
    # Restablecer valores
    observeEvent(input$reset, {
      updateSliderInput(session, "beta", value = 0.3)
      updateSliderInput(session, "sigma", value = 0.2)
      updateSliderInput(session, "gamma", value = 0.1)
      updateSliderInput(session, "omega", value = 0.01)
      updateSliderInput(session, "S0", value = 0.99)
      updateSliderInput(session, "E0", value = 0.01)
      updateSliderInput(session, "I0", value = 0)
    })
    
    # Validaciones
    observe({
      # Asegurarnos que los inputs existen
      req(input$beta, input$gamma, input$sigma, input$omega, 
          input$S0, input$E0, input$I0)
      
      # Validar R0
      if (r0() > 10) {
        showNotification(
          "El valor de \\(\\mathcal{R}_0\\) es muy alto. Considere ajustar los parámetros.",
          type = "warning"
        )
      }
      
      # Validar condiciones iniciales
      total <- input$S0 + input$E0 + input$I0
      if (total > 1) {
        showNotification(
          "La suma de las proporciones iniciales no puede ser mayor que 1.",
          type = "error"
        )
      }
      
      # Validar tasa de pérdida de inmunidad
      if (input$omega > input$gamma) {
        showNotification(
          "La tasa de pérdida de inmunidad es mayor que la tasa de recuperación. 
       Esto podría llevar a resultados poco realistas.",
          type = "warning"
        )
      }
    })
    
    # Modifica también los observeEvent de tooltips
    observeEvent(input$beta, {
      req(input$beta)
      if (input$beta > 0.5) {
        showNotification(
          "Una tasa de transmisión alta indica una enfermedad muy contagiosa.",
          type = "message"
        )
      }
    })
    
    observeEvent(input$sigma, {
      req(input$sigma)
      if (1/input$sigma > 14) {
        showNotification(
          "El período de incubación es bastante largo. 
       Esto es típico de enfermedades como la hepatitis.",
          type = "message"
        )
      }
    })
    
    observeEvent(input$gamma, {
      req(input$gamma)
      if (1/input$gamma > 30) {
        showNotification(
          "El período de infección es muy largo. 
       Esto es característico de enfermedades crónicas.",
          type = "message"
        )
      }
    })
    
    # Cálculos adicionales para análisis
    reactive_analysis <- reactive({
      # Obtener datos de la simulación
      data <- results()$sim_data
      
      # Calcular métricas clave
      peak_time <- data$time[which.max(data$I)]
      peak_infected <- max(data$I)
      total_infected <- max(data$R)
      
      # Estado endémico (si existe)
      endemic_exists <- input$omega > 0
      endemic_level <- if (endemic_exists) {
        tail(data$I, 1)
      } else {
        0
      }
      
      # Tiempo hasta control (I < 1%)
      control_time <- min(data$time[data$I < 0.01])
      
      list(
        peak_time = peak_time,
        peak_infected = peak_infected,
        total_infected = total_infected,
        endemic_exists = endemic_exists,
        endemic_level = endemic_level,
        control_time = control_time
      )
    })
    
    # Mostrar análisis si se solicita
    output$analysis <- renderPrint({
      analysis <- reactive_analysis()
      cat("Análisis detallado del modelo SEIRS:\n\n")
      cat(sprintf("Tiempo hasta el pico: %.1f días\n", analysis$peak_time))
      cat(sprintf("Pico de infectados: %.1f%%\n", analysis$peak_infected * 100))
      cat(sprintf("Total infectados: %.1f%%\n", analysis$total_infected * 100))
      if (analysis$endemic_exists) {
        cat(sprintf("Nivel endémico: %.1f%%\n", analysis$endemic_level * 100))
      }
      cat(sprintf("Tiempo hasta control (<1%%): %.1f días\n", analysis$control_time))
    })
  })
}