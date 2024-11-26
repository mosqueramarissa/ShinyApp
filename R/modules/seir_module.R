# R/modules/seir_module.R

# UI del módulo SEIR
seirUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Panel de parámetros
      box(
        title = "Parámetros del Modelo Estocástico",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        
        # Parámetros
        sliderInput(ns("beta"), 
                    "Tasa de transmisión (\\(\\beta\\)):",
                    min = 0, max = 1, value = 0.3, step = 0.01),
        
        sliderInput(ns("sigma"), 
                    "Tasa de incubación (\\(\\sigma\\)):",
                    min = 0, max = 1, value = 0.2, step = 0.01),
        
        sliderInput(ns("gamma"), 
                    "Tasa de recuperación (\\(\\gamma\\):",
                    min = 0, max = 1, value = 0.1, step = 0.01),
        
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
        title = "Simulación del Modelo Estocástico",
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
                 h4("Tamaño Final"),
                 textOutput(ns("final_size")))
        )
      )
    ),
    
    # Panel de ecuaciones
    fluidRow(
      box(
        title = "Ecuaciones del Modelo Estocástico",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        
        withMathJax(
          HTML("
            <div style='font-size: 16px;'>
              <p>El modelo estocástico extiende el modelo SIR incluyendo un compartimento de Expuestos (E). Las ecuaciones son:</p>
              \\[
              \\begin{align*}
              \\frac{dS}{dt} &= -\\beta SI \\\\
              \\frac{dE}{dt} &= \\beta SI - \\sigma E \\\\
              \\frac{dI}{dt} &= \\sigma E - \\gamma I \\\\
              \\frac{dR}{dt} &= \\gamma I
              \\end{align*}
              \\]
              donde:
              <ul>
                <li>S(t) es la proporción de individuos susceptibles</li>
                <li>E(t) es la proporción de individuos expuestos (infectados pero no infecciosos)</li>
                <li>I(t) es la proporción de individuos infectados e infecciosos</li>
                <li>R(t) es la proporción de individuos recuperados</li>
                <li>β es la tasa de transmisión</li>
                <li>σ es la tasa de incubación (1/σ es el período de latencia)</li>
                <li>γ es la tasa de recuperación</li>
              </ul>
              <p>El número reproductivo básico \\(\\mathcal{R}_0\\) se calcula como:</p>
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

# Server del módulo SEIR
seirServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    results <- eventReactive(input$calculate, {
      req(input$beta, input$gamma, input$S0, input$I0)
      
      params <- list(
        beta = input$beta,
        sigma = input$sigma,
        gamma = input$gamma
      )
      
      init_cond <- c(
        S = input$S0, 
        E = input$E0,
        I = input$I0,
        R = 1 - input$S0 - input$E0 - input$I0
      )
      
      # Calcular R0
      r0 <- params$beta / params$gamma
      
      # Simulación
      sim_data <- run_simulation(
          model_type = "SEIR",
          parameters = params,
          times = seq(0, 200, by = 0.1),
          initial_conditions = init_cond
        )
      
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
    
    output$latent_period <- renderText({
      req(results())
      sprintf("%.1f", 1/results()$input$sigma)
    })
    
    output$infection_duration <- renderText({
      req(results())
      sprintf("%.1f", 1/results()$input$gamma)
    })
    
    output$plot <- renderPlotly({
      req(results())
      create_epidemic_plot(results()$sim_data, "SEIR")
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
    
    output$final_size <- renderText({
      req(results())
      data <- results()$sim_data
      final_R <- tail(data$R, 1)
      sprintf("%.1f%%", final_R * 100)
    })
    
    # Descarga de datos
    output$download <- downloadHandler(
      filename = function() {
        paste("seir_simulation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(results()$sim_data, file, row.names = FALSE)
      }
    )
    
    # Restablecer valores
    observeEvent(input$reset, {
      updateSliderInput(session, "beta", value = 0.3)
      updateSliderInput(session, "sigma", value = 0.2)
      updateSliderInput(session, "gamma", value = 0.1)
      updateSliderInput(session, "S0", value = 0.99)
      updateSliderInput(session, "E0", value = 0.01)
      updateSliderInput(session, "I0", value = 0)
    })
    
    # Validaciones
    observe({
      # Asegurarnos que los inputs existen
      req(input$beta, input$gamma, input$sigma, input$S0, input$E0, input$I0)
      
      # Validar R0
      if (R0() > 10) {
        showNotification(
          "El valor de R₀ es muy alto. Considere ajustar los parámetros.",
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
    })
  })
}