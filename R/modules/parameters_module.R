# R/modules/parameters_module.R

# UI del módulo de parámetros
parametersUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Descripción general de parámetros
      box(
        title = "Parámetros de los Modelos Epidemiológicos",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        HTML("
          <div class='parameters-intro'>
            <p>Los modelos epidemiológicos utilizan diversos parámetros para describir la dinámica de la enfermedad.
               Cada parámetro tiene un significado específico y su valor afecta directamente el comportamiento del modelo.</p>
          </div>
        ")
      )
    ),
    
    fluidRow(
      # Descripción detallada de cada parámetro
      box(
        title = "Tasa de Transmisión (β)",
        width = 6,
        status = "info",
        solidHeader = TRUE,
        
        HTML("
          <div class='parameter-description'>
            <h4>Definición</h4>
            <p>La tasa de transmisión (β) representa el número promedio de contactos adecuados por persona por unidad de tiempo.</p>
            
            <h4>Interpretación</h4>
            <ul>
              <li>Un valor alto indica una enfermedad muy contagiosa</li>
              <li>Se ve afectado por medidas de control como el distanciamiento social</li>
              <li>Típicamente entre 0.1 y 1.0 para muchas enfermedades</li>
            </ul>
            
            <h4>Ejemplo Interactivo</h4>
          </div>
        "),
        
        # Ejemplo interactivo para β
        sliderInput(ns("beta_example"), "Ajusta β:", 
                    min = 0, max = 1, value = 0.3, step = 0.1),
        plotlyOutput(ns("beta_effect"))
      ),
      
      box(
        title = "Tasa de Recuperación (γ)",
        width = 6,
        status = "info",
        solidHeader = TRUE,
        
        HTML("
          <div class='parameter-description'>
            <h4>Definición</h4>
            <p>La tasa de recuperación (γ) es el inverso del período infeccioso promedio.</p>
            
            <h4>Interpretación</h4>
            <ul>
              <li>1/γ es la duración promedio de la infección</li>
              <li>Se ve afectada por tratamientos y la naturaleza de la enfermedad</li>
              <li>Típicamente entre 0.1 y 0.5 para enfermedades agudas</li>
            </ul>
            
            <h4>Ejemplo Interactivo</h4>
          </div>
        "),
        
        # Ejemplo interactivo para γ
        sliderInput(ns("gamma_example"), "Ajusta γ:", 
                    min = 0, max = 0.5, value = 0.1, step = 0.05),
        plotlyOutput(ns("gamma_effect"))
      )
    ),
    
    fluidRow(
      box(
        title = "Tasa de Incubación (σ)",
        width = 6,
        status = "info",
        solidHeader = TRUE,
        
        HTML("
          <div class='parameter-description'>
            <h4>Definición</h4>
            <p>La tasa de incubación (σ) es el inverso del período de latencia promedio.</p>
            
            <h4>Interpretación</h4>
            <ul>
              <li>1/σ es el tiempo promedio que tarda un expuesto en volverse infeccioso</li>
              <li>Varía significativamente entre enfermedades</li>
              <li>Importante para medidas de cuarentena</li>
            </ul>
            
            <h4>Ejemplo Interactivo</h4>
          </div>
        "),
        
        # Ejemplo interactivo para σ
        sliderInput(ns("sigma_example"), "Ajusta σ:", 
                    min = 0, max = 1, value = 0.2, step = 0.1),
        plotlyOutput(ns("sigma_effect"))
      ),
      
      box(
        title = "Tasa de Pérdida de Inmunidad (ω)",
        width = 6,
        status = "info",
        solidHeader = TRUE,
        
        HTML("
          <div class='parameter-description'>
            <h4>Definición</h4>
            <p>La tasa de pérdida de inmunidad (ω) representa la velocidad a la que se pierde la protección inmune.</p>
            
            <h4>Interpretación</h4>
            <ul>
              <li>1/ω es la duración promedio de la inmunidad</li>
              <li>Relevante para enfermedades como la gripe</li>
              <li>Afecta la posibilidad de reinfección</li>
            </ul>
            
            <h4>Ejemplo Interactivo</h4>
          </div>
        "),
        
        # Ejemplo interactivo para ω
        sliderInput(ns("omega_example"), "Ajusta ω:", 
                    min = 0, max = 0.1, value = 0.01, step = 0.01),
        plotlyOutput(ns("omega_effect"))
      )
    ),
    
    fluidRow(
      box(
        title = "Número Reproductivo Básico (R₀)",
        width = 12,
        status = "warning",
        solidHeader = TRUE,
        
        HTML("
          <div class='r0-description'>
            <h4>Definición</h4>
            <p>R₀ es el número promedio de casos secundarios generados por un caso índice en una población totalmente susceptible.</p>
            
            <h4>Cálculo</h4>
            <div class='equation-box'>
              \\[ R_0 = \\frac{\\beta}{\\gamma} \\]
            </div>
            
            <h4>Interpretación</h4>
            <ul>
              <li>R₀ > 1: La epidemia crecerá</li>
              <li>R₀ = 1: La enfermedad será endémica</li>
              <li>R₀ < 1: La epidemia disminuirá</li>
            </ul>
          </div>
        ")
      )
    )
  )
}

# Server del módulo de parámetros
parametersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Efecto de β
    output$beta_effect <- renderPlotly({
      # Simular modelo SIR simple con β variable
      times <- seq(0, 100, by = 1)
      parameters <- list(
        beta = input$beta_example,
        gamma = 0.1
      )
      
      sim_data <- run_simulation("SIR", parameters, times)
      
      plot_ly(sim_data, x = ~time) %>%
        add_trace(y = ~I, name = "Infectados", type = "scatter", mode = "lines") %>%
        layout(
          title = paste("Efecto de β =", input$beta_example),
          xaxis = list(title = "Tiempo"),
          yaxis = list(title = "Proporción infectada")
        )
    })
    
    # Efecto de γ
    output$gamma_effect <- renderPlotly({
      times <- seq(0, 100, by = 1)
      parameters <- list(
        beta = 0.3,
        gamma = input$gamma_example
      )
      
      sim_data <- run_simulation("SIR", parameters, times)
      
      plot_ly(sim_data, x = ~time) %>%
        add_trace(y = ~I, name = "Infectados", type = "scatter", mode = "lines") %>%
        layout(
          title = paste("Efecto de γ =", input$gamma_example),
          xaxis = list(title = "Tiempo"),
                       yaxis = list(title = "Proporción infectada")
          )
    })
      
      # Efecto de σ
      output$sigma_effect <- renderPlotly({
        times <- seq(0, 100, by = 1)
        parameters <- list(
          beta = 0.3,
          gamma = 0.1,
          sigma = input$sigma_example
        )
        
        sim_data <- run_simulation("SEIR", parameters, times)
        
        plot_ly(sim_data, x = ~time) %>%
          add_trace(y = ~I, name = "Infectados", type = "scatter", mode = "lines") %>%
          add_trace(y = ~E, name = "Expuestos", type = "scatter", mode = "lines") %>%
          layout(
            title = paste("Efecto de σ =", input$sigma_example),
            xaxis = list(title = "Tiempo"),
            yaxis = list(title = "Proporción de la población")
          )
      })
      
      # Efecto de ω
      output$omega_effect <- renderPlotly({
        times <- seq(0, 200, by = 1)  # Tiempo más largo para ver el efecto de la pérdida de inmunidad
        parameters <- list(
          beta = 0.3,
          gamma = 0.1,
          sigma = 0.2,
          omega = input$omega_example
        )
        
        sim_data <- run_simulation("SEIRS", parameters, times)
        
        plot_ly(sim_data, x = ~time) %>%
          add_trace(y = ~S, name = "Susceptibles", type = "scatter", mode = "lines") %>%
          add_trace(y = ~I, name = "Infectados", type = "scatter", mode = "lines") %>%
          layout(
            title = paste("Efecto de ω =", input$omega_example),
            xaxis = list(title = "Tiempo"),
            yaxis = list(title = "Proporción de la población")
          )
      })
      
      # Observadores para información adicional
      observe({
        # Calcular R0 basado en los parámetros actuales
        R0_beta <- input$beta_example / 0.1  # gamma fijo en 0.1
        
        if (R0_beta > 1) {
          showNotification(
            sprintf("Con β = %.1f, R₀ = %.1f > 1: La epidemia crecerá", 
                    input$beta_example, R0_beta),
            type = "message",
            duration = 5
          )
        }
      })
      
      observe({
        # Mostrar información sobre el tiempo de recuperación
        recovery_time <- 1/input$gamma_example
        
        if (recovery_time > 14) {
          showNotification(
            sprintf("Tiempo de recuperación: %.1f días - Relativamente largo", 
                    recovery_time),
            type = "warning",
            duration = 5
          )
        }
      })
      
      observe({
        # Mostrar información sobre el período de incubación
        incubation_time <- 1/input$sigma_example
        
        if (incubation_time > 7) {
          showNotification(
            sprintf("Período de incubación: %.1f días - Relativamente largo", 
                    incubation_time),
            type = "warning",
            duration = 5
          )
        }
      })
      
      observe({
        # Mostrar información sobre la pérdida de inmunidad
        immunity_time <- 1/input$omega_example
        
        if (immunity_time < 180) {  # Menos de 6 meses
          showNotification(
            sprintf("Duración de la inmunidad: %.1f días - Relativamente corta", 
                    immunity_time),
            type = "warning",
            duration = 5
          )
        }
      })
      
      # Función auxiliar para calcular características epidémicas
      calculate_epidemic_characteristics <- reactive({
        # Para el ejemplo con β variable
        times <- seq(0, 100, by = 1)
        parameters <- list(
          beta = input$beta_example,
          gamma = 0.1
        )
        
        sim_data <- run_simulation("SIR", parameters, times)
        
        # Calcular características clave
        max_infected <- max(sim_data$I)
        time_to_peak <- sim_data$time[which.max(sim_data$I)]
        final_size <- tail(sim_data$R, 1)
        
        list(
          max_infected = max_infected,
          time_to_peak = time_to_peak,
          final_size = final_size
        )
      })
      
      # Mostrar tooltips informativos cuando los parámetros cambien significativamente
      observeEvent(input$beta_example, {
        chars <- calculate_epidemic_characteristics()
        showNotification(
          sprintf("Pico epidémico: %.1f%% a los %.1f días", 
                  chars$max_infected * 100, chars$time_to_peak),
          type = "message",
          duration = 3
        )
      })
      
      # Agregar leyendas explicativas a las gráficas
      observe({
        # Agregar anotaciones a las gráficas cuando sea relevante
        if (input$beta_example > 0.5) {
          showNotification(
            "Las tasas de transmisión altas pueden indicar la necesidad de intervenciones",
            type = "warning"
          )
        }
        
        if (input$gamma_example < 0.1) {
          showNotification(
            "Las tasas de recuperación bajas pueden sobrecargar el sistema de salud",
            type = "warning"
          )
        }
      })
  })
}