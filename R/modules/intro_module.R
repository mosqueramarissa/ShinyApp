# R/modules/intro_module.R

# UI del módulo de introducción
introUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Introducción General
      box(
        title = "Modelos Epidemiológicos",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        HTML("
          <div class='intro-section'>
            <h3>Introducción a los Modelos Epidemiológicos</h3>
            <p>Los modelos epidemiológicos son herramientas matemáticas fundamentales para comprender y 
               predecir la propagación de enfermedades infecciosas en poblaciones. Estos modelos han sido 
               especialmente relevantes durante eventos como la pandemia de COVID-19, ayudando a informar 
               políticas de salud pública y estrategias de intervención.</p>
            
            <h4>Historia y Desarrollo</h4>
            <p>El desarrollo de modelos epidemiológicos matemáticos comenzó a principios del siglo XX. 
               Algunos hitos importantes incluyen:</p>
            <ul>
              <li>1766: Daniel Bernoulli desarrolla el primer modelo matemático para la viruela</li>
              <li>1906: Hamer introduce modelos de transmisión de masa</li>
              <li>1927: Kermack y McKendrick desarrollan el modelo SIR clásico</li>
            </ul>

            <h4>Modelos Principales</h4>
            <div class='model-overview'>
              <h5>Modelo SIR</h5>
              <p>El modelo más básico que divide la población en tres compartimentos:</p>
              <ul>
                <li><strong>Susceptibles (S):</strong> Individuos que pueden contraer la enfermedad</li>
                <li><strong>Infectados (I):</strong> Individuos que tienen la enfermedad y pueden transmitirla</li>
                <li><strong>Recuperados (R):</strong> Individuos que se han recuperado y son inmunes</li>
              </ul>

              <h5>Modelo SEIR</h5>
              <p>Extensión del modelo SIR que incluye:</p>
              <ul>
                <li><strong>Expuestos (E):</strong> Individuos que han sido infectados pero aún no son infecciosos</li>
                <li>Especialmente útil para enfermedades con período de incubación significativo</li>
              </ul>

              <h5>Modelo SEIRS</h5>
              <p>Variante que considera la pérdida de inmunidad:</p>
              <ul>
                <li>Incluye la posibilidad de que individuos recuperados vuelvan a ser susceptibles</li>
                <li>Relevante para enfermedades donde la inmunidad es temporal</li>
              </ul>
            </div>

            <h4>Aplicaciones en Salud Pública</h4>
            <p>Los modelos epidemiológicos son fundamentales para:</p>
            <ul>
              <li>Predecir la propagación de enfermedades</li>
              <li>Evaluar estrategias de intervención</li>
              <li>Optimizar la asignación de recursos</li>
              <li>Informar políticas de salud pública</li>
            </ul>

            <h4>Parámetros Clave</h4>
            <p>Los modelos utilizan diversos parámetros para describir la dinámica de la enfermedad:</p>
            <ul>
              <li><strong>β (beta):</strong> Tasa de transmisión</li>
              <li><strong>γ (gamma):</strong> Tasa de recuperación</li>
              <li><strong>σ (sigma):</strong> Tasa de incubación (en modelos SEIR/SEIRS)</li>
              <li><strong>ω (omega):</strong> Tasa de pérdida de inmunidad (en modelo SEIRS)</li>
            </ul>

            <h4>Limitaciones y Consideraciones</h4>
            <p>Es importante tener en cuenta que:</p>
            <ul>
              <li>Los modelos son simplificaciones de la realidad</li>
              <li>La precisión depende de la calidad de los datos y parámetros</li>
              <li>Las predicciones son más confiables a corto plazo</li>
              <li>Los factores sociales y comportamentales pueden afectar significativamente los resultados</li>
            </ul>
          </div>
        ")
      )
    )
  )
}

# Server del módulo de introducción
introServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Este módulo es puramente informativo
    # No requiere lógica de servidor
  })
}