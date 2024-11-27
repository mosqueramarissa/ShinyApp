# ui.R
ui <- dashboardPage(
  
  skin = "green",
  
  # Header
  dashboardHeader(
    title = "Modelos Epidemiológicos",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar_collapse",
      menuItem("Introducción", tabName = "intro"),
      menuItem("Modelos", tabName = "models",
               menuSubItem("Modelo SIR Determinista", tabName = "sir"),
               menuSubItem("Modelo SIR Estocástico", tabName = "sirStoch"),
               menuSubItem("Modelo SEIRS", tabName = "seirs")
      ),
      menuItem("Parámetros", tabName = "parameters"),
      menuItem("Comparación", tabName = "comparison"),
      menuItem("Referencias", tabName = "references")
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", 
                href = "custom.css")
    ),
    
    tabItems(
      # Tab Introducción
      tabItem(tabName = "intro",
              introUI("intro")
      ),
      
      # Tab Modelo SIR
      tabItem(tabName = "sir",
              sirUI("sir1")
      ),
      
      # Tab Modelo SIR Stochastic
      tabItem(tabName = "sir estocástico",
              sirStochUI("sirStoch")
      ),
      
      # Tab Modelo SEIRS
      tabItem(tabName = "seirs",
              seirsUI("seirs1")
      ),
      
      # Tab Parámetros
      tabItem(tabName = "parameters",
              parametersUI("params")
      ),
      
      # Tab Comparación
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  title = "Comparación de Modelos",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  includeMarkdown("www/md/comparison.md")
                )
              ),
              fluidRow(
                box(
                  title = "Visualización Comparativa",
                  width = 12,
                  solidHeader = TRUE,
                  plotlyOutput("comparison_plot", height = "600px")
                )
              )
      ),
      
      # Tab Referencias
      tabItem(tabName = "references",
              fluidRow(
                box(
                  title = "Referencias Bibliográficas",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  includeMarkdown("www/md/references.md")
                )
              )
      )
    )
  )
)
