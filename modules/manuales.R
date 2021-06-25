manuales_ui <- function(id) {
  ns <- NS(id)

  archivos <- c(
    "Cargar datos" = "cargar_datos.md",
    "Filtros y opciones adicionales" = "filtros_opciones.md",
    "Agrupadores"  = "agrupadores.md",
    "Selección de datos" = "seleccion_datos.md",
    "Descriptiva" = "descriptiva.md",
    "Outliers" = "outliers.md",
    "Otros gráficos" = "otros_graficos.md",
    "Composición" = "composicion.md",
    "Seguimiento contratos" = "seguimiento_contratos.md"
  )

  tagList(
    fluidRow(
      box(
        width = 12,
        selectizeInput(
          inputId = ns("manual"),
          label = "Manual",
          width = "100%",
          choices = archivos
        ),
        uiOutput(ns("markdown"))
      )
    )
  )

}

manuales_server <- function(id) {

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$markdown <- renderUI({
        tags$article(
          class="markdown-body",
          includeMarkdown(file.path("man", input$manual))
        )
      })

    }
  )

}
