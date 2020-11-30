otros_graficos <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        tabsetPanel(
          tabPanel(
            title = "Inicio",
            tags$br(),
            tags$h2(textOutput(ns("tabla_titulo")), class = "titulo_center"),
            tags$br(),
            div(
              DT::dataTableOutput(outputId = ns("episodios_tabla")),
              style = "font-size:90%")
          ),
          tabPanel(
            title = "Distribución de edades",
            tags$br()
          )
        )
      )
    )
  )
}