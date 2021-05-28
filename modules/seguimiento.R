seguimiento_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        width = 4,
        selectizeInput(
          inputId =  ns("nota_tecnica"),
          label = "Comparar con nota técnica:",
          choices = NULL,
          multiple = FALSE),
        agrupadores_widget(
          id = id,
          separadores = FALSE,
          checkboxGroupInput(
            inputId = ns("tablas"),
            label = "Seguimiento:",
            choices = c(
              "Frecuencias" = "frecuencias",
              "Valor facturado" = "valor"),
            selected = "descriptiva",
            inline = TRUE,
            width = "100%")),
        tags$br(),
        actionButton(ns("exe"), "Generar"),
        tags$br()
      ),
      box(
        width = 8
      )
    )
  )
}

seguimiento_server <- function(id, opciones, conn) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- NS(id)
      episodios <- reactiveValues()

      # Se observa que el usuario haga click en los titulos de las unidades
      # de conteo en el widget de jerarquia.
      # De esta manera se pueden mover los diferentes agrupadores de manera
      # sencilla entre unidades.

      episodios_jerarquia_server(
        id = id,
        episodios = episodios,
        opciones = opciones)

    }
  )
}
