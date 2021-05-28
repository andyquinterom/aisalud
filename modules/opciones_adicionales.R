opciones_adicionales_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId = ns("perfil"),
      width = "100%",
      "Perfil:",
      choices = "Ninguno"),
    tags$hr(),
    checkboxInput(
      inputId = ns("cantidad"),
      width = "100%",
      label = "Prestaciones por cantidad",
      value = FALSE
    )
  )
}

opciones_adicionales_server <- function(id, opciones) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      observe({
        updateSelectizeInput(
          session = session,
          inputId = "perfil",
          # Si no es eliminado el perfil seleccionado se mantiene
          selected = opciones$perfil_selected,
          choices = c("Ninguno", names(opciones$perfil_lista))
        )
      })

      observe({
        # Se observan cambios a opciones$perfil_selected en caso de que
        # el modulo de seguimiento pida un cambio de perfil
        updateSelectizeInput(
          session = session,
          inputId = "perfil",
          selected = opciones$perfil_selected
        )
      })

      # Validación de que haya un perfil seleccionado
      observe({
        opciones$perfil_enable <- FALSE
        if (input$perfil %notin% c("Ninguno", "")) {
          opciones$perfil_enable <- TRUE
          opciones$perfil_selected <- input$perfil
        }
      })

      # Opciones para el conteo por cantidades

      observe({
        cantidad_enable <- NULL
        # Si se tiene un perfil seleccionado se utilizará la opción del
        # perfil
        if (opciones$perfil_enable) {
          cantidad_enable <- opciones$perfil_lista[[
            opciones$perfil_selected]][["cantidad"]]
        }
        if (!is.null(cantidad_enable)) {
          # Si pasa el test este se cambiará
          updateCheckboxInput(
            inputId = "cantidad",
            value = cantidad_enable
          )
        }
      })

      observe({
        # Se observan cambios al checkboxInput
        opciones$cantidad <- input$cantidad
      })


    }
  )
}
