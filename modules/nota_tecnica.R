nota_tecnica_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      uiOutput(ns("nota_tecnica_jerarquia"))
    ),
    fluidRow(
      box(
        width = 3,
        pickerInput(
          inputId = ns("nota_tecnica_col_valor"),
          label = "Sumar valor por:",
          choices = c("NA"),
          multiple = FALSE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE)),
        pickerInput(
          inputId = ns("nota_tecnica_cols"),
          label = "Agrupar por:",
          choices = c("NA"),
          multiple = FALSE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE)),
        pickerInput(
          inputId = ns("nota_tecnica_cols_sep"),
          label = "Separar por:",
          choices = c("NA"),
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `select-all-text` = "Seleccionar todos",
            `deselect-all-text` = "Deseleccionar todos"))
      ),
      box(
        width = 9
      )
    ),
    fluidRow(
      uiOutput(
        outputId = ns("nota_tecnica_escenarios")
      )
    )
  )
}

nota_tecnica_server <- function(input, output, session, datos, opciones, 
                                nombre_id) {
  
  ns <- NS(nombre_id)
  
  nota_tecnica <- reactiveValues(tabla = data.table())
  
  observeEvent(datos$colnames, {
    updatePickerInput(
      session = session,
      inputId = "nota_tecnica_col_valor",
      choices = datos$colnames
    )
    updatePickerInput(
      session = session,
      inputId = "nota_tecnica_cols",
      choices = datos$colnames
    )
    updatePickerInput(
      session = session,
      inputId = "nota_tecnica_cols_sep",
      choices = datos$colnames
    )
  })
  
  observeEvent(input$nota_tecnica_cols, {
    if (!is.null(datos$colnames) && 
        length(datos$valores_unicos[[input$nota_tecnica_cols]]) <= 125) {
      tryCatch(
        expr = {
          agrupadores_items <- datos$valores_unicos[[input$nota_tecnica_cols]]
          output$nota_tecnica_jerarquia <- renderUI({
            tagList(
              box(
                width = 3,
                orderInput(
                  inputId = ns("nota_tecnica_jerarquia_nivel_1"),
                  label = "Episodio",
                  items = NULL,
                  width = "100%", 
                  connect = c(
                    ns("nota_tecnica_jerarquia_nivel_2"),
                    ns("nota_tecnica_jerarquia_nivel_3"),
                    ns("nota_tecnica_jerarquia_nivel_4")))
                ),
              box(
                width = 3,
                orderInput(
                  inputId = ns("nota_tecnica_jerarquia_nivel_2"),
                  label = "Factura",
                  items = NULL,
                  width = "100%",
                  connect = c(
                    ns("nota_tecnica_jerarquia_nivel_1"),
                    ns("nota_tecnica_jerarquia_nivel_3"),
                    ns("nota_tecnica_jerarquia_nivel_4")))
                ),
              box(
                width = 3,
                orderInput(
                  inputId = ns("nota_tecnica_jerarquia_nivel_3"),
                  label = "Paciente",
                  items = NULL,
                  width = "100%",
                  connect = c(
                    ns("nota_tecnica_jerarquia_nivel_1"),
                    ns("nota_tecnica_jerarquia_nivel_2"),
                    ns("nota_tecnica_jerarquia_nivel_4")))
                ),
              box(
                width = 3,
                orderInput(
                  inputId = ns("nota_tecnica_jerarquia_nivel_4"),
                  label = "Prestación",
                  items = agrupadores_items,
                  width = "100%",
                  connect = c(
                    ns("nota_tecnica_jerarquia_nivel_1"),
                    ns("nota_tecnica_jerarquia_nivel_2"),
                    ns("nota_tecnica_jerarquia_nivel_3")))
                )
            )
          })
        },
        error = function(e) {
          sendSweetAlert(
            session = session,
            title = "Error", 
            type = "error",
            text = "Por favor revisar los parametros de carga de datos,
                columnas, formato de fecha y los datos. Si este problema persiste
                ponerse en contacto con un administrador."
          )
        }
      )
    } else {
      output$nota_tecnica_jerarquia <- renderUI({
        box(
          width = 12,
          radioButtons(
            inputId = ns("descriptiva_unidades"),
            label = "Unidad de descriptiva",
            choiceNames = c(
              "Prestación",
              "Paciente",
              "Factura"
            ),
            choiceValues = c(
              "prestacion",
              "NRO_IDENTIFICACION",
              "NRO_FACTURA"
            )
          ))
      })
    }
  })
  
}