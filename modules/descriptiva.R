descriptiva_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(width = 3,
      pickerInput(
        inputId = ns("descriptiva_cols"),
        label = "Agrupar por:",
        choices = c("NA"),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `deselect-all-text` = "Deseleccionar todos",
          `select-all-text` = "Seleccionar todos",
          `live-search` = TRUE)),
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
      ),
      actionButton(
        inputId = ns("descriptiva_exe"),
        label = "Confirmar"),
      tags$br(),
      tags$br(),
      downloadButton(
        outputId = ns("descriptiva_descargar_csv"),
        label = "CSV",
        style = "width:100%;"),
      tags$br(),
      tags$br(),
      downloadButton(
        outputId = ns("descriptiva_descargar_xlsx"), 
        label = "Excel",
        style = "width:100%;"),
      br(),
      br(),
      textOutput(outputId = ns("descriptiva_sumas_registros")),
      textOutput(outputId = ns("descriptiva_sumas_pacientes")),
      textOutput(outputId = ns("descriptiva_sumas_valor"))),
  box(
    width = 9,
    div(
      DT::dataTableOutput(outputId = ns("descriptiva_tabla")),
      style = "font-size:90%"))
  )
}

descriptiva_server <- function(input, output, session, datos, opciones) {
  
  descriptiva <- reactiveValues(tabla = data.table())
  
  observeEvent(datos$colnames, {
    updatePickerInput(
      session = session,
      inputId = "descriptiva_cols",
      choices = datos$colnames
    )
  })
  
  observeEvent(input$descriptiva_exe, {
    if(!is.null(datos$colnames)) {
      if(!is.null(input$descriptiva_cols) && input$descriptiva_cols != "NA") {
        opciones$descriptiva_cols <- input$descriptiva_cols
        withProgress(message = "Calculando descriptiva", {
          tryCatch(
            expr = {
              descriptiva$tabla <- descriptiva(
                data = datos$data_table, 
                columnas = opciones$descriptiva_cols, 
                columna_valor = opciones$valor_costo, 
                columna_suma = input$descriptiva_unidades,
                prestaciones = input$descriptiva_unidades == "prestacion"
              )
              
              output$descriptiva_tabla <- DT::renderDataTable({
                DT::datatable(
                  descriptiva$tabla,
                  options = list(
                    language = list(
                      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                    pageLength = 50,
                    autoWidth = FALSE,
                    ordering = T, 
                    scrollX = TRUE,
                    scrollY = "60vh"),
                  rownames= FALSE) %>%
                  formatCurrency(
                    c('P50','P75','P90','Media','Media truncada 10%',
                      'Media truncada 5%','Desv.tipica'),
                    mark = ".", 
                    dec.mark = ",") %>%
                  formatCurrency(c('Suma','Min.','Max.','Rango'),
                                 digits = 0, 
                                 mark = ".",
                                 dec.mark = ",")
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
              print(e)
            }
          )
        })
      }
    }
  })
  
  output$descriptiva_sumas_registros <- renderText({
    if (!is.null(datos$colnames)) {
      paste("Total registros:", 
            formatC(
              length(datos$data_table[["NRO_IDENTIFICACION"]]),
              big.mark = ".", 
              decimal.mark = ",", 
              format = "f", 
              digits = 0
            ),
            sep = " "
      )
    }
  })
  
  output$descriptiva_sumas_pacientes <- renderText({
    if (!is.null(datos$colnames)) {
      paste("Total pacientes:", 
            formatC(
              uniqueN(datos$data_table[["NRO_IDENTIFICACION"]]),
              big.mark = ".", 
              decimal.mark = ",", 
              format = "f", 
              digits = 0
            ),
            sep = " "
      )
    }
  })
  
  output$descriptiva_sumas_valor <- renderText({
    if (!is.null(datos$colnames)) {
      if(nrow(descriptiva$tabla) > 1) {
        paste("Total",
              paste0(tolower(opciones$valor_costo), ":"), 
              formatC(
                sum(
                  descriptiva$tabla[["Suma"]],
                  na.rm = TRUE), 
                big.mark = ".", 
                decimal.mark = ",", 
                format = "f", 
                digits = 0),
              sep = " "
        )
      }
    }
  })
  
  output$descriptiva_descargar_csv <- downloadHandler(
    filename = function() {
      paste("Descriptiva",
            ".csv", sep="")
    },
    content = function(file) {
      write.csv(
        x = descriptiva$tabla,
        file = file, 
        row.names = FALSE,
        na="")
    }, 
    contentType = "text/csv"
  )
  
  output$descriptiva_descargar_xlsx <- downloadHandler(
    filename = function() {
      paste("Descriptiva",
            ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(
        x = descriptiva$tabla,
        path = file)
    }, 
    contentType = "xlsx"
  )
}