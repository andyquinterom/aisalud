episodios_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 3,
        selectizeInput(
          inputId = ns("episodios_col_valor"),
          label = "Sumar valor por:",
          choices = NULL,
          multiple = FALSE),
        selectizeInput(
          inputId = ns("episodios_cols"),
          label = "Agrupar por:",
          choices = NULL, 
          multiple = FALSE),
        selectizeInput(
          inputId = ns("episodios_cols_sep"),
          label = "Separar por:",
          choices = NULL,
          multiple = TRUE),
        textOutput(outputId = ns("episodios_sumas_valor")),
        tags$br(),
        uiOutput(
          outputId = ns("episodios_jerarquia")
        ),
        tags$br(),
        actionButton(ns("episodios_exe"), "Confirmar"),
        tags$br(),
        tags$br(),
        downloadButton(
          outputId = ns("episodios_descargar_csv"),
          label = "CSV",
          style = "width:100%;"),
        tags$br(),
        tags$br(),
        downloadButton(
          outputId = ns("episodios_descargar_xlsx"),
          label = "Excel",
          style = "width:100%;")),
    box(
      width = 9,
      div(
        DT::dataTableOutput(outputId = ns("episodios_tabla")),
        style = "font-size:90%")))
  )
}

episodios_server <- function(input, output, session, datos, opciones, 
                             nombre_id) {
  
  ns <- NS(nombre_id)
  
  episodios <- reactiveValues(tabla = data.table())
  
  observeEvent(datos$colnames, {
    updateSelectizeInput(
      session = session,
      inputId = "episodios_col_valor",
      choices = datos$colnames
    )
    updateSelectizeInput(
      session = session,
      inputId = "episodios_cols",
      choices = datos$colnames
    )
    updateSelectizeInput(
      session = session,
      inputId = "episodios_cols_sep",
      choices = datos$colnames
    )
  })
  
  observeEvent(input$episodios_cols, {
    if (!is.null(datos$colnames) && 
        !is.null(input$episodios_cols) &&
        length(datos$valores_unicos[[input$episodios_cols]]) <= 125) {
      tryCatch(
        expr = {
          agrupadores_items <- datos$valores_unicos[[input$episodios_cols]]
          output$episodios_jerarquia <- renderUI({
            tagList(
              orderInput(
                inputId = ns("episodios_jerarquia_nivel_1"),
                label = "Episodio",
                items = NULL,
                width = "100%", 
                connect = c(
                  ns("episodios_jerarquia_nivel_2"),
                  ns("episodios_jerarquia_nivel_3"),
                  ns("episodios_jerarquia_nivel_4"))
              ),
              orderInput(
                inputId = ns("episodios_jerarquia_nivel_2"),
                label = "Factura",
                items = NULL,
                width = "100%",
                connect = c(
                  ns("episodios_jerarquia_nivel_1"),
                  ns("episodios_jerarquia_nivel_3"),
                  ns("episodios_jerarquia_nivel_4"))
              ),
              orderInput(
                inputId = ns("episodios_jerarquia_nivel_3"),
                label = "Paciente",
                items = NULL,
                width = "100%",
                connect = c(
                  ns("episodios_jerarquia_nivel_1"),
                  ns("episodios_jerarquia_nivel_2"),
                  ns("episodios_jerarquia_nivel_4"))
              ),
              orderInput(
                inputId = ns("episodios_jerarquia_nivel_4"),
                label = "Prestación",
                items = agrupadores_items,
                width = "100%",
                connect = c(
                  ns("episodios_jerarquia_nivel_1"),
                  ns("episodios_jerarquia_nivel_2"),
                  ns("episodios_jerarquia_nivel_3"))
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
    }
  })
  
  observeEvent(input$episodios_exe, {
    if(!is.null(datos$colnames)) {
      if(!is.null(input$episodios_col_valor) && 
         !is.null(input$episodios_cols) &&
         input$episodios_cols != "NA") {
        tryCatch(
          expr = {
            opciones$episodios_cols <- input$episodios_cols
            opciones$episodios_col_valor <- input$episodios_col_valor
            opciones$episodios_cols_sep <- input$episodios_cols_sep
            withProgress(message = "Calculando descriptiva por episodio",{
              episodios$tabla <- episodios_jerarquia(
                data = datos$data_table,
                columnas =      opciones$episodios_cols, 
                columna_valor = opciones$valor_costo, 
                columna_sep =   opciones$episodios_cols_sep,
                columna_suma =  opciones$episodios_col_valor,
                nivel_1 = input$episodios_jerarquia_nivel_1_order,
                nivel_2 = input$episodios_jerarquia_nivel_2_order,
                nivel_3 = input$episodios_jerarquia_nivel_3_order,
                nivel_4 = input$episodios_jerarquia_nivel_4_order)
              
              output$episodios_tabla <- DT::renderDataTable({
                DT::datatable(
                  episodios$tabla,
                  options = list(
                    language = list(
                      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                    pageLength = 50,
                    autoWidth = FALSE,
                    ordering=T, 
                    scrollX = TRUE,
                    scrollY = "60vh"),
                  rownames= FALSE) %>%
                  formatCurrency(
                    c('P50','P75','P90','Media','Media truncada 10%',
                      'Media truncada 5%','Desv.tipica'),
                    mark = ".",
                    dec.mark = ",") %>%
                  formatCurrency(c('Suma','Min.','Max.','Rango'),
                                 digits=0,
                                 mark = ".",
                                 dec.mark = ",")
              })   
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
      }
    }
  })
  
  output$episodios_sumas_valor <- renderText({
    if (!is.null(datos$colnames)) {
      if(nrow(episodios$tabla) > 1) {
        paste("Total",
              paste0(tolower(opciones$valor_costo), ":"), 
              formatC(
                sum(
                  episodios$tabla[["Suma"]],
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
  
  output$episodios_descargar_csv <- downloadHandler(
    filename = function() {
      paste("Episodios",
            ".csv", sep="")
    },
    content = function(file) {
      write.csv(
        x = episodios$tabla,
        file = file, 
        row.names = FALSE,
        na="")
    }, 
    contentType = "text/csv"
  )
  
  output$episodios_descargar_xlsx <- downloadHandler(
    filename = function() {
      paste("Episodios",
            ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(
        x = episodios$tabla,
        path = file)
    }, 
    contentType = "xlsx"
  )
}