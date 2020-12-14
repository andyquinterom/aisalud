frecuencias_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 3,
        checkboxInput(
          inputId = ns("episodios_enable"),
          label = "Agrupar por episodios",
          value = F
        ),
        uiOutput(
          outputId = ns("episodios_col_valor_out")
        ),  
        selectizeInput(
          inputId = ns("agrupador_cols"),
          label = "Agrupar por:",
          choices = NULL, 
          multiple = FALSE),
        selectizeInput(
          inputId = ns("agrupador_cols_sep"),
          label = "Separar por:",
          choices = NULL,
          multiple = TRUE),
        tags$br(),
        uiOutput(
          outputId = ns("episodios_jerarquia")
        ),
        tags$br(),
        actionButton(ns("frecuencias_exe"), "Confirmar"),
        tags$br(),
        tags$br(),
        downloadButton(
          outputId = ns("frecuencias_descargar_csv"),
          label = "CSV",
          style = "width:100%;"),
        tags$br(),
        tags$br(),
        downloadButton(
          outputId = ns("frecuencias_descargar_xlsx"),
          label = "Excel",
          style = "width:100%;")),
      box(
        width = 9,
        tabsetPanel(
          tabPanel(
            title = "Tabla",
            tags$br(),
            tags$h2(textOutput(ns("tabla_titulo")), class = "titulo_center"),
            tags$br(),
            div(
              DT::dataTableOutput(outputId = ns("frecuencias_tabla")) %>%
                withSpinner(),
              style = "font-size:90%")
          )
        )
      )
    )
  )
}

frecuencias_server <- function(input, output, session, datos, opciones, 
                             nombre_id) {
  
  ns <- NS(nombre_id)
  
  frecuencias <- reactiveValues(
    tabla = list("descriptiva" = data.table(), "data" = data.table()),
    agrupadores_items = NULL)
  
  observeEvent(datos$colnames, {
    if (input$episodios_enable) {
      updateSelectizeInput(
        session = session,
        inputId = "episodios_col_valor",
        choices = datos$colnames,
        selected = "nro_identificacion"
      )
    }
    updateSelectizeInput(
      session = session,
      inputId = "agrupador_cols",
      choices = datos$colnames
    )
    updateSelectizeInput(
      session = session,
      inputId = "agrupador_cols_sep",
      choices = datos$colnames
    )
  })
  
  observeEvent(input$episodios_enable, {
    if (input$episodios_enable) {
      output$episodios_col_valor_out <- renderUI({
        selectizeInput(
          inputId = ns("episodios_col_valor"),
          label = "Sumar valor por:",
          choices = datos$colnames,
          selected = "nro_identificacion",
          multiple = FALSE)
      })
    } else {
      output$episodios_col_valor_out <- renderUI({})
    }
  })
  
  cambio_columnas <- reactive({
    list(input$agrupador_cols, input$episodios_enable)
  })
  
  observeEvent(cambio_columnas(), {
    if (!is.null(datos$colnames) && 
        !is.null(input$agrupador_cols)) {
      tryCatch(
        expr = {
          if (length(datos$valores_unicos[[input$agrupador_cols]]) <= 60 &&
              input$episodios_enable) {
            frecuencias$agrupadores_items <-
              datos$valores_unicos[[input$agrupador_cols]]
            output$episodios_jerarquia <- renderUI({
              descriptiva_jerarquia(
                ns = ns,
                items_nivel_4 = frecuencias$agrupadores_items
              )
            })
          } else {
            frecuencias$agrupadores_items <- NULL
            output$episodios_jerarquia <- renderUI({
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
                  "nro_identificacion",
                  "nro_factura"
                )
              )
            })
          }
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
  
  observeEvent(input$seleccionar_episodio, {
    output$episodios_jerarquia <- renderUI({
      tagList(
        descriptiva_jerarquia(
          ns = ns,
          items_nivel_1 = frecuencias$agrupadores_items)
      )
    })
  })
  
  observeEvent(input$seleccionar_factura, {
    output$episodios_jerarquia <- renderUI({
      tagList(
        descriptiva_jerarquia(
          ns = ns,
          items_nivel_2 = frecuencias$agrupadores_items)
      )
    })
  })
  
  observeEvent(input$seleccionar_paciente, {
    output$episodios_jerarquia <- renderUI({
      tagList(
        descriptiva_jerarquia(
          ns = ns,
          items_nivel_3 = frecuencias$agrupadores_items)
      )
    })
  })
  
  observeEvent(input$seleccionar_prestacion, {
    output$episodios_jerarquia <- renderUI({
      tagList(
        descriptiva_jerarquia(
          ns = ns,
          items_nivel_4 = frecuencias$agrupadores_items)
      )
    })
  })
  
  
  observeEvent(input$frecuencias_exe, {
    
    if(!is.null(datos$colnames)) {
      if(!is.null(input$agrupador_cols) &&
         input$agrupador_cols != "NA") {
        tryCatch(
          expr = {
            agrupador_cols <- input$agrupador_cols
            if (input$episodios_enable) {
              episodios_col_valor <- input$episodios_col_valor
            }
            agrupador_cols_sep <- input$agrupador_cols_sep
            withProgress(message = "Calculando descriptiva...",{
              if (!is.null(frecuencias$agrupadores_items)) {
                frecuencias$descriptiva_basica <- descriptiva_basica_jerarquia(
                  data = datos$data_table,
                  columnas =      agrupador_cols, 
                  columna_valor = opciones$valor_costo, 
                  columna_suma =  episodios_col_valor,
                  nivel_1 = input$episodios_jerarquia_nivel_1_order,
                  nivel_2 = input$episodios_jerarquia_nivel_2_order,
                  nivel_3 = input$episodios_jerarquia_nivel_3_order,
                  nivel_4 = input$episodios_jerarquia_nivel_4_order)
              } else {
                frecuencias$descriptiva_basica <- descriptiva_basica(
                  data = datos$data_table,
                  agrupador = agrupador_cols,
                  columna_valor = opciones$valor_costo,
                  columna_suma = input$descriptiva_unidades,
                  prestaciones = (input$descriptiva_unidades == "prestacion"),
                  columna_fecha = "fecha_prestacion"
                )
              }
              
              frecuencias$tabla <- descriptiva_basica_trans(
                data = frecuencias$descriptiva_basica,
                agrupador = agrupador_cols,
                suma = FALSE
              )
              
              output$frecuencias_tabla <- DT::renderDataTable({
                DT::datatable(
                  frecuencias$tabla,
                  options = list(
                    language = list(
                      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                    pageLength = 50,
                    autoWidth = FALSE,
                    ordering=T, 
                    scrollX = TRUE,
                    scrollY = "60vh"),
                  rownames= FALSE)
                  # formatCurrency(
                  #   c('P50','P75','P90','Media','Media truncada 10%',
                  #     'Media truncada 5%','Desv.tipica'),
                  #   mark = ".",
                  #   dec.mark = ",") %>%
                  # formatCurrency(c('Suma','Min.','Max.','Rango'),
                  #                digits=0,
                  #                mark = ".",
                  #                dec.mark = ",")
              })
              
              output$tabla_titulo <- renderText({
                paste(
                  "Descriptiva de",
                  agrupador_cols,
                  ifelse(
                    test = is.null(agrupador_cols_sep),
                    yes = "",
                    no = "separada por"
                  ),
                  separar_spanish(agrupador_cols_sep),
                  collapse = " "
                )
              })
              
              # output$histograma_select_agrupador <- DT::renderDataTable({
              #   DT::datatable(
              #     frecuencias$tabla[["descriptiva"]][, c(
              #       agrupador_cols, agrupador_cols_sep), with = FALSE],
              #     options = list(
              #       language = list(
              #         url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              #       pageLength = 10000,
              #       dom = 'ft',
              #       autoWidth = FALSE,
              #       ordering=T, 
              #       scrollX = TRUE,
              #       scrollY = "370px"),
              #     rownames= FALSE) %>%
              #     formatStyle(
              #       columns = 1:length(c(agrupador_cols, agrupador_cols_sep)),
              #       fontSize = '95%')
              # })
              # 
              # output$caja_de_bigotes_select_agrupador <- 
              #   DT::renderDataTable({
              #     DT::datatable(
              #       frecuencias$tabla[["descriptiva"]][, c(
              #         agrupador_cols, agrupador_cols_sep), with = FALSE],
              #       options = list(
              #         language = list(
              #           url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              #         pageLength = 10000,
              #         dom = 'ft',
              #         autoWidth = FALSE,
              #         ordering=T, 
              #         scrollX = TRUE,
              #         scrollY = "370px"),
              #       rownames= FALSE) %>%
              #       formatStyle(
              #         columns = 1:length(c(agrupador_cols, agrupador_cols_sep)),
              #         fontSize = '95%')
              #   })
              # 
              # output$grafico_barras_select_agrupador <- 
              #   DT::renderDataTable({
              #     DT::datatable(
              #       frecuencias$tabla[["descriptiva"]][, c(
              #         agrupador_cols, agrupador_cols_sep), with = FALSE],
              #       options = list(
              #         language = list(
              #           url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              #         pageLength = 10000,
              #         dom = 'ft',
              #         autoWidth = FALSE,
              #         ordering=T, 
              #         scrollX = TRUE,
              #         scrollY = "370px"),
              #       rownames= FALSE) %>%
              #       formatStyle(
              #         columns = 1:length(c(agrupador_cols, agrupador_cols_sep)),
              #         fontSize = '95%')
              #   })
              
            })
          },
          error = function(e) {
            print(e)
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
  
  output$episodios_descargar_csv <- downloadHandler(
    filename = function() {
      paste("Descriptiva",
            ".csv", sep="")
    },
    content = function(file) {
      write.csv(
        x = frecuencias$tabla[["descriptiva"]],
        file = file, 
        row.names = FALSE,
        na="")
    }, 
    contentType = "text/csv"
  )
  
  output$episodios_descargar_xlsx <- downloadHandler(
    filename = function() {
      paste("Descriptiva",
            ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(
        x = frecuencias$tabla[["descriptiva"]],
        path = file)
    }, 
    contentType = "xlsx"
  )
}

descriptiva_jerarquia <- function(
  ns,
  items_nivel_1 = NULL,
  items_nivel_2 = NULL,
  items_nivel_3 = NULL, 
  items_nivel_4 = NULL) {
  return(
    tagList(
      orderInput(
        inputId = ns("episodios_jerarquia_nivel_1"),
        label = actionLink(ns("seleccionar_episodio"), label = "Episodio"),
        items = items_nivel_1,
        width = "100%", 
        height = "100%",
        connect = c(
          ns("episodios_jerarquia_nivel_2"),
          ns("episodios_jerarquia_nivel_3"),
          ns("episodios_jerarquia_nivel_4"))),
      orderInput(
        inputId = ns("episodios_jerarquia_nivel_2"),
        label = actionLink(ns("seleccionar_factura"), label = "Factura"),
        items = items_nivel_2,
        width = "100%",
        height = "100%",
        connect = c(
          ns("episodios_jerarquia_nivel_1"),
          ns("episodios_jerarquia_nivel_3"),
          ns("episodios_jerarquia_nivel_4"))),
      orderInput(
        inputId = ns("episodios_jerarquia_nivel_3"),
        label = actionLink(ns("seleccionar_paciente"), label = "Paciente"),
        items = items_nivel_3,
        width = "100%",
        height = "100%",
        connect = c(
          ns("episodios_jerarquia_nivel_1"),
          ns("episodios_jerarquia_nivel_2"),
          ns("episodios_jerarquia_nivel_4"))),
      orderInput(
        inputId = ns("episodios_jerarquia_nivel_4"),
        label = actionLink(ns("seleccionar_prestacion"), label = "Prestación"),
        items = items_nivel_4,
        width = "100%",
        height = "100%",
        connect = c(
          ns("episodios_jerarquia_nivel_1"),
          ns("episodios_jerarquia_nivel_2"),
          ns("episodios_jerarquia_nivel_3")))
    )
  )
}