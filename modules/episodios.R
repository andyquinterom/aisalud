episodios_ui <- function(id) {
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
          inputId = ns("episodios_cols"),
          label = "Agrupar por:",
          choices = NULL, 
          multiple = FALSE),
        selectizeInput(
          inputId = ns("episodios_cols_sep"),
          label = "Separar por:",
          choices = NULL,
          multiple = TRUE),
        textOutput(outputId = ns("descriptiva_sumas_registros")),
        textOutput(outputId = ns("descriptiva_sumas_pacientes")),
        textOutput(outputId = ns("descriptiva_sumas_facturas")),
        textOutput(outputId = ns("descriptiva_sumas_valor")),
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
      tabsetPanel(
        tabPanel(
          title = "Tabla",
          tags$br(),
          tags$h2(textOutput(ns("tabla_titulo")), class = "titulo_center"),
          tags$br(),
          div(
            DT::dataTableOutput(outputId = ns("episodios_tabla")),
            style = "font-size:90%")
        ),
        tabPanel(
          title = "Histogramas",
          tags$br(),
          fluidRow(
            column(
              width = 8,
              tags$h3(
                textOutput(ns("histograma_titulo")), class = "titulo_center"),
              plotlyOutput(
                outputId = ns("histograma_render")
              )
            ),
            column(
              width = 4,
              DT::dataTableOutput(
                outputId = ns("histograma_select_agrupador")
              )
            )
          )
        ),
        tabPanel(
          title = "Caja de bigotes",
          tags$br(),
          fluidRow(
            column(
              width = 8,
              tags$h3(
                textOutput(ns("caja_de_bigotes_titulo")), class = "titulo_center"),
              plotlyOutput(
                outputId = ns("caja_de_bigotes_render")
              )
            ),
            column(
              width = 4,
              DT::dataTableOutput(
                outputId = ns("caja_de_bigotes_select_agrupador")
              )
            )
          )
        )
      )
      )
    )
  )
}

episodios_server <- function(input, output, session, datos, opciones, 
                             nombre_id) {
  
  ns <- NS(nombre_id)
  
  episodios <- reactiveValues(
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
      inputId = "episodios_cols",
      choices = datos$colnames
    )
    updateSelectizeInput(
      session = session,
      inputId = "episodios_cols_sep",
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
    list(input$episodios_cols, input$episodios_enable)
  })
  
  observeEvent(cambio_columnas(), {
    if (!is.null(datos$colnames) && 
        !is.null(input$episodios_cols)) {
      tryCatch(
        expr = {
          if (length(datos$valores_unicos[[input$episodios_cols]]) <= 60 &&
              input$episodios_enable) {
            episodios$agrupadores_items <-
              datos$valores_unicos[[input$episodios_cols]]
            output$episodios_jerarquia <- renderUI({
              descriptiva_jerarquia(
                ns = ns,
                items_nivel_4 = episodios$agrupadores_items
              )
            })
          } else {
            episodios$agrupadores_items <- NULL
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
          items_nivel_1 = episodios$agrupadores_items)
      )
    })
  })
  
  observeEvent(input$seleccionar_factura, {
    output$episodios_jerarquia <- renderUI({
      tagList(
        descriptiva_jerarquia(
          ns = ns,
          items_nivel_2 = episodios$agrupadores_items)
      )
    })
  })
  
  observeEvent(input$seleccionar_paciente, {
    output$episodios_jerarquia <- renderUI({
      tagList(
        descriptiva_jerarquia(
          ns = ns,
          items_nivel_3 = episodios$agrupadores_items)
      )
    })
  })
  
  observeEvent(input$seleccionar_prestacion, {
    output$episodios_jerarquia <- renderUI({
      tagList(
        descriptiva_jerarquia(
          ns = ns,
          items_nivel_4 = episodios$agrupadores_items)
      )
    })
  })
  
  
  observeEvent(input$episodios_exe, {
    
    if(!is.null(datos$colnames)) {
      if(!is.null(input$episodios_cols) &&
         input$episodios_cols != "NA") {
        tryCatch(
          expr = {
            episodios_cols <- input$episodios_cols
            if (input$episodios_enable) {
              episodios_col_valor <- input$episodios_col_valor
            }
            episodios_cols_sep <- input$episodios_cols_sep
            withProgress(message = "Calculando descriptiva por episodio",{
              if (!is.null(episodios$agrupadores_items)) {
                episodios$tabla <- episodios_jerarquia(
                  data = datos$data_table,
                  columnas =      episodios_cols, 
                  columna_valor = opciones$valor_costo, 
                  columna_sep =   episodios_cols_sep,
                  columna_suma =  episodios_col_valor,
                  nivel_1 = input$episodios_jerarquia_nivel_1_order,
                  nivel_2 = input$episodios_jerarquia_nivel_2_order,
                  nivel_3 = input$episodios_jerarquia_nivel_3_order,
                  nivel_4 = input$episodios_jerarquia_nivel_4_order)
              } else {
                episodios$tabla <- descriptiva(
                  data = datos$data_table,
                  columnas = c(
                    episodios_cols,
                    episodios_cols_sep
                  ),
                  columna_valor = opciones$valor_costo,
                  columna_suma = input$descriptiva_unidades,
                  prestaciones = (input$descriptiva_unidades == "prestacion")
                )
              }
              
              episodios$histograma_titulo <- paste(
                "Histograma de valores por",
                separar_spanish(
                  if (!is.null(episodios$agrupadores_items)) {
                    c("episodio", "factura", "paciente", "prestación")[
                      !c(is.null(input$episodios_jerarquia_nivel_1_order),
                         is.null(input$episodios_jerarquia_nivel_2_order),
                         is.null(input$episodios_jerarquia_nivel_3_order),
                         is.null(input$episodios_jerarquia_nivel_4_order))
                    ]
                  } else {
                    c("prestación", "paciente", "factura")[
                      c(input$descriptiva_unidades == "prestacion",
                        input$descriptiva_unidades == "nro_identificacion",
                        input$descriptiva_unidades == "nro_factura")
                    ]
                  }
                )
              )
              
              episodios$caja_de_bigotes_titulo <- paste(
                "Distribución del valor por",
                separar_spanish(
                  if (!is.null(episodios$agrupadores_items)) {
                    c("episodio", "factura", "paciente", "prestación")[
                      !c(is.null(input$episodios_jerarquia_nivel_1_order),
                         is.null(input$episodios_jerarquia_nivel_2_order),
                         is.null(input$episodios_jerarquia_nivel_3_order),
                         is.null(input$episodios_jerarquia_nivel_4_order))
                    ]
                  } else {
                    c("prestación", "paciente", "factura")[
                      c(input$descriptiva_unidades == "prestacion",
                        input$descriptiva_unidades == "nro_identificacion",
                        input$descriptiva_unidades == "nro_factura")
                    ]
                  }
                )
              )
              
              episodios$lista_agrupadores <- 
                episodios$tabla[["descriptiva"]][, c(
                  episodios_cols, episodios_cols_sep), with = FALSE]
              
              output$episodios_tabla <- DT::renderDataTable({
                DT::datatable(
                  episodios$tabla[["descriptiva"]],
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
              
              output$tabla_titulo <- renderText({
                paste(
                  "Descriptiva de",
                  episodios_cols,
                  ifelse(
                    test = is.null(episodios_cols_sep),
                    yes = "",
                    no = "separada por"
                  ),
                  separar_spanish(episodios_cols_sep),
                  collapse = " "
                )
              })
              
              output$histograma_select_agrupador <- DT::renderDataTable({
                DT::datatable(
                  episodios$tabla[["descriptiva"]][, c(
                    episodios_cols, episodios_cols_sep), with = FALSE],
                  options = list(
                    language = list(
                      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                    pageLength = 10000,
                    dom = 'ft',
                    autoWidth = FALSE,
                    ordering=T, 
                    scrollX = TRUE,
                    scrollY = "370px"),
                  rownames= FALSE) %>%
                  formatStyle(
                    columns = 1:length(c(episodios_cols, episodios_cols_sep)),
                    fontSize = '95%')
              })
              
              output$caja_de_bigotes_select_agrupador <- 
                DT::renderDataTable({
                  DT::datatable(
                    episodios$tabla[["descriptiva"]][, c(
                      episodios_cols, episodios_cols_sep), with = FALSE],
                    options = list(
                      language = list(
                        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 10000,
                      dom = 'ft',
                      autoWidth = FALSE,
                      ordering=T, 
                      scrollX = TRUE,
                      scrollY = "370px"),
                    rownames= FALSE) %>%
                    formatStyle(
                      columns = 1:length(c(episodios_cols, episodios_cols_sep)),
                      fontSize = '95%')
              })
              
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
  
  output$histograma_titulo <- renderText({
    if (!is.null(input$histograma_select_agrupador_rows_selected)) {
      episodios$histograma_titulo
    }
  })
  
  output$caja_de_bigotes_titulo <- renderText({
    if (!is.null(input$caja_de_bigotes_select_agrupador_rows_selected)) {
      episodios$caja_de_bigotes_titulo
    }
  })
  
  output$histograma_render <- renderPlotly({
    if (!is.null(input$histograma_select_agrupador_rows_selected)) {
      histograma_agrupador(
        titulo = "Histograma",
        data = merge.data.table(
          x = episodios$lista_agrupadores[
            input$histograma_select_agrupador_rows_selected],
          y = episodios$tabla[["data"]]
        )[["valor_calculos"]]
      )
    }
  })
  
  output$caja_de_bigotes_render <- renderPlotly({
    if (!is.null(input$caja_de_bigotes_select_agrupador_rows_selected)) {
      caja_de_bigotes_agrupador(
        data = episodios$tabla[["data"]],
        columna_numeros = "valor_calculos", 
        columnas_sep = episodios$lista_agrupadores[
          input$caja_de_bigotes_select_agrupador_rows_selected]
      )
    }
  })
  
  output$descriptiva_sumas_valor <- renderText({
    if (!is.null(datos$colnames)) {
      if(nrow(episodios$tabla[["descriptiva"]]) > 1) {
        paste("Total",
              paste0(tolower(opciones$valor_costo), ":"), 
              formatAsCurrency(
                sum(episodios$tabla[["descriptiva"]][["Suma"]],
                  na.rm = TRUE)),
              sep = " "
        )
      }
    }
  })
  
  output$descriptiva_sumas_registros <- renderText({
    if (!is.null(datos$colnames)) {
      paste("Número de registros:", 
            formatC(
              length(datos$data_table[["nro_identificacion"]]),
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
      paste("Número de pacientes:", 
            formatC(
              uniqueN(datos$data_table[["nro_identificacion"]]),
              big.mark = ".", 
              decimal.mark = ",", 
              format = "f", 
              digits = 0
            ),
            sep = " "
      )
    }
  })
  
  output$descriptiva_sumas_facturas <- renderText({
    if (!is.null(datos$colnames) && "nro_factura" %in% datos$colnames) {
      paste("Número de facturas:", 
            formatC(
              uniqueN(datos$data_table[["nro_factura"]]),
              big.mark = ".", 
              decimal.mark = ",", 
              format = "f", 
              digits = 0
            ),
            sep = " "
      )
    }
  })
  
  output$episodios_descargar_csv <- downloadHandler(
    filename = function() {
      paste("Descriptiva",
            ".csv", sep="")
    },
    content = function(file) {
      write.csv(
        x = episodios$tabla[["descriptiva"]],
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
        x = episodios$tabla[["descriptiva"]],
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