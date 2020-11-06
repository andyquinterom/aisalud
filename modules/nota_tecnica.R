nota_tecnica_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      uiOutput(ns("nota_tecnica_jerarquia"))
    ),
    tags$br(),
    fluidRow(
      box(
        width = 3,
        selectizeInput(
          inputId = ns("nota_tecnica_col_valor"),
          label = "Sumar valor por:",
          choices = c("NA"),
          multiple = FALSE),
        selectizeInput(
          inputId = ns("nota_tecnica_cols"),
          label = "Agrupar por:",
          choices = c("NA"),
          multiple = FALSE),
        selectizeInput(
          inputId = ns("nota_tecnica_cols_sep"),
          label = "Separar por:",
          choices = c("NA"),
          multiple = TRUE),
        numericInput(
          inputId = ns("nota_tecnica_meses"),
          label = "Número de meses:",
          min = 1,
          value = 12
        ),
        numericInput(
          inputId = ns("nota_tecnica_poblacion"),
          label = "Población:",
          min = 1,
          value = 40000
        ),
        actionButton(
          inputId = ns("nota_tecnica_exe"),
          label = "Generar escenarios",
          width = "100%"
        )
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
    updateSelectizeInput(
      session = session,
      inputId = "nota_tecnica_col_valor",
      choices = datos$colnames
    )
    updateSelectizeInput(
      session = session,
      inputId = "nota_tecnica_cols",
      choices = datos$colnames
    )
    updateSelectizeInput(
      session = session,
      inputId = "nota_tecnica_cols_sep",
      choices = datos$colnames
    )
    updateNumericInput(
      session = session,
      inputId = "nota_tecnica_meses",
      value = uniqueN(month(datos$valores_unicos[["FECHA_PRESTACION"]]))
    )
  })
  
  observeEvent(input$nota_tecnica_cols, {
    if (!is.null(datos$colnames) && 
        length(datos$valores_unicos[[input$nota_tecnica_cols]]) <= 125) {
      tryCatch(
        expr = {
          nota_tecnica$agrupadores_items <-
            datos$valores_unicos[[input$nota_tecnica_cols]]
          output$nota_tecnica_jerarquia <- renderUI({
            tagList(
              nota_tecnica_cajas_jerarquia(
                ns = ns,
                items_nivel_4 = nota_tecnica$agrupadores_items)
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
      nota_tecnica$agrupadores_items <- NULL
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
  
  observeEvent(input$seleccionar_episodio, {
    output$nota_tecnica_jerarquia <- renderUI({
      tagList(
        nota_tecnica_cajas_jerarquia(
          ns = ns,
          items_nivel_1 = nota_tecnica$agrupadores_items)
      )
    })
  })
  
  observeEvent(input$seleccionar_factura, {
    output$nota_tecnica_jerarquia <- renderUI({
      tagList(
        nota_tecnica_cajas_jerarquia(
          ns = ns,
          items_nivel_2 = nota_tecnica$agrupadores_items)
      )
    })
  })
  
  observeEvent(input$seleccionar_paciente, {
    output$nota_tecnica_jerarquia <- renderUI({
      tagList(
        nota_tecnica_cajas_jerarquia(
          ns = ns,
          items_nivel_3 = nota_tecnica$agrupadores_items)
      )
    })
  })
  
  observeEvent(input$seleccionar_prestacion, {
    output$nota_tecnica_jerarquia <- renderUI({
      tagList(
        nota_tecnica_cajas_jerarquia(
          ns = ns,
          items_nivel_4 = nota_tecnica$agrupadores_items)
      )
    })
  })
  
  observeEvent(input$nota_tecnica_exe, {
    
    tryCatch(
      expr = {
    
        test_episodio <- FALSE
        test_factura <- FALSE
        test_paciente <- FALSE
        test_prestacion <- FALSE
        
        if (!is.null(nota_tecnica$agrupadores_items)) {
          test_episodio <- !is.null(input$nota_tecnica_jerarquia_nivel_1_order)
          test_factura <- !is.null(input$nota_tecnica_jerarquia_nivel_2_order)
          test_paciente <- !is.null(input$nota_tecnica_jerarquia_nivel_3_order)
          test_prestacion <- !is.null(input$nota_tecnica_jerarquia_nivel_4_order)
        } else {
          test_factura <- "NRO_FACTURA" == input$descriptiva_unidades
          test_paciente <- "NRO_IDENTIFICACION" == input$descriptiva_unidades
          test_prestacion <- "prestacion" == input$descriptiva_unidades
        }
        
        nota_tecnica_cols <- input$nota_tecnica_cols
        nota_tecnica_col_valor <- input$nota_tecnica_col_valor
        nota_tecnica_cols_sep <- input$nota_tecnica_cols_sep
        
        output$nota_tecnica_escenarios <- renderUI({
          
          nombres_escenarios <- c(
            "Escenario media",
            "Escenario P75",
            "Escenario media trucada 10%",
            "Escenario media trucada 5%"
          )
          
          lapply(
            X = 1:4,
            FUN = function(i) {
              box(
                title = tags$h1(nombres_escenarios[i]),
                width = 6,
                if (test_episodio) {
                  tagList(
                    tags$h4("Episodio"),
                    DT::dataTableOutput(
                      outputId = ns(paste0("escenario_episodio_", i))
                    ),
                    tags$br()
                  )
                },
                if (test_factura) {
                  tagList(
                    tags$h4("Factura"),
                    DT::dataTableOutput(
                      outputId = ns(paste0("escenario_factura_", i))
                    ),
                    tags$br()
                  )
                },
                if (test_paciente) {
                  tagList(
                    tags$h4("Paciente"),
                    DT::dataTableOutput(
                      outputId = ns(paste0("escenario_paciente_", i))
                    ),
                    tags$br()
                  )
                },
                if (test_prestacion) {
                  tagList(
                    tags$h4("Prestación"),
                    DT::dataTableOutput(
                      outputId = ns(paste0("escenario_prestacion_", i))
                    )
                  )
                },
              )
            }
          )
        })
        
        if (!is.null(nota_tecnica$agrupadores_items)) {
          descriptiva_escenarios <- episodios_jerarquia(
            data = datos$data_table,
            columnas =      nota_tecnica_cols, 
            columna_valor = opciones$valor_costo, 
            columna_sep =   nota_tecnica_cols_sep,
            columna_suma =  nota_tecnica_col_valor,
            nivel_1 = input$nota_tecnica_jerarquia_nivel_1_order,
            nivel_2 = input$nota_tecnica_jerarquia_nivel_2_order,
            nivel_3 = input$nota_tecnica_jerarquia_nivel_3_order,
            nivel_4 = input$nota_tecnica_jerarquia_nivel_4_order,
            return_list = TRUE)
        } else {
          descriptiva_escenarios <- list(
            factura = if (test_factura) {
              descriptiva(
                data = datos$data_table,
                columnas = c(
                  nota_tecnica_cols,
                  nota_tecnica_cols_sep), 
                columna_valor = opciones$valor_costo, 
                columna_suma =  input$descriptiva_unidades, 
                prestaciones = FALSE)
            },
            paciente = if (test_paciente) {
              descriptiva(
                data = datos$data_table,
                columnas = c(
                  nota_tecnica_cols,
                  nota_tecnica_cols_sep), 
                columna_valor = opciones$valor_costo, 
                columna_suma =  input$descriptiva_unidades, 
                prestaciones = FALSE)
            },
            prestacion = if (test_prestacion) {
              descriptiva(
                data = datos$data_table,
                columnas = c(
                  nota_tecnica_cols,
                  nota_tecnica_cols_sep), 
                columna_valor = opciones$valor_costo, 
                columna_suma =  input$descriptiva_unidades, 
                prestaciones = test_prestacion)
            }
          )
        }
        
        lapply(
          X = 1:4,
          FUN = function(i) {
            if (test_episodio) {
              output[[paste0("escenario_episodio_", i)]] <- DT::renderDataTable(
                server = FALSE,
                clean_datatable(
                  crear_notatecnica_escenarios(
                    data = descriptiva_escenarios[["episodio"]], 
                    columnas = c(
                      nota_tecnica_cols,
                      nota_tecnica_cols_sep),
                    poblacion = input$nota_tecnica_poblacion,
                    meses = input$nota_tecnica_meses,
                    escenario = i
                  ),
                  columnDefs = list(
                    list(
                      targets = c(
                        length(c(
                          nota_tecnica_cols,
                          nota_tecnica_cols_sep)) + 4),
                      visible = FALSE))
                ) %>%
                  formato_escenarios()
              )
            }
            if (test_factura) {
              output[[paste0("escenario_factura_", i)]] <- DT::renderDataTable(
                server = FALSE,
                clean_datatable(
                  crear_notatecnica_escenarios(
                    data = descriptiva_escenarios[["factura"]], 
                    columnas = c(
                      nota_tecnica_cols,
                      nota_tecnica_cols_sep),
                    poblacion = input$nota_tecnica_poblacion,
                    meses = input$nota_tecnica_meses,
                    escenario = i
                  ),
                  columnDefs = list(
                    list(
                      targets = c(
                        length(c(
                          nota_tecnica_cols,
                          nota_tecnica_cols_sep)) + 4),
                      visible = FALSE))
                ) %>%
                  formato_escenarios()
              )
            }
            if (test_paciente) {
              output[[paste0("escenario_paciente_", i)]] <- DT::renderDataTable(
                server = FALSE,
                clean_datatable(
                  crear_notatecnica_escenarios(
                    data = descriptiva_escenarios[["paciente"]], 
                    columnas = c(
                      nota_tecnica_cols,
                      nota_tecnica_cols_sep),
                    poblacion = input$nota_tecnica_poblacion,
                    meses = input$nota_tecnica_meses,
                    escenario = i
                  ),
                  columnDefs = list(
                    list(
                      targets = c(
                        length(c(
                          nota_tecnica_cols,
                          nota_tecnica_cols_sep)) + 4),
                      visible = FALSE))
                ) %>%
                  formato_escenarios()
              )
            }
            if (test_prestacion) {
              output[[paste0("escenario_prestacion_", i)]] <- DT::renderDataTable(
                server = FALSE,
                clean_datatable(
                  crear_notatecnica_escenarios(
                    data = descriptiva_escenarios[["prestacion"]], 
                    columnas = c(
                      nota_tecnica_cols,
                      nota_tecnica_cols_sep),
                    poblacion = input$nota_tecnica_poblacion,
                    meses = input$nota_tecnica_meses,
                    escenario = i
                  ),
                  columnDefs = list(
                    list(
                      targets = c(
                        length(c(
                          nota_tecnica_cols,
                          nota_tecnica_cols_sep)) + 4),
                      visible = FALSE))
                ) %>%
                  formato_escenarios()
              )
            }
          }
        )
    
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
  
  })
  
}

clean_datatable <- function(data, length = 1000, columnDefs = NULL) {
  return(
    datatable(
      data = data,
      rownames = FALSE, 
      options = list(
        ordering = F,
        scrollX = TRUE,
        pageLength = length,
        dom = "t",
        columnDefs = columnDefs
      )
    )
  )
}

formato_escenarios <- function(x) {
  brks <- c(1:3)
  clrs <- round(seq(255, 40, length.out = 4), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
  return(
    x %>%
      formatCurrency(
        c("CME", "Valor a mes"),
        dec.mark = ",", 
        mark = ".", 
        currency = "$", 
        digits = 0
      ) %>%
      formatStyle(
        "Coe",
        target = "row",
        backgroundColor = styleInterval(
          cuts = brks, values = clrs
        )
      ) %>%
      formatStyle(
        "Frecuencia a mes",
        target = "row",
        backgroundColor = styleEqual(
          levels = c(0, 1),
          values = c('yellow', 'yellow')
        )
      )
  )
}

nota_tecnica_cajas_jerarquia <- function(
  ns,
  items_nivel_1 = NULL,
  items_nivel_2 = NULL,
  items_nivel_3 = NULL, 
  items_nivel_4 = NULL) {
  return(
    tags$div(
      class = "nota_tecnica_jerarquia_row",
      box(
        width = 3,
        orderInput(
          inputId = ns("nota_tecnica_jerarquia_nivel_1"),
          label = actionLink(ns("seleccionar_episodio"), label = "Episodio"),
          items = items_nivel_1,
          width = "100%", 
          height = "100%",
          connect = c(
            ns("nota_tecnica_jerarquia_nivel_2"),
            ns("nota_tecnica_jerarquia_nivel_3"),
            ns("nota_tecnica_jerarquia_nivel_4")))
      ),
      box(
        width = 3,
        orderInput(
          inputId = ns("nota_tecnica_jerarquia_nivel_2"),
          label = actionLink(ns("seleccionar_factura"), label = "Factura"),
          items = items_nivel_2,
          width = "100%",
          height = "100%",
          connect = c(
            ns("nota_tecnica_jerarquia_nivel_1"),
            ns("nota_tecnica_jerarquia_nivel_3"),
            ns("nota_tecnica_jerarquia_nivel_4")))
      ),
      box(
        width = 3,
        orderInput(
          inputId = ns("nota_tecnica_jerarquia_nivel_3"),
          label = actionLink(ns("seleccionar_paciente"), label = "Paciente"),
          items = items_nivel_3,
          width = "100%",
          height = "100%",
          connect = c(
            ns("nota_tecnica_jerarquia_nivel_1"),
            ns("nota_tecnica_jerarquia_nivel_2"),
            ns("nota_tecnica_jerarquia_nivel_4")))
      ),
      box(
        width = 3,
        orderInput(
          inputId = ns("nota_tecnica_jerarquia_nivel_4"),
          label = actionLink(ns("seleccionar_prestacion"), label = "Prestación"),
          items = items_nivel_4,
          width = "100%",
          height = "100%",
          connect = c(
            ns("nota_tecnica_jerarquia_nivel_1"),
            ns("nota_tecnica_jerarquia_nivel_2"),
            ns("nota_tecnica_jerarquia_nivel_3")))
      )
    )
  )
}