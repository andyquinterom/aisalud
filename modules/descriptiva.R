episodios_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 3,
        tabsetPanel(
          tabPanel(
            title = "Generales",
            tags$br(),
            checkboxInput(
              inputId = ns("episodios_enable"),
              label = "Agrupar por episodios",
              value = FALSE
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
            tags$br()
          ),
          tabPanel(
            title = "Avanzado",
            tags$br(),
            tags$p("Para poder hacer uso de los gráficos se debe generar la descriptiva"),
            checkboxInput(inputId = ns("descriptiva_activar"),
                          label = "Generar descriptiva",
                          value = TRUE),
            checkboxInput(inputId = ns("frecuencias_activar"),
                          label = "Calcular frecuencias",
                          value = FALSE),
            radioButtons(
              inputId = ns("frecuencias_intervalo"),
              choiceNames = c("Mes", "Semana", "Día"),
              choiceValues = c("mes", "semana", "dia"),
              label = "Intervalo para frecuencias"
            )
          ),
          tabPanel(
            title = "Descargas",
            tags$br(),
            tags$b("Descriptiva:"),
            fluidRow(
              column(
                width = 6,
                downloadButton(
                  outputId = ns("episodios_descargar_csv"),
                  label = "CSV",
                  style = "width:100%;")
              ),
              column(
                width = 6,
                downloadButton(
                  outputId = ns("episodios_descargar_xlsx"),
                  label = "Excel",
                  style = "width:100%;")
              )
            ),
            tags$br(),
            tags$b("Frecuencias:"),
            fluidRow(
              column(
                width = 6,
                downloadButton(
                  outputId = ns("frecuencias_descargar_csv"),
                  label = "CSV",
                  style = "width:100%;")
              ),
              column(
                width = 6,
                downloadButton(
                  outputId = ns("frecuencias_descargar_xlsx"),
                  label = "Excel",
                  style = "width:100%;")
              )
            )
          )
        )),
    box(
      width = 9,
      tabsetPanel(
        tabPanel(
          title = "Tabla",
          tags$br(),
          tags$h2(textOutput(ns("tabla_titulo")), class = "titulo_center"),
          tags$br(),
          div(
            DT::dataTableOutput(outputId = ns("episodios_tabla")) %>%
              withSpinner(),
            style = "font-size:90%")
        ),
        tabPanel(
          title = "Frecuencias",
          tags$br(),
          div(
            DT::dataTableOutput(outputId = ns("frecuencias_tabla")) %>%
              withSpinner(),
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
              ) %>%
                withSpinner()
            ),
            column(
              width = 4,
              actionButton(
                inputId = ns("histograma_ejecutar"),
                label = "Generar histograma",
                width = "100%"
              ),
              tags$br(),
              tags$br(),
              sliderTextInput(
                inputId = ns("histograma_numero_columnas"),
                label = "Número de columnas",
                choices = c("Auto", 5, 10, 20, 25, 50),
                width = "100%", 
                selected = "Auto"
              ),
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
              ) %>%
                withSpinner()
            ),
            column(
              width = 4,
              actionButton(
                inputId = ns("caja_de_bigotes_ejecutar"),
                label = "Generar caja de bigotes",
                width = "100%"
              ),
              tags$br(),
              tags$br(),
              DT::dataTableOutput(
                outputId = ns("caja_de_bigotes_select_agrupador")
              )
            )
          )
        ),
        tabPanel(
          title = "Gráfico de barras",
          tags$br(),
          fluidRow(
            column(
              width = 8,
              tags$h3(
                textOutput(ns("grafico_barras_titulo")), class = "titulo_center"),
              plotlyOutput(
                outputId = ns("grafico_barras_render")
              ) %>%
                withSpinner()
            ),
            column(
              width = 4,
              selectizeInput(
                width = "100%",
                inputId = ns("grafico_barras_indicador"),
                label = "Indicador:",
                choices = c("Suma" = "Suma",
                            "Frecuencia" = "Frecuencia",
                            "Media" = "Media", 
                            "Mediana" = "P50",
                            "Variación" = "Coef.var")
              ),
              actionButton(
                inputId = ns("grafico_barras_ejecutar"),
                label = "Generar gráfico",
                width = "100%"
              ),
              tags$br(),
              tags$br(),
              DT::dataTableOutput(
                outputId = ns("grafico_barras_select_agrupador")
              )
            )
          )
        )
      )
      )
    )
  )
}

episodios_server <- function(id, opciones, conn) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      ns <- NS(id)
      
      episodios <- reactiveValues(
        tabla = list("descriptiva" = data.table(), "data" = data.table()),
        agrupadores_items = NULL)
      
      observeEvent(opciones$colnames, {
        if (input$episodios_enable) {
          updateSelectizeInput(
            session = session,
            inputId = "episodios_col_valor",
            choices = opciones$colnames,
            selected = "nro_identificacion"
          )
        }
        updateSelectizeInput(
          session = session,
          inputId = "episodios_cols",
          choices = c("Ninguno", opciones$colnames)
        )
        updateSelectizeInput(
          session = session,
          inputId = "episodios_cols_sep",
          choices = opciones$colnames
        )
      })
      
      observeEvent(input$episodios_enable, {
        if (input$episodios_enable) {
          output$episodios_col_valor_out <- renderUI({
            selectizeInput(
              inputId = ns("episodios_col_valor"),
              label = "Sumar valor por:",
              choices = opciones$colnames,
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
        if (!is.null(opciones$colnames) && 
            input$episodios_cols %notin% c("", "Ninguno")) {
          tryCatch(
            expr = {
              if (input$episodios_enable) {
                episodios$agrupadores_items <- opciones$tabla %>%
                  select(!!as.name(input$episodios_cols)) %>%
                  distinct() %>%
                  pull(!!as.name(input$episodios_cols))
                if (length(episodios$agrupadores_items) <= 60) {
                  output$episodios_jerarquia <- renderUI({
                    if (opciones$perfil_enable) {
                      perfil_jerarquia(
                        perfiles = opciones$perfil_lista,
                        perfil_select = opciones$perfil_selected,
                        items = episodios$agrupadores_items,
                        funcion_jerarquia = descriptiva_jerarquia,
                        ns = ns
                      )
                    } else {
                      descriptiva_jerarquia(
                        ns = ns,
                        items_nivel_4 = episodios$agrupadores_items
                      )
                    }
                  })
                } else {
                  episodios$agrupadores_items <- NULL
                  output$episodios_jerarquia <- renderUI({
                    radioButtons(
                      inputId = ns("descriptiva_unidades"),
                      label = "Unidad de descriptiva",
                      selected = episodios$unidad_descriptiva,
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
                  episodios$unidad_descriptiva <- input$descriptiva_unidades
                }
              } else {
                episodios$agrupadores_items <- NULL
                output$episodios_jerarquia <- renderUI({
                  radioButtons(
                    inputId = ns("descriptiva_unidades"),
                    label = "Unidad de descriptiva",
                    selected = episodios$unidad_descriptiva,
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
                episodios$unidad_descriptiva <- input$descriptiva_unidades
              }
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
        
        if(!is.null(opciones$colnames)) {
          if(!is.null(input$episodios_cols) &&
             input$episodios_cols %notin% c("", "Ninguno")) {
            tryCatch(
              expr = {
                episodios_cols <- input$episodios_cols
                if (input$episodios_enable) {
                  episodios_col_valor <- input$episodios_col_valor
                }
                episodios_cols_sep <- input$episodios_cols_sep
                episodios$cols <- episodios_cols
                episodios$cols_sep <- episodios_cols_sep
                withProgress(message = "Calculando descriptiva...",{
                  if (!is.null(episodios$agrupadores_items)) {
                    if (input$descriptiva_activar) {
                      episodios$tabla <- episodios_jerarquia(
                        data = opciones$tabla,
                        columnas =      episodios_cols, 
                        columna_valor = opciones$valor_costo, 
                        columna_sep =   episodios_cols_sep,
                        columna_suma =  episodios_col_valor,
                        frec_cantidad = opciones$cantidad,
                        nivel_1 = input$episodios_jerarquia_nivel_1_order,
                        nivel_2 = input$episodios_jerarquia_nivel_2_order,
                        nivel_3 = input$episodios_jerarquia_nivel_3_order,
                        nivel_4 = input$episodios_jerarquia_nivel_4_order)
                    }
                    if (input$frecuencias_activar) {
                      episodios$frecuencias <- frecuencias_jerarquia(
                        data = opciones$tabla,
                        columnas =      episodios_cols, 
                        columna_fecha = "fecha_prestacion",
                        columna_sep =   episodios_cols_sep,
                        columna_suma =  episodios_col_valor,
                        frec_cantidad = opciones$cantidad,
                        nivel_1 = input$episodios_jerarquia_nivel_1_order,
                        nivel_2 = input$episodios_jerarquia_nivel_2_order,
                        nivel_3 = input$episodios_jerarquia_nivel_3_order,
                        nivel_4 = input$episodios_jerarquia_nivel_4_order,
                        intervalo = input$frecuencias_intervalo)[["descriptiva"]]
                    }
                  } else {
                    if (input$descriptiva_activar) {
                      episodios$tabla <- descriptiva(
                        data = opciones$tabla,
                        columnas = c(
                          episodios_cols,
                          episodios_cols_sep
                        ),
                        columna_valor = opciones$valor_costo,
                        columna_suma = input$descriptiva_unidades,
                        prestaciones = (input$descriptiva_unidades == "prestacion"),
                        frec_cantidad = opciones$cantidad
                      )
                      episodios$tabla[["data"]] <- 
                        list("temporal" = episodios$tabla[["data"]])
                      names(episodios$tabla[["data"]]) <-
                        input$descriptiva_unidades
                    }
                    if (input$frecuencias_activar) {
                      episodios$frecuencias <- frecuencias(
                        columna_fecha = "fecha_prestacion",
                        data = opciones$tabla,
                        agrupador = c(
                          episodios_cols,
                          episodios_cols_sep
                        ),
                        columna_suma = input$descriptiva_unidades,
                        prestaciones = (input$descriptiva_unidades == "prestacion"),
                        frec_cantidad = opciones$cantidad,
                        intervalo = input$frecuencias_intervalo
                      )
                    }
                  }
                  
                  if (input$descriptiva_activar) {
                    episodios$lista_agrupadores <- 
                      episodios$tabla[["descriptiva"]][, c(
                        episodios_cols, episodios_cols_sep), with = FALSE]
                    
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
                  }
                  
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
      
      output$episodios_tabla <- DT::renderDataTable({
        if (nrow(episodios$tabla[["descriptiva"]]) != 0) {
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
              c('P25','P50','P75','P90','Media','Desv.tipica'),
              dec.mark = ",", mark = ".") %>%
            formatRound("Coef.var",
                        dec.mark = ",", mark = ".", digits = 2) %>%
            formatCurrency(c('Suma','Min.','Max.','Rango'),
                           digits=0,
                           dec.mark = ",", mark = ".")
        }
      })
      
      output$frecuencias_tabla <- DT::renderDataTable({
        DT::datatable(
          episodios$frecuencias,
          options = list(
            language = list(
              url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
            pageLength = 50,
            autoWidth = FALSE,
            ordering= TRUE, 
            scrollX = TRUE,
            scrollY = "60vh"),
          rownames= FALSE)
      })
      
      output$grafico_barras_select_agrupador <- 
        DT::renderDataTable({
          if (nrow(episodios$tabla[["descriptiva"]]) != 0) {
            DT::datatable(
              episodios$tabla[["descriptiva"]][, c(
                episodios$cols, episodios$cols_sep), with = FALSE],
              options = list(
                language = list(
                  url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 10000,
                dom = 'ft',
                autoWidth = FALSE,
                ordering= TRUE, 
                scrollX = TRUE,
                scrollY = "370px"),
              rownames = FALSE) %>%
              formatStyle(
                columns = 1:length(c(episodios$cols, episodios$cols_sep)),
                fontSize = '95%')
          }
      })
      
      output$histograma_select_agrupador <- DT::renderDataTable({
        if (nrow(episodios$tabla[["descriptiva"]]) != 0) {
          DT::datatable(
            episodios$tabla[["descriptiva"]][, c(
              episodios$cols, episodios$cols_sep), with = FALSE],
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 10000,
              dom = 'ft',
              autoWidth = FALSE,
              ordering = TRUE, 
              scrollX = TRUE,
              scrollY = "370px"),
            rownames = FALSE) %>%
            formatStyle(
              columns = 1:length(c(episodios$cols, episodios$cols_sep)),
              fontSize = '95%')
        }
      })
      
      output$caja_de_bigotes_select_agrupador <- 
        DT::renderDataTable({
          if (nrow(episodios$tabla[["descriptiva"]]) != 0) {
            DT::datatable(
              episodios$tabla[["descriptiva"]][, c(
                episodios$cols, episodios$cols_sep), with = FALSE],
              options = list(
                language = list(
                  url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 10000,
                dom = 'ft',
                autoWidth = FALSE,
                ordering = TRUE, 
                scrollX = TRUE,
                scrollY = "370px"),
              rownames = FALSE) %>%
              formatStyle(
                columns = 1:length(c(episodios$cols, episodios$cols_sep)),
                fontSize = '95%')
          }
        })
      
      output$histograma_titulo <- renderText({
        "Histograma de valores"
      })
      
      output$caja_de_bigotes_titulo <- renderText({
        "Distribución de valores"
      })
      
      observeEvent(input$histograma_ejecutar, {
        if (!is.null(input$histograma_select_agrupador_rows_selected)) {
          if (input$histograma_numero_columnas == "Auto") {
            numero_bins <- 20
          } else {
            numero_bins <- input$histograma_numero_columnas
          }
          episodios$histograma_plot <- histograma_agrupador(
            titulo = "Histograma",
            data = episodios$tabla[["data"]],
            columnas_sep = episodios$lista_agrupadores[
              input$histograma_select_agrupador_rows_selected],
            numero_bins = numero_bins
          )
        }
      })
      
      output$histograma_render <- renderPlotly({
        episodios$histograma_plot
      })
      
      observeEvent(input$caja_de_bigotes_ejecutar, {
        if (!is.null(input$caja_de_bigotes_select_agrupador_rows_selected)) {
          episodios$caja_de_bigotes_plot <- caja_de_bigotes_agrupador(
            data = episodios$tabla[["descriptiva"]],
            columnas_sep = episodios$lista_agrupadores[
              input$caja_de_bigotes_select_agrupador_rows_selected]
          )
        }
      })
      
      output$caja_de_bigotes_render <- renderPlotly({
        episodios$caja_de_bigotes_plot
      })
      
      observeEvent(input$grafico_barras_ejecutar, {
        if (!is.null(input$grafico_barras_select_agrupador_rows_selected)) {
          episodios$grafico_barras_plot <- grafico_barras_descriptiva(
            data = episodios$tabla[["descriptiva"]],
            columna_numeros = input$grafico_barras_indicador,
            columnas_sep = episodios$lista_agrupadores[
              input$grafico_barras_select_agrupador_rows_selected
            ]
          )
        }
      })
      
      output$grafico_barras_render <- renderPlotly({
        episodios$grafico_barras_plot
      })
      
      output$descriptiva_sumas_valor <- renderText({
        if (!is.null(opciones$colnames)) {
          if(nrow(episodios$tabla[["descriptiva"]]) > 0) {
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
        if (opciones$datos_cargados) {
          paste("Número de registros:",
                formatC(
                  {opciones$tabla %>%
                      ungroup() %>%
                      count() %>%
                      collect() %>%
                      unlist() %>%
                      unname()},
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
        if (opciones$datos_cargados &&
            "nro_identificacion" %in% opciones$colnames) {
          paste("Número de pacientes:",
                formatC(
                  {opciones$tabla %>%
                      group_by(nro_identificacion) %>%
                      count() %>%
                      ungroup() %>%
                      count() %>%
                      collect() %>%
                      unlist() %>%
                      unname()},
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
        if (opciones$datos_cargados &&
            "nro_factura" %in% opciones$colnames) {
          paste("Número de facturas:",
                formatC(
                  {opciones$tabla %>%
                      group_by(nro_factura) %>%
                      count() %>%
                      ungroup() %>%
                      count() %>%
                      collect() %>%
                      unlist() %>%
                      unname()},
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
                ".csv", sep = "")
        },
        content = function(file) {
          write.csv(
            x = episodios$tabla[["descriptiva"]],
            file = file, 
            row.names = FALSE,
            na = "")
        }, 
        contentType = "text/csv"
      )
      
      output$episodios_descargar_xlsx <- downloadHandler(
        filename = function() {
          paste("Descriptiva",
                ".xlsx", sep = "")
        },
        content = function(file) {
          write_xlsx(
            x = as.data.frame(episodios$tabla[["descriptiva"]]),
            path = file)
        }, 
        contentType = "xlsx"
      )
      
      output$frecuencias_descargar_csv <- downloadHandler(
        filename = function() {
          paste("Frecuencias",
                ".csv", sep = "")
        },
        content = function(file) {
          write.csv(
            x = episodios$frecuencias,
            file = file, 
            row.names = FALSE,
            na = "")
        }, 
        contentType = "text/csv"
      )
      
      output$frecuencias_descargar_xlsx <- downloadHandler(
        filename = function() {
          paste("Frecuencias",
                ".xlsx", sep = "")
        },
        content = function(file) {
          write_xlsx(
            x = as.data.frame(episodios$frecuencias),
            path = file)
        }, 
        contentType = "xlsx"
      )
    }
    
  )
}
  
  