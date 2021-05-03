# El módulo de descriptiva y episodios encapsula las funciones básicas
# de descripcion de los datos. 
# La tabla general de descriptiva, frecuencias y gráficos.

episodios_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
    box(
      width = 4,
      style = "min-height: 600px",
      tabsetPanel(
        tabPanel(
          title = "Generales",
          tags$br(),
          selectizeInput(
            inputId = ns("agrupador"),
            label = "Agrupador principal:",
            choices = NULL, 
            multiple = FALSE),
          selectizeInput(
            inputId = ns("separadores"),
            label = "Separadores:",
            choices = NULL,
            multiple = TRUE),
          checkboxGroupInput(
            inputId = ns("tablas"),
            label = "Tablas a generar:",
            choices = c(
              "Descripiva" = "descriptiva",
              "Frecuencias" = "frecuencias"),
            selected = "descriptiva",
            inline = TRUE,
            width = "100%"),
          radioButtons(
            inputId = ns("intervalo"),
            choiceNames = c("Mes", "Semana", "Día"),
            choiceValues = c("mes", "semana", "dia"),
            label = "Intervalo para frecuencias"),
          checkboxInput(
            inputId = ns("episodios"),
            label = "Agrupador por episodios",
            value = FALSE),
          uiOutput(outputId = ns("episodios_col_rel")),
          tags$div(
            style = "overflow-y: scroll; max-height: 300px;",
            uiOutput(outputId = ns("episodios_jerarquia"))),
          tags$br(),
          textOutput(outputId = ns("descriptiva_sumas_registros")),
          textOutput(outputId = ns("descriptiva_sumas_pacientes")),
          textOutput(outputId = ns("descriptiva_sumas_facturas")),
          textOutput(outputId = ns("descriptiva_sumas_valor")),
          tags$br(),
          actionButton(ns("descriptiva_exe"), "Generar")
        ),
        tabPanel(
          title = "Descargas",
          tags$br(),
          tags$b("Descriptiva:"),
          fluidRow(
            column(
              width = 6,
              downloadButton(
                outputId = ns("descriptiva_descargar_csv"),
                label = "CSV",
                style = "width:100%;")
            ),
            column(
              width = 6,
              downloadButton(
                outputId = ns("descriptiva_descargar_xlsx"),
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
        )
      ),
    box(
      width = 8,
      tabsetPanel(
        tabPanel(
          title = "Tabla",
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
        if (input$episodios) {
          updateSelectizeInput(
            session = session,
            inputId = "episodios_col_rel",
            choices = opciones$colnames,
            selected = "nro_factura"
          )
        }
        updateSelectizeInput(
          session = session,
          inputId = "agrupador",
          choices = c("Ninguno", opciones$colnames)
        )
        updateSelectizeInput(
          session = session,
          inputId = "separadores",
          choices = opciones$colnames
        )
      })
      
      observeEvent(input$episodios, {
        if (!input$episodios) output$episodios_col_rel <- renderUI({})
        if (input$episodios) {
          output$episodios_col_rel <- renderUI({
            selectizeInput(
              inputId = ns("episodios_col_rel"),
              label = "Relacionar episodios por:",
              choices = opciones$colnames,
              selected = "nro_factura",
              multiple = FALSE)
          })
        }
      })
      
      cambio_columnas <- reactive({
        list(input$agrupador, input$episodios)
      })
      
      observeEvent(cambio_columnas(), {
        if (!is.null(opciones$colnames) && 
            input$agrupador %notin% c("", "Ninguno")) {
          tryCatch(
            expr = {
              widget_jerarquia <- radioButtons(
                inputId = ns("unidades"),
                label = "Unidad de descriptiva",
                selected = episodios$unidad_descriptiva,
                choiceNames = c("Prestación", "Paciente", "Factura"),
                choiceValues = c(
                  "prestacion",
                  "nro_identificacion",
                  "nro_factura"
                )
              )
              episodios$agrupadores_items <- NULL
              if (input$episodios) {
                episodios$agrupadores_items <- opciones$tabla %>%
                  select(!!as.name(input$agrupador)) %>%
                  distinct() %>%
                  pull(!!as.name(input$agrupador))
                if (length(episodios$agrupadores_items) <= 60) {
                    if (opciones$perfil_enable) {
                      widget_jerarquia <- perfil_jerarquia(
                        perfiles = opciones$perfil_lista,
                        perfil_select = opciones$perfil_selected,
                        items = episodios$agrupadores_items,
                        funcion_jerarquia = descriptiva_jerarquia,
                        ns = ns
                      )
                    }
                    if (!opciones$perfil_enable) {
                      widget_jerarquia <- descriptiva_jerarquia(
                        ns = ns,
                        items_nivel_4 = episodios$agrupadores_items
                      )
                    }
                }
                if (length(episodios$agrupadores_items) > 60) {
                  episodios$agrupadores_items <- NULL
                  updateCheckboxInput(
                    inputId = "episodios",
                    value = FALSE
                  )
                }
              }
              output$episodios_jerarquia <- renderUI({widget_jerarquia})
              episodios$unidad_descriptiva <- input$descriptiva_unidades
            },
            error = function(e) {
              print(e)
              sendSweetAlert(
                session = session,
                title = "Error", 
                type = "error",
                text = "Por favor revisar los parametros de carga de datos, columnas, formato de fecha y los datos. Si este problema persiste ponerse en contacto con un administrador."
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
      
      observeEvent(input$descriptiva_exe, {
        if(!is.null(opciones$colnames)) {
          if(input$agrupador %notin% c("", "Ninguno")) {
            tryCatch(
              expr = {
                withProgress(message = "Calculando descriptiva...",{
                  episodios$n_pacientes <- paste(
                    "Número de pacientes:",
                    formatC(
                      {opciones$tabla %>%
                          distinct(nro_identificacion) %>%
                          count() %>%
                          pull()},
                      big.mark = ".",
                      decimal.mark = ",",
                      format = "f",
                      digits = 0),
                    sep = " ")
                  if ("nro_factura" %in% opciones$colnames) {
                    episodios$n_facturas <- paste(
                      "Número de facturas:",
                      formatC(
                        {opciones$tabla %>%
                            distinct(nro_factura) %>%
                            count() %>%
                            pull()},
                        big.mark = ".",
                        decimal.mark = ",",
                        format = "f",
                        digits = 0
                      ),
                      sep = " "
                    )
                  }
                })
              },
              error = function(e) {
                print(e)
                sendSweetAlert(
                  session = session,
                  title = "Error", 
                  type = "error",
                  text = "Por favor revisar los parametros de carga de datos, columnas, formato de fecha y los datos. Si este problema persiste ponerse en contacto con un administrador."
                )
              }
            )
          }
        }
      })

      observeEvent(input$descriptiva_exe, {
        if(opciones$datos_cargados &&
          input$agrupador %notin% c("", "Ninguno")) {
          print(input$tablas)
          print(input$unidades)
          agrupador <- input$agrupador
          if (input$episodios) episodios_col_rel <- input$episodios_col_rel
          separadores <- input$separadores
          episodios$agrupador <- agrupador
          episodios$separadores <- separadores
          if ("descriptiva" %in% input$tablas) {
            if (input$episodios) {
              episodios$tabla <- episodios_jerarquia(
                data = opciones$tabla,
                columnas =      agrupador, 
                columna_valor = opciones$valor_costo, 
                columna_sep =   separadores,
                columna_suma =  episodios_col_rel,
                nivel_1 = input$episodios_jerarquia_nivel_1_order$text,
                nivel_2 = input$episodios_jerarquia_nivel_2_order$text,
                nivel_3 = input$episodios_jerarquia_nivel_3_order$text,
                nivel_4 = input$episodios_jerarquia_nivel_4_order$text,
                frec_cantidad = opciones$cantidad)
            }
            if (!input$episodios) {
              episodios$tabla <- descriptiva(
                data = opciones$tabla,
                columnas = c(agrupador, separadores),
                columna_valor = opciones$valor_costo,
                columna_suma = input$unidades,
                prestaciones = (input$unidades == "prestacion"),
                frec_cantidad = opciones$cantidad)
              episodios$tabla[["data"]] <-
                list("temporal" = episodios$tabla[["data"]])
              names(episodios$tabla[["data"]]) <- input$unidades
            }
            episodios$lista_agrupadores <- 
              episodios$tabla[["descriptiva"]][, c(
                agrupador, separadores), with = FALSE]
            episodios$tabla_titulo <- paste(
              "Descriptiva de", agrupador,
              ifelse(
                test = is.null(separadores),
                yes = "", no = "separada por"),
              separar_spanish(separadores),
              collapse = " ")
          }
          if ("frecuencias" %in% input$tablas) {
            if (input$episodios) {
              episodios$frecuencias <- frecuencias_jerarquia(
                data = opciones$tabla,
                columnas =      agrupador, 
                columna_fecha = "fecha_prestacion",
                columna_sep =   separadores,
                columna_suma =  episodios_col_rel,
                frec_cantidad = opciones$cantidad,
                nivel_1 = input$episodios_jerarquia_nivel_1_order$text,
                nivel_2 = input$episodios_jerarquia_nivel_2_order$text,
                nivel_3 = input$episodios_jerarquia_nivel_3_order$text,
                nivel_4 = input$episodios_jerarquia_nivel_4_order$text,
                intervalo = input$intervalo)[["descriptiva"]]
            }
            if (!input$episodios) {
              episodios$frecuencias <- frecuencias(
                data = opciones$tabla,
                agrupador = c(agrupador, separadores),
                columna_fecha = "fecha_prestacion",
                columna_suma = input$unidades,
                prestaciones = (input$unidades == "prestacion"),
                frec_cantidad = opciones$cantidad,
                intervalo = input$intervalo
              )
            }
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
              scrollY = "500px"),
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
            scrollY = "500px"),
          rownames= FALSE)
      })
      
      output$grafico_barras_select_agrupador <- 
        DT::renderDataTable({
          if (nrow(episodios$tabla[["descriptiva"]]) != 0) {
            DT::datatable(
              episodios$tabla[["descriptiva"]][, c(
                episodios$agrupador, episodios$separadores), with = FALSE],
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
                columns = 1:length(
                  c(episodios$agrupador, episodios$separadores)),
                fontSize = '95%')
          }
      })
      
      output$histograma_select_agrupador <- DT::renderDataTable({
        if (nrow(episodios$tabla[["descriptiva"]]) != 0) {
          DT::datatable(
            episodios$tabla[["descriptiva"]][, c(
              episodios$agrupador, episodios$separadores), with = FALSE],
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 25,
              dom = 'ft',
              autoWidth = FALSE,
              ordering = TRUE, 
              scrollX = TRUE,
              scrollY = "370px"),
            rownames = FALSE) %>%
            formatStyle(
              columns = 1:length(
                c(episodios$agrupador, episodios$separadores)),
              fontSize = '95%')
        }
      })
      
      output$caja_de_bigotes_select_agrupador <- 
        DT::renderDataTable({
          if (nrow(episodios$tabla[["descriptiva"]]) != 0) {
            DT::datatable(
              episodios$tabla[["descriptiva"]][, c(
                episodios$agrupador, episodios$separadores), with = FALSE],
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
                columns = 1:length(
                  c(episodios$agrupadores, episodios$separadores)),
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

      output$descriptiva_sumas_pacientes <- renderText({
        episodios$n_pacientes
      })

      output$descriptiva_sumas_facturas <- renderText({
        episodios$n_facturas
      })
      
      output$descriptiva_descargar_csv <- downloadHandler(
        filename = function() {
          paste("Descriptiva",
                ".csv", sep = "")
        },
        content = function(file) {
          write.csv(
            x = as.data.frame(episodios$tabla[["descriptiva"]]),
            file = file, 
            row.names = FALSE,
            na = "")
        }, 
        contentType = "text/csv"
      )
      
      output$descriptiva_descargar_xlsx <- downloadHandler(
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
