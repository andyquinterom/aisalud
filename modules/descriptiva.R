# El módulo de descriptiva y episodios encapsula las funciones básicas
# de descripcion de los datos.
# La tabla general de descriptiva, frecuencias y gráficos.

descriptiva_ui <- function(id) {
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
            agrupadores_widget(
              id = id,
              separadores = TRUE,
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
                choiceNames = c("Mes", "Día"),
                choiceValues = c("mes", "dia"),
                label = "Intervalo para frecuencias")
            ),
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
                  height = "500px",
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
                  outputId = ns("histograma_agrupador")
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
                  textOutput(ns("caja_de_bigotes_titulo")),
                  class = "titulo_center"),
                plotlyOutput(
                  height = "500px",
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
                  outputId = ns("bigotes_agrupador")
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
                  textOutput(
                    outputId = ns("grafico_barras_titulo")),
                  class = "titulo_center"),
                plotlyOutput(
                  height = "500px",
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
                  outputId = ns("barras_agrupador")
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server module para descriptiva
descriptiva_server <- function(id, opciones, conn) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- NS(id)

      # Valores reactivos necesarios para la ejecución del módulo
      episodios <- reactiveValues(
        tabla = list("descriptiva" = data.table(), "data" = data.table()),
        frecuencias = data.table(),
        agrupadores_items = NULL)

      # Se observa que el usuario haga click en los titulos de las unidades
      # de conteo en el widget de jerarquia.
      # De esta manera se pueden mover los diferentes agrupadores de manera
      # sencilla entre unidades.

      episodios_jerarquia_server(
        episodios = episodios,
        opciones = opciones,
        id = id,
        separadores = TRUE)

      observeEvent(input$descriptiva_exe, {
        # Se valide que hayan datos cargados y un agrupador seleccionado
        if (opciones$datos_cargados &&
          input$agrupador %notin% c("", "Ninguno")) {
          tryCatch(
            expr = {
              withProgress(message = "Calculando datos iniciales...", {
                cache_id <- digest(
                  object = list("desc_inf", opciones$tabla_query),
                  algo = "xxhash32",
                  seed = 1)
                check_cache <- cache_id %in% names(opciones$cache)
                # Se calcula el total de algunos indicadores necesarios
                if (check_cache) {
                  episodios$n_pacientes <- 
                    opciones$cache[[cache_id]][["n_pacientes"]]
                  episodios$n_facturas <-
                    opciones$cache[[cache_id]][["n_facturas"]]
                }
                if (!check_cache) {
                  episodios$n_pacientes <- paste(
                    "Número de pacientes:",
                    formatC({
                      opciones$tabla %>%
                        distinct(nro_identificacion) %>%
                        count() %>%
                        pull()
                      },
                      big.mark = ".",
                      decimal.mark = ",",
                      format = "f",
                      digits = 0),
                    sep = " ")
                  if ("nro_factura" %in% opciones$colnames) {
                    episodios$n_facturas <- paste(
                      "Número de facturas:",
                      formatC({
                        opciones$tabla %>%
                          distinct(nro_factura) %>%
                          count() %>%
                          pull()
                        },
                        big.mark = ".",
                        decimal.mark = ",",
                        format = "f",
                        digits = 0
                      ),
                      sep = " "
                    )
                  }
                  opciones$cache[[cache_id]] <- list(
                    n_pacientes = episodios$n_pacientes,
                    n_facturas = episodios$n_facturas)
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
      })

      # Generar descriptiva
      observeEvent(input$descriptiva_exe, {
        withProgress(message = "Calculando descriptiva", {
          if (opciones$datos_cargados &&
            input$agrupador %notin% c("", "Ninguno") &&
            "descriptiva" %in% input$tablas) {
            agrupador <- input$agrupador
            if (input$episodios) episodios_col_rel <- input$episodios_col_rel
            separadores <- input$separadores
            episodios$agrupador <- agrupador
            episodios$separadores <- separadores
              # Si se va a generar por episodios
              if (input$episodios) {
                # Se genera un ID para el cache y se busca si ya ha sido
                # generado en el pasado
                cache_id <- digest(
                  object = list(
                    "desc_ep", opciones$tabla_query,
                    columnas =      agrupador,
                    columna_valor = opciones$valor_costo,
                    columna_sep =   separadores,
                    columna_suma =  episodios_col_rel,
                    nivel_1 = input$episodios_jerarquia_nivel_1_order$text,
                    nivel_2 = input$episodios_jerarquia_nivel_2_order$text,
                    nivel_3 = input$episodios_jerarquia_nivel_3_order$text,
                    nivel_4 = input$episodios_jerarquia_nivel_4_order$text,
                    frec_cantidad = opciones$cantidad),
                  algo = "xxhash32",
                  seed = 1)
                check_cache <- cache_id %in% names(opciones$cache)
                if (check_cache) episodios$tabla <- opciones$cache[[cache_id]]
                if (!check_cache) {
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
                  opciones$cache[[cache_id]] <- episodios$tabla
                }
              }
              if (!input$episodios) {
                # Si se va a generar de manera tradicional
                # Se checkea un ID para el cache y se busca si ha sido
                # generada en el pasado
                cache_id <- digest(
                  object = list(
                    "desc", opciones$tabla_query,
                    columnas = c(agrupador, separadores),
                    columna_valor = opciones$valor_costo,
                    columna_suma = input$unidades,
                    prestaciones = (input$unidades == "prestacion"),
                    frec_cantidad = opciones$cantidad),
                  algo = "xxhash32",
                  seed = 1)
                check_cache <- cache_id %in% names(opciones$cache)
                if (check_cache) episodios$tabla <- opciones$cache[[cache_id]]
                if (!check_cache) {
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
                  opciones$cache[[cache_id]] <- episodios$tabla
                }
              }
              # lista de los agrupadores únicos
              episodios$lista_agrupadores <-
                episodios$tabla[["descriptiva"]] %>%
                  select(!!!rlang::syms(unique(c(agrupador, separadores))))
              episodios$tabla_titulo <- paste(
                "Descriptiva de", agrupador,
                ifelse(
                  test = is.null(separadores),
                  yes = "", no = "separada por"),
                separar_spanish(separadores),
                collapse = " ")
          }
        })
      })

      # Generar frecuencias
      observeEvent(input$descriptiva_exe, {
        withProgress(message = "Calculando frecuencias", {
          if (opciones$datos_cargados &&
            input$agrupador %notin% c("", "Ninguno") &&
            "frecuencias" %in% input$tablas) {
            agrupador <- input$agrupador
            if (input$episodios) episodios_col_rel <- input$episodios_col_rel
            separadores <- input$separadores
            episodios$agrupador <- agrupador
            episodios$separadores <- separadores
            # Validacion por episodio o tradicional
            if (input$episodios) {
              cache_id <- digest(
                object = list(
                  "frec_ep", opciones$tabla_query,
                  columnas =      agrupador,
                  columna_fecha = "fecha_prestacion",
                  columna_sep =   separadores,
                  columna_suma =  episodios_col_rel,
                  frec_cantidad = opciones$cantidad,
                  nivel_1 = input$episodios_jerarquia_nivel_1_order$text,
                  nivel_2 = input$episodios_jerarquia_nivel_2_order$text,
                  nivel_3 = input$episodios_jerarquia_nivel_3_order$text,
                  nivel_4 = input$episodios_jerarquia_nivel_4_order$text,
                  intervalo = input$intervalo),
                algo = "xxhash32",
                seed = 1)
              check_cache <- cache_id %in% names(opciones$cache)
              if (check_cache) episodios$frecuencias <-
                opciones$cache[[cache_id]]
              if (!check_cache) {
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
                opciones$cache[[cache_id]] <- episodios$frecuencias
              }
            }
            if (!input$episodios) {
              cache_id <- digest(
                object = list(
                  "frec", opciones$tabla_query,
                  agrupador = c(agrupador, separadores),
                  columna_fecha = "fecha_prestacion",
                  columna_suma = input$unidades,
                  prestaciones = (input$unidades == "prestacion"),
                  frec_cantidad = opciones$cantidad,
                  intervalo = input$intervalo),
                algo = "xxhash32",
                seed = 1)
              check_cache <- cache_id %in% names(opciones$cache)
              if (check_cache) episodios$frecuencias <-
                opciones$cache[[cache_id]]
              if (!check_cache) {
                episodios$frecuencias <- frecuencias(
                  data = opciones$tabla,
                  agrupador = c(agrupador, separadores),
                  columna_fecha = "fecha_prestacion",
                  columna_suma = input$unidades,
                  prestaciones = (input$unidades == "prestacion"),
                  frec_cantidad = opciones$cantidad,
                  intervalo = input$intervalo
                )
                opciones$cache[[cache_id]] <- episodios$frecuencias
              }
            }
          }
        })
      })

      # Render tabla de descriptiva
      output$episodios_tabla <- DT::renderDataTable({
        if (nrow(episodios$tabla[["descriptiva"]]) > 0) {
          agrupador <- episodios$agrupador
          separadores <- episodios$separadores
          # Número total de agrupadores únicos
          distinct_agrupadores <- n_distinct(c(agrupador, separadores))
          DT::datatable(
            episodios$tabla[["descriptiva"]],
            extensions = "FixedColumns",
            colnames = c(
              "Unidad de conteo" = "unidad_conteo"
            ),
            options = list(
              fixedColumns = list(leftColumns = distinct_agrupadores + 1),
              language = list(
                url = dt_spanish),
              pageLength = 20,
              autoWidth = FALSE,
              ordering = TRUE,
              scrollX = TRUE,
              scrollY = "500px"),
            rownames = FALSE) %>%
            formatCurrency(
              c("P25", "P50", "P75", "P90", "Media", "Desv.tipica"),
              dec.mark = ",", mark = ".") %>%
            formatRound("Coef.var",
                        dec.mark = ",", mark = ".", digits = 2) %>%
            formatCurrency(c("Suma", "Min.", "Max.", "Rango"),
                           digits = 0,
                           dec.mark = ",", mark = ".") %>%
            formatStyle(TRUE, backgroundColor = "white")
        }
      })

      # Render tabla de frecuencias
      output$frecuencias_tabla <- DT::renderDataTable({
        if (nrow(episodios$frecuencias) != 0) {
          agrupador <- episodios$agrupador
          separadores <- episodios$separadores
          # Número de agrupadores únicos
          distinct_agrupadores <- n_distinct(c(agrupador, separadores))
          DT::datatable(
            episodios$frecuencias,
            extensions = "FixedColumns",
            colnames = c(
              "Frecuencia media" = "frec_media",
              "Frecuencia total" = "frec_suma",
              "Unidad de conteo" = "unidad_conteo"),
            options = list(
              fixedColumns = list(leftColumns = distinct_agrupadores + 1),
              language = list(
                url = dt_spanish),
              pageLength = 20,
              autoWidth = FALSE,
              ordering = TRUE,
              scrollX = TRUE,
              scrollY = "500px"),
            rownames = FALSE) %>%
          formatStyle(TRUE, backgroundColor = "white") %>%
          formatRound(
            columns = (2 + distinct_agrupadores):ncol(episodios$frecuencias),
            dec.mark = ",", mark = ".", digits = 2
          )
        }
      })

      # Gráfico de barras
      output$barras_agrupador <-
        DT::renderDataTable({
          if (nrow(episodios$tabla[["descriptiva"]]) != 0) {
            DT::datatable(
              episodios$tabla[["descriptiva"]][, c(
                episodios$agrupador, episodios$separadores), with = FALSE],
              options = list(
                language = list(
                  url = dt_spanish),
                pageLength = 20,
                dom = 'ftp',
                autoWidth = FALSE,
                ordering= TRUE,
                scrollX = TRUE,
                scrollY = "370px"),
              rownames = FALSE) %>%
              formatStyle(TRUE, fontSize = '95%', backgroundColor = 'white')
          }
      })

      # Histograma
      output$histograma_agrupador <- DT::renderDataTable({
        if (nrow(episodios$tabla[["descriptiva"]]) != 0) {
          DT::datatable(
            episodios$tabla[["descriptiva"]][, c(
              episodios$agrupador, episodios$separadores), with = FALSE],
            options = list(
              language = list(
                url = dt_spanish),
              pageLength = 20,
              dom = 'ftp',
              autoWidth = FALSE,
              ordering = TRUE,
              scrollX = TRUE,
              scrollY = "370px"),
            rownames = FALSE) %>%
            formatStyle(TRUE, fontSize = '95%', backgroundColor = 'white')
        }
      })

      # Caja de bigotes
      output$bigotes_agrupador <-
        DT::renderDataTable({
          if (nrow(episodios$tabla[["descriptiva"]]) != 0) {
            DT::datatable(
              episodios$tabla[["descriptiva"]][, c(
                episodios$agrupador, episodios$separadores), with = FALSE],
              options = list(
                language = list(
                  url = dt_spanish),
                pageLength = 10000,
                dom = 'ftp',
                autoWidth = FALSE,
                ordering = TRUE,
                scrollX = TRUE,
                scrollY = "370px"),
              rownames = FALSE) %>%
              formatStyle(TRUE, fontSize = '95%', backgroundColor = 'white')
          }
        })

      output$histograma_titulo <- renderText({
        "Histograma de valores"
      })

      output$caja_de_bigotes_titulo <- renderText({
        "Distribución de valores"
      })

      # Generar histograma
      observeEvent(input$histograma_ejecutar, {
        if (!is.null(input$histograma_agrupador_rows_selected)) {
          tryCatch(
            expr = {
              # Selección del número de bins
              if (input$histograma_numero_columnas == "Auto") {
                numero_bins <- 20
              } else {
                numero_bins <- input$histograma_numero_columnas
              }
              # Se genera el histograma
              episodios$histograma_plot <- histograma_agrupador(
                titulo = "Histograma",
                # Se utiliza episodios$tabla[["data"]] para no utilizar collect
                data = episodios$tabla[["data"]],
                columnas_sep = episodios$lista_agrupadores[
                  input$histograma_agrupador_rows_selected],
                numero_bins = numero_bins
              )
            },
            error = function(e) {
              sendSweetAlert(
                session = session,
                title = "Error",
                type = "error",
                text = "Error generando histograma. Es posible que la frecuencia sea demasiado baja."
              )
            }
          )
        }
      })

      output$histograma_render <- renderPlotly({
        episodios$histograma_plot
      })

      # Se genera el gráfico de tabla de bigotes
      observeEvent(input$caja_de_bigotes_ejecutar, {
        if (!is.null(input$bigotes_agrupador_rows_selected)) {
          episodios$caja_de_bigotes_plot <- caja_de_bigotes_agrupador(
            data = episodios$tabla[["descriptiva"]],
            columnas_sep = episodios$lista_agrupadores[
              input$bigotes_agrupador_rows_selected]
          )
        }
      })

      output$caja_de_bigotes_render <- renderPlotly({
        episodios$caja_de_bigotes_plot
      })

      # se genera grafico de barras
      observeEvent(input$grafico_barras_ejecutar, {
        if (!is.null(input$barras_agrupador_rows_selected)) {
          episodios$grafico_barras_plot <- grafico_barras_descriptiva(
            data = episodios$tabla[["descriptiva"]],
            columna_numeros = input$grafico_barras_indicador,
            columnas_sep = episodios$lista_agrupadores[
              input$barras_agrupador_rows_selected
            ]
          )
        }
      })

      output$grafico_barras_render <- renderPlotly({
        episodios$grafico_barras_plot
      })

      # Se suma el valor total en la descriptiva
      output$descriptiva_sumas_valor <- renderText({
        if (!is.null(opciones$colnames)) {
          if (nrow(episodios$tabla[["descriptiva"]]) > 0) {
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

      # Botones de descarga
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
