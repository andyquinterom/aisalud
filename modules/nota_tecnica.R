nota_tecnica_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        width = 4,
        agrupadores_widget(
          id = id,
          separadores = FALSE,
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
          )
        ),
        actionButton(
          inputId = ns("exe"),
          label = "Generar escenarios",
          width = "100%"
        ),
        tags$br(),
        tags$br(),
        downloadButton(
          outputId = ns("nota_tecnica_descargar_xlsx"),
          label = "Excel",
          style = "width:100%;"),
        tags$br(),
        tags$br(),
        downloadButton(
          outputId = ns("nota_tecnica_descargar_csv"),
          label = "CSV",
          style = "width:100%;")
      ),
      box(
        width = 8,
        tags$div(
          style = "height: 600px;",
          valueBoxOutput(
            outputId = ns("nota_tecnica_suma"),
            width = 6),
          valueBoxOutput(
            outputId = ns("nota_tecnica_porcentaje"),
            width = 6),
          DT::dataTableOutput(ns("nota_tecnica_junta"))
        )
      )
    ),
    fluidRow(
      box(
        width = 4,
        tags$div(
          style = "height: 590px;",
          checkboxInput(
            inputId = ns("seguimiento_plot_frec"),
            label = "Gráfico de frecuencias",
            value = FALSE
          ),
          plotlyOutput(
            outputId = ns("seguimiento_plot"),
            width = "100%",
            height = "540px"
          )
        )
      ),
      box(
        width = 8,
        tags$div(
          style = "height: 600px",
          selectizeInput(
            inputId = ns("conf_agrupador"),
            label = "Agrupador",
            choices = NULL
          ),
          tabsetPanel(
            tabPanel(
              title = "Costos medios",
              sliderInput(
                inputId = ns("costo_medio_ajuste"),
                label = "Percentil del costo medio:",
                min = 0,
                max = 100,
                value = 50
              ),
              plotlyOutput(
                outputId = ns("costo_medio_plot"),
                height = "370px",
                width = "100%"
              )
            ),
            tabPanel(
              title = "Frecuencias"
            )
          )
        )
      )
    )
  )
}

nota_tecnica_server <- function(id, opciones) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- NS(id)

      episodios <- reactiveValues(
        descriptiva = data.table(),
        frecuencias = data.table(),
        tabla = list(descriptiva = data.table(), data = data.table()),
        agrupadores_items = NULL)

      episodios_jerarquia_server(
        episodios = episodios,
        opciones = opciones,
        id = id,
        separadores = FALSE)

      nota_tecnica <- reactiveValues(
        tabla = data.table(),
        tabla_junta = data.table(),
        nota_tecnica = data.table(),
        timeseries = data.table(),
        escenarios = list(
          "episodio" = list(),
          "factura" = list(),
          "paciente" = list(),
          "prestacion" = list()
        ))

      observe({
        if (opciones$datos_cargados) {
          numero_meses <-  round(interval(
            opciones$fecha_rango[1],
            opciones$fecha_rango[2]) / months(1),
            digits = 1)
          updateNumericInput(
            session = session,
            inputId = "nota_tecnica_meses",
            value = numero_meses
          )
        }
      })

      # Generar descriptiva
      observeEvent(input$exe, {
        withProgress(message = "Calculando descriptiva", {
          if (opciones$datos_cargados &&
            input$agrupador %notin% c("", "Ninguno")) {
            agrupador <- input$agrupador
            if (input$episodios) episodios_col_rel <- input$episodios_col_rel
            separadores <- c("ais_mes", "ais_anio")
            episodios$agrupador <- agrupador
            episodios$separadores <- separadores
              # Si se va a generar por episodios
              if (input$episodios) {
                # Se genera un ID para el cache y se busca si ya ha sido
                # generado en el pasado
                cache_id <- digest(
                  object = list(
                    "desc_ep_seg", opciones$tabla_query,
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
                if (check_cache) {
                  episodios$descriptiva <- opciones$cache[[cache_id]]
                }
                if (!check_cache) {
                  episodios$descriptiva <- episodios_jerarquia(
                    data = mutate(
                      opciones$tabla,
                      ais_mes = month(fecha_prestacion),
                      ais_anio = year(fecha_prestacion),
                      ais_mes_anio = ais_anio * 100 + ais_mes),
                    columnas =      agrupador,
                    columna_valor = opciones$valor_costo,
                    columna_sep =   separadores,
                    columna_suma =  episodios_col_rel,
                    nivel_1 = input$episodios_jerarquia_nivel_1_order$text,
                    nivel_2 = input$episodios_jerarquia_nivel_2_order$text,
                    nivel_3 = input$episodios_jerarquia_nivel_3_order$text,
                    nivel_4 = input$episodios_jerarquia_nivel_4_order$text,
                    frec_cantidad = opciones$cantidad)[["descriptiva"]]
                  opciones$cache[[cache_id]] <- episodios$descriptiva
                }
              }
              if (!input$episodios) {
                # Si se va a generar de manera tradicional
                # Se checkea un ID para el cache y se busca si ha sido
                # generada en el pasado
                cache_id <- digest(
                  object = list(
                    "desc_seg", opciones$tabla_query,
                    columnas = c(agrupador, separadores),
                    columna_valor = opciones$valor_costo,
                    columna_suma = input$unidades,
                    prestaciones = (input$unidades == "prestacion"),
                    frec_cantidad = opciones$cantidad),
                  algo = "xxhash32",
                  seed = 1)
                check_cache <- cache_id %in% names(opciones$cache)
                if (check_cache) episodios$descriptiva <-
                  opciones$cache[[cache_id]]
                if (!check_cache) {
                  episodios$descriptiva <- descriptiva(
                    data = mutate(
                      opciones$tabla,
                      ais_mes = month(fecha_prestacion),
                      ais_anio = year(fecha_prestacion),
                      ais_mes_anio = ais_anio * 100 + ais_mes),
                    columnas = c(agrupador, separadores),
                    columna_valor = opciones$valor_costo,
                    columna_suma = input$unidades,
                    prestaciones = (input$unidades == "prestacion"),
                    frec_cantidad = opciones$cantidad)[["descriptiva"]]
                  opciones$cache[[cache_id]] <- episodios$descriptiva
              }
            }
          }
        })
      })

      # Generar frecuencias
      observeEvent(input$exe, {
        withProgress(message = "Calculando frecuencias", {
          if (opciones$datos_cargados &&
            input$agrupador %notin% c("", "Ninguno")) {
            agrupador <- input$agrupador
            if (input$episodios) episodios_col_rel <- input$episodios_col_rel
            separadores <- NULL
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
                  intervalo = "mes"),
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
                  intervalo = "mes")[["descriptiva"]]
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
                  intervalo = "mes"),
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
                  intervalo = "mes"
                )
                opciones$cache[[cache_id]] <- episodios$frecuencias
              }
            }
          }
        })
      })

      # Si la descriptiva fue generada de manera satisfactoria, se genera
      # una nota técnica con el estadandar básico de AIS
      observe({
        if (nrow(episodios$descriptiva) > 0) {
          nota_tecnica$timeseries <- episodios$descriptiva %>%
            descriptiva_timeseries(agrupador = episodios$agrupador) %>%
            mutate(
              mes_anio = mes_spanish_juntos(mes_anio_num),
              mes_anio_date = do.call(purrr::map(
                .x = as.Date(paste(ais_anio, ais_mes, "01", sep = "-")),
                .f = function(x) last(seq(x, length = 2, by = "months") - 1)),
                what = "c")) %>%
            arrange(mes_anio_date)
            nota_tecnica$agrupadores <- nota_tecnica$timeseries %>%
              pull(!!rlang::sym(episodios$agrupador)) %>%
              unique()
            updateSelectizeInput(
            inputId = "conf_agrupador",
            choices = nota_tecnica$agrupadores
          )
          nota_tecnica$nota_tecnica <- esquema_nota_tecnica(
            timeseries = nota_tecnica$timeseries,
            agrupador = episodios$agrupador,
            perfil = opciones$perfil_selected
          )
        }
      }) %>%
        bindEvent(episodios$descriptiva)

      # Cada vez que se haga un cambio a la nota técnica esta sera parsed a un
      # data.frame
      observe({
        nota_tecnica$parsed <- nota_tecnica$nota_tecnica %>%
          parse_nt()
      })

      # Se crea un subset de la serie de tiempo generada por los datos
      # para el agrupador seleccionado
      observe({
        conf_agrupador <- input$conf_agrupador
        if (nrow(nota_tecnica$timeseries) > 0 && !is.null(conf_agrupador)) {
          nota_tecnica$timeseries_selected <- nota_tecnica$timeseries %>%
            filter(!!rlang::sym(episodios$agrupador) == conf_agrupador)
          nota_tecnica$agrupadores_temp <-
            nota_tecnica$nota_tecnica$nota_tecnica$agrupadores
        }
      }) %>%
        bindEvent(input$conf_agrupador, nota_tecnica$timeseries)

      # Cuando se hagan cambios a los costos medios o frecuencias de los
      # agrupadores se actualiza la nota técnica
      observe({
        nota_tecnica$nota_tecnica$nota_tecnica$agrupadores <-
          nota_tecnica$agrupadores_temp
      }) %>%
        bindEvent(nota_tecnica$agrupadores_temp)

      # Se renderiza plot de la comparación con valor facturado
      output$seguimiento_plot <- renderPlotly({
        # Se hacen las mismas comparaciones de seguimiento entre los datos
        # del usuario y la nota técnica que se esta desarrollando.
        if (!input$seguimiento_plot_frec) {
          if (nrow(episodios$descriptiva) > 0) {
            comparacion_valor_facturado(
              descriptiva_tabla = episodios$descriptiva,
              nota_tecnica = nota_tecnica$parsed,
              agrupador = episodios$agrupador
            )[["ui"]][["plot_valor_acumulado"]]
          }
        } else {
          if (nrow(episodios$frecuencias) > 0) {
            comparacion_frecuencias(
              frecuencias_tabla = episodios$frecuencias,
              nota_tecnica = nota_tecnica$parsed,
              agrupador = episodios$agrupador
            )[["ui"]][["plot_valor_acumulado"]]
          }
        }
      }) %>%
        bindEvent(nota_tecnica$nota_tecnica, input$seguimiento_plot_frec)

      # Plot de los costos medios es generado
      output$costo_medio_plot <- renderPlotly({
        percentil_selected <- nota_tecnica$agrupadores_temp[[
          input$conf_agrupador]][["percentil"]]
        if (is.null(percentil_selected)) percentil_selected <- 0.5
        updateSliderInput(
          inputId = "costo_medio_ajuste",
          value = percentil_selected * 100
        )
        nota_tecnica$timeseries_selected %>%
          ungroup() %>%
          plot_ly(
            x = ~mes_anio_date,
            y = ~Media,
            name = "Costos medios",
            type = "scatter",
            mode = "lines+markers") %>%
          add_trace(
            y = quantile(nota_tecnica$timeseries_selected$Media, 0.5),
            name = "Mediana",
            mode = "lines"
          ) %>%
          add_trace(
            y = mean(nota_tecnica$timeseries_selected$Media),
            name = "Media",
            mode = "lines"
          ) %>%
          add_trace(
            y = quantile(nota_tecnica$timeseries_selected$Media, 0.75),
            name = "Percentil 75",
            mode = "lines"
          ) %>%
          add_trace(
            y = quantile(nota_tecnica$timeseries_selected$Media,
              percentil_selected),
            name = "Ajuste usuario",
            mode = "lines",
            line = list(color = "rgb(205, 12, 24)", dash = "dash")
          )
      }) %>%
        bindEvent(nota_tecnica$timeseries_selected)

      # Se observa que el usuario o la maquina cambie el sliderInput
      # y reacciona cambiando las variables en la nota técnica y utilizando
      # el proxy de plotly para actualizar el gráfico.
      observe({
        nota_tecnica$agrupadores_temp[[
            input$conf_agrupador]][["percentil"]] <-
              input$costo_medio_ajuste / 100
        quantile_value <- quantile(nota_tecnica$timeseries_selected$Media,
          input$costo_medio_ajuste / 100) %>%
          as.numeric() %>%
          rep(length(nota_tecnica$timeseries_selected$mes_anio_date))
        nota_tecnica$agrupadores_temp[[
            input$conf_agrupador]][["cm"]] <- quantile_value[1]
        plotlyProxy("costo_medio_plot", session) %>%
          # Quite el trace numero 4
          plotlyProxyInvoke("deleteTraces", list(as.integer(4))) %>%
          plotlyProxyInvoke("addTraces", list(list(
            y = quantile_value,
            x = nota_tecnica$timeseries_selected$mes_anio_date,
            mode = "lines",
            type = "scatter",
            line = list(color = "rgb(205, 12, 24)", dash = "dash"),
            name = "Ajuste usuario")))
      }) %>%
        bindEvent(input$costo_medio_ajuste)

      # Se muestra la nota técnica
      output$nota_tecnica_junta <- DT::renderDataTable({
        datatable(
          data = nota_tecnica$parsed %>%
            select(-nt),
          rownames = FALSE,
          extensions = c("FixedColumns"),
          options = list(
            ordering = T,
            scrollY = "370px",
            fixedColumns = list(leftColumnas = 1),
            scrollX = TRUE,
            scrollCollapse = TRUE,
            pageLength = 1000,
            dom = "ft"
          )
        ) %>%
          formatCurrency(
            c("cm", "valor_mes"),
            dec.mark = ",",
            mark = ".",
            currency = "$",
            digits = 0
          )
      }) %>%
        bindEvent(nota_tecnica$nota_tecnica)

      # Se suman los valores a mes de la nota técnica
      output$nota_tecnica_suma <- renderValueBox({
        if (opciones$datos_cargados) {
          valueBox(
            subtitle = "Valor total a mes.",
            value = {
              if (nrow(nota_tecnica$parsed) >= 1) {
                formatAsCurrency(
                  sum(nota_tecnica$parsed$valor_mes, na.rm = TRUE)
                )
              } else {
                0
              }
            },
            color = "green",
            icon = icon("dollar-sign", "font-awesome")
          )
        } else {
          valueBox(
            subtitle = "Valor total a mes.",
            value = 0,
            color = "green",
            icon = icon("dollar-sign", "font-awesome")
          )
        }
      })

      output$nota_tecnica_porcentaje <- renderValueBox({
        if (opciones$datos_cargados) {
          valueBox(
            subtitle = "Porcentaje del valor de los datos.",
            value = {
              if (nrow(nota_tecnica$tabla_junta) >= 1) {
                formatAsPerc(
                  0
                )
              } else {
                0
              }
            },
            color = "yellow",
            icon = icon("percent", "font-awesome")
          )
        } else {
          valueBox(
            subtitle = "Porcentaje del valor de los datos.",
            value = 0,
            color = "yellow",
            icon = icon("percent", "font-awesome")
          )
        }
      })

    }
  )
}
