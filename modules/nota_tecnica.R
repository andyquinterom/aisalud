#
# Analítica Integrada Salud
#
# Derechos de autor 2021 por MD&CO Consulting Group (NIT 901.119.781-5)
# Copyright (C) 2021 by MD&CO Consulting Group
#
# Este programa es software libre: puede redistribuirlo o modificarlo bajo
# los términos de la licencia Affero General Public License tal cual
# publicada por la Free Software Foundation, sea la versión 3 de la licencia
# o cualquier versión posterior. Este programa se distribuye SIN GARANTÍA
# EXPERSA O IMPLÍCITA, INCLUIDAS LAS DE NO INFRACCIÓN, COMERCIABILIDAD O
# APTITUD PARA UN PROPÓSITO PARTICULAR. Referir a la
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) para más detalles.
#

nota_tecnica_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        width = 4,
        agrupadores_widget(
          id = id,
          separadores = FALSE
        ),
        actionButton(
          inputId = ns("exe"),
          label = "Generar escenarios",
          width = "100%"
        ),
        tags$br(),
        tags$br(),
        numericInput(
          inputId = ns("poblacion"),
          label = "Población:",
          min = 1,
          value = 40000
        ),
        tags$br(),
        downloadButton(
          outputId = ns("nota_tecnica_descargar_xlsx"),
          label = "Excel",
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
            outputId = ns("nota_tecnica_suma_pc"),
            width = 6),
          tabsetPanel(
            tabPanel(
              title = "Tabla",
              DT::dataTableOutput(ns("nota_tecnica_junta"))
            ),
            tabPanel(
              title = "JSON",
              shinyAce::aceEditor(
                ns("nota_tecnica_json")
              )
            )
          )
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
          sliderInput(
            inputId = ns("seguimiento_fechas"),
            label = NULL,
            min = ymd("2020-01-01"),
            max = ymd("2020-01-01"),
            value = ymd(rep("2020-01-01", 2)),
            timeFormat = "%Y-%m"
          ),
          plotlyOutput(
            outputId = ns("seguimiento_plot"),
            width = "100%",
            height = "440px"
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
              title = "Frecuencias",
              sliderInput(
                inputId = ns("frecuencias_ajuste"),
                label = "Frecuencia",
                min = 1,
                max = 1,
                value = 1
              ),
              plotlyOutput(
                outputId = ns("frecuencias_plot"),
                height = "370px",
                width = "100%"
              )
            )
          )
        )
      )
    )
  )
}

nota_tecnica_server <- function(id, opciones, cache) {
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
        parsed = data.table(),
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
                episodios$descriptiva <- 
                  cache_call(
                    fn = function(data){
                      episodios_jerarquia(
                        data = mutate(
                          opciones$tabla,
                          ais_mes = month(fecha_prestacion),
                          ais_anio = year(fecha_prestacion),
                          ais_mes_anio = ais_anio * 100 + ais_mes)
                      )
                    },
                    cache = cache,
                    cache_params = list(
                      columnas =      agrupador,
                      columna_valor = opciones$valor_costo,
                      columna_sep =   separadores,
                      columna_suma =  episodios_col_rel,
                      nivel_1 = input$episodios_jerarquia_nivel_1_order$text,
                      nivel_2 = input$episodios_jerarquia_nivel_2_order$text,
                      nivel_3 = input$episodios_jerarquia_nivel_3_order$text,
                      nivel_4 = input$episodios_jerarquia_nivel_4_order$text,
                      frec_cantidad = opciones$cantidad),
                    non_cache_params = list(data=opciones$tabla),
                    prefix = "desc_ep_seg",
                    cache_depends = opciones$tabla_query
                  )[["descriptiva"]]
              }
              if (!input$episodios) {
                # Si se va a generar de manera tradicional
                # Se checkea un ID para el cache y se busca si ha sido
                # generada en el pasado
                episodios$descriptiva <- 
                  cache_call(
                    fn = function(data){
                      descriptiva(
                        data = mutate(
                          opciones$tabla,
                          ais_mes = month(fecha_prestacion),
                          ais_anio = year(fecha_prestacion),
                          ais_mes_anio = ais_anio * 100 + ais_mes)
                      )
                  },
                    cache = cache,
                    cache_params = list(
                      columnas = c(agrupador, separadores),
                      columna_valor = opciones$valor_costo,
                      columna_suma = input$unidades,
                      prestaciones = (input$unidades == "prestacion"),
                      frec_cantidad = opciones$cantidad),
                    non_cache_params = list(opciones$tabla),
                    prefix = "desc_seg",
                    cache_depends = opciones$tabla_query
                  )[["descriptiva"]]
              }
            episodios$descriptiva <- episodios$descriptiva %>%
              mutate(mes_anio_num = ais_anio * 100 + ais_mes)
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
              episodios$frecuencias <- 
                cache_call(
                  fn = frecuencias_jerarquia,
                  cache = cache,
                  cache_params = list(
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
                  non_cache_params = opciones$tabla,
                  prefix = "frec_ep",
                  cache_depends = opciones$tabla_query
                )[["descriptiva"]]
            }
            if (!input$episodios) {
              episodios$frecuencias <- 
                cache_call(
                  fn = frecuencias,cache = cache,
                  cache_params = list(
                    agrupador = c(agrupador, separadores),
                    columna_fecha = "fecha_prestacion",
                    columna_suma = input$unidades,
                    prestaciones = (input$unidades == "prestacion"),
                    frec_cantidad = opciones$cantidad,
                    intervalo = "mes"),
                  non_cache_params = opciones$tabla,
                  prefix = "frec",
                  cache_depends = opciones$tabla_query
                )
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
              # Un truquito para sacar el dia final de los meses
              mes_anio_date = do.call(purrr::map(
                .x = as.Date(paste(ais_anio, ais_mes, "01", sep = "-")),
                .f = function(x) last(seq(x, length = 2, by = "months") - 1)),
                what = "c")) %>%
            arrange(mes_anio_date)
          nota_tecnica$agrupadores <- nota_tecnica$timeseries %>%
            pull(!!rlang::sym(episodios$agrupador)) %>%
            unique()
          mes_anio_date_list <- c(
            min(nota_tecnica$timeseries$mes_anio_date),
            max(nota_tecnica$timeseries$mes_anio_date)
          )
          updateSliderInput(
            inputId = "seguimiento_fechas",
            session = session,
            min = mes_anio_date_list[1],
            max = mes_anio_date_list[2],
            value = mes_anio_date_list,
            timeFormat = "%Y-%m"
          )
          updateSelectizeInput(
            inputId = "conf_agrupador",
            choices = nota_tecnica$agrupadores
          )
          nota_tecnica$nota_tecnica <- esquema_nota_tecnica(
            timeseries = nota_tecnica$timeseries,
            agrupador = episodios$agrupador,
            perfil = opciones$perfil_selected,
            poblacion = input$poblacion
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

      observe({
        if (!is.null(nota_tecnica$nota_tecnica$nota_tecnica$poblacion)) {
          nota_tecnica$nota_tecnica$nota_tecnica$poblacion <-
            input$poblacion
        }
      })

      observe({
        if (!is.null(nota_tecnica$nota_tecnica$nota_tecnica$poblacion)) {
          updateAceEditor(
            session = session,
            "nota_tecnica_json",
            value = toJSON(
              x = purrr::map(nota_tecnica$nota_tecnica, debloat_nt),
              pretty = TRUE,
              auto_unbox=TRUE),
            mode = "json"
          )
        }
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
        bindEvent(
          input$frecuencias_ajuste_int,
          input$conf_agrupador,
          nota_tecnica$timeseries)

      # Cuando se hagan cambios a los costos medios o frecuencias de los
      # agrupadores se actualiza la nota técnica
      observe({
        nota_tecnica$nota_tecnica$nota_tecnica$agrupadores <-
          nota_tecnica$agrupadores_temp
      }) %>%
        bindEvent(nota_tecnica$agrupadores_temp)


      observe({
        nota_tecnica$seguimiento_fechas <- year(input$seguimiento_fechas) *
          100 + month(input$seguimiento_fechas)
      })

      # Se renderiza plot de la comparación con valor facturado
      output$seguimiento_plot <- renderPlotly({
        tryCatch(
          expr = {
            # Se hacen las mismas comparaciones de seguimiento entre los datos
            # del usuario y la nota técnica que se esta desarrollando.
            if (!input$seguimiento_plot_frec) {
              if (nrow(episodios$descriptiva) > 0) {
                comparacion_valor_facturado(
                  descriptiva_tabla = episodios$descriptiva %>%
                    filter(mes_anio_num >= nota_tecnica$seguimiento_fechas[1] &
                      mes_anio_num <= nota_tecnica$seguimiento_fechas[2]),
                  nota_tecnica = nota_tecnica$parsed,
                  agrupador = episodios$agrupador
                )[["ui"]][["plot_valor_acumulado"]]
              }
            } else {
              if (nrow(nota_tecnica$timeseries) > 0) {
                comparacion_valor_facturado(
                  descriptiva_tabla = nota_tecnica$timeseries %>%
                    filter(mes_anio_num >= nota_tecnica$seguimiento_fechas[1] &
                      mes_anio_num <= nota_tecnica$seguimiento_fechas[2]) %>%
                    frec_x_cm(
                      nota_tecnica = nota_tecnica$parsed,
                      agrupador = episodios$agrupador),
                  nota_tecnica = nota_tecnica$parsed,
                  agrupador = episodios$agrupador
                )[["ui"]][["plot_valor_acumulado"]]
              }
            }
          },
          error = function(e) {}
        )
      }) %>%
        bindEvent(
          nota_tecnica$nota_tecnica,
          input$seguimiento_plot_frec,
          nota_tecnica$seguimiento_fechas
        )

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
            name = "Ajuste analista",
            mode = "lines",
            line = list(color = "rgb(205, 12, 24)", dash = "dash")
          ) %>%
          layout(
            xaxis = list(title = "Fecha"),
            yaxis = list(title = "Costo medio",
                         tickformat = ",.2f")
          ) %>%
          config(locale = "es")

      }) %>%
        bindEvent(nota_tecnica$timeseries_selected)

      # Plot que muestra las fechas a través del tiempo
      output$frecuencias_plot <- renderPlotly({
        if (!is.null(nota_tecnica$agrupadores_temp)) {
          n_min <- nota_tecnica$agrupadores_temp[[
            input$conf_agrupador]][["n_min"]]
          n_max <- nota_tecnica$agrupadores_temp[[
            input$conf_agrupador]][["n_max"]]
          n_selected <- nota_tecnica$agrupadores_temp[[
            input$conf_agrupador]][["n"]]
          updateSliderInput(
            inputId = "frecuencias_ajuste",
            value = n_selected,
            step = 0.1,
            min = n_min * 0.8,
            max = n_max * 1.2
          )
          nota_tecnica$timeseries_selected %>%
            ungroup() %>%
            plot_ly(
              x = ~mes_anio_date,
              y = ~Frecuencia,
              name = "Costos medios",
              type = "scatter",
              mode = "lines+markers") %>%
            add_trace(
              y = mean(nota_tecnica$timeseries_selected$Frecuencia),
              name = "Frecuencia media",
              mode = "lines") %>%
            add_trace(
              y = n_selected,
              name = "Ajuste analista",
              mode = "lines",
              line = list(color = "rgb(205, 12, 24)", dash = "dash")
            ) %>%
            layout(
              xaxis = list(title = "Fecha"),
              yaxis = list(title = "Frecuencia",
                           tickformat = ",.2f")
            ) %>%
            config(locale = "es")
        }
      }) %>%
        bindEvent(
          nota_tecnica$timeseries_selected
        )

      # Se observa que el usuario o la maquina cambie el sliderInput
      # y reacciona cambiando las variables en la nota técnica y utilizando
      # el proxy de plotly para actualizar el gráfico.
      observe({
        frecuencias_ajuste <- round(input$frecuencias_ajuste, digits = 3)
        nota_tecnica$agrupadores_temp[[
            input$conf_agrupador]][["n"]] <- frecuencias_ajuste
        n_value <- frecuencias_ajuste %>%
          rep(length(nota_tecnica$timeseries_selected$mes_anio_date))
        plotlyProxy("frecuencias_plot", session) %>%
          # Quite el trace numero 4
          plotlyProxyInvoke("deleteTraces", list(as.integer(2))) %>%
          plotlyProxyInvoke("addTraces", list(list(
            y = n_value,
            x = nota_tecnica$timeseries_selected$mes_anio_date,
            mode = "lines",
            type = "scatter",
            line = list(color = "rgb(205, 12, 24)", dash = "dash"),
            name = "Ajuste analista")))
      }) %>%
        bindEvent(input$frecuencias_ajuste)

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
          round(digits = 0) %>%
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
            name = "Ajuste analista")))
      }) %>%
        bindEvent(input$costo_medio_ajuste)

      # Se muestra la nota técnica
      output$nota_tecnica_junta <- DT::renderDataTable({
        if (nrow(nota_tecnica$parsed) > 0) {
          datatable(
            data = nota_tecnica$parsed %>%
              select(-nt),
            rownames = FALSE,
            colnames = c(
              "Valor a mes" = "valor_mes",
              "Costo medio" = "cm",
              "Frecuencia per capita" = "frecuencia_pc",
              "Agrupador" = "agrupador",
              "Frecuencia a mes" = "frec_mes"
            ),
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
              c("Costo medio", "Valor a mes"),
              dec.mark = ",",
              mark = ".",
              currency = "$",
              digits = 0
            ) %>%
            DT::formatRound(
              c("Frecuencia per capita"),
              dec.mark = ",",
              mark = ".",
              digits = 6
            ) %>%
            DT::formatRound(
              c("Frecuencia a mes"),
              dec.mark = ",",
              mark = ".",
              digits = 2
            )
        }
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

      output$nota_tecnica_suma_pc <- renderValueBox({
        if (opciones$datos_cargados) {
          valueBox(
            subtitle = "Valor a mes per capita.",
            value = {
              if (nrow(nota_tecnica$parsed) >= 1) {
                formatAsCurrency(
                  sum(nota_tecnica$parsed$valor_mes / input$poblacion,
                  na.rm = TRUE)
                )
              } else {
                0
              }
            },
            color = "orange",
            icon = icon("dollar-sign", "font-awesome")
          )
        } else {
          valueBox(
            subtitle = "Valor total a mes.",
            value = 0,
            color = "orange",
            icon = icon("dollar-sign", "font-awesome")
          )
        }
      })

      output$nota_tecnica_descargar_xlsx <- downloadHandler(
        filename = "nota_tecnica.xlsx",
        content = function(file) {
          write_xlsx(nota_tecnica$parsed, file)
        },
        contentType = "xlsx"
      )

    }
  )
}
