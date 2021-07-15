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

seguimiento_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        width = 4,
        tabsetPanel(
          tabPanel(
            title = "Generales",
            tags$br(),
            selectizeInput(
              inputId =  ns("nota_tecnica"),
              label = "Comparar con nota técnica:",
              choices = NULL,
              multiple = FALSE),
            agrupadores_widget(
              id = id,
              separadores = FALSE,
              checkboxGroupInput(
                inputId = ns("tablas"),
                label = "Seguimiento:",
                choices = c(
                  "Frecuencias" = "frecuencias",
                  "Valor facturado" = "valor"),
                selected = "descriptiva",
                inline = TRUE,
                width = "100%")),
            tags$br(),
            actionButton(ns("exe"), "Generar"),
            tags$br()
          ),
          tabPanel(
            title = "Descargas",
            tags$br(),
            downloadButton(
              outputId = ns("descargar_informe"),
              label = "Informe completo",
              width = "100%"
            )
          )
        )
      ),
      box(
        width = 8,
        tabsetPanel(
          tabPanel(
            title = "Resultados frecuencias",
            uiOutput(outputId = ns("resultados_frec"))
          ),
          tabPanel(
            title = "Resultados de valor facturado",
            uiOutput(outputId = ns("resultados_valor"))
          )
        )
      )
    )
  )
}

seguimiento_server <- function(id, opciones, cache) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- NS(id)
      episodios <- reactiveValues(
        nt_current = data.frame(),
        frecuencias = data.frame(),
        nt_selected = "Ninguno",
        descriptiva = data.frame())

      observe({
        updateSelectizeInput(
          inputId = "nota_tecnica",
          choices = c("Ninguno", opciones$notas_tecnicas$nt),
          selected = episodios$nt_selected
          )
      }) %>%
      bindEvent(opciones$notas_tecnicas)

      observe({
        episodios$nt_selected <- input$nota_tecnica
      })

      # Se observa que el usuario haga click en los titulos de las unidades
      # de conteo en el widget de jerarquia.
      # De esta manera se pueden mover los diferentes agrupadores de manera
      # sencilla entre unidades.

      episodios_jerarquia_server(
        id = id,
        episodios = episodios,
        cache = cache,
        opciones = opciones)

      # Generar descriptiva
      observeEvent(input$exe, {
        withProgress(message = "Calculando descriptiva", {
          if (opciones$datos_cargados &&
            input$agrupador %notin% c("", "Ninguno") &&
            "valor" %in% input$tablas) {
            agrupador <- input$agrupador
            if (input$episodios) episodios_col_rel <- input$episodios_col_rel
            separadores <- c("ais_mes", "ais_anio")
            episodios$agrupador <- agrupador
            episodios$separadores <- separadores
            # Si se va a generar por episodios
            if (input$episodios) {
              # Se genera un ID para el cache y se busca si ya ha sido
              # generado en el pasado
              episodios$descriptiva <- cache_call(
                fn = episodios_jerarquia,
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
                non_cache_params = list(
                  data = opciones$tabla %>% 
                    mutate(
                      ais_mes = month(fecha_prestacion),
                      ais_anio = year(fecha_prestacion),
                      ais_mes_anio = ais_anio * 100 + ais_mes)),
                prefix = "desc_ep_seg",
                cache_depends = opciones$tabla_query
              )[["descriptiva"]]
            }
            if (!input$episodios) {
              # Si se va a generar de manera tradicional
              # Se checkea un ID para el cache y se busca si ha sido
              # generada en el pasado
              episodios$descriptiva <- cache_call(
                fn = descriptiva,
                cache = cache,
                cache_params = list(
                  columnas = c(agrupador, separadores),
                  columna_valor = opciones$valor_costo,
                  columna_suma = input$unidades,
                  prestaciones = (input$unidades == "prestacion"),
                  frec_cantidad = opciones$cantidad),
                non_cache_params = list(
                  data = opciones$tabla %>% 
                    mutate(
                      ais_mes = month(fecha_prestacion),
                      ais_anio = year(fecha_prestacion),
                      ais_mes_anio = ais_anio * 100 + ais_mes)),
                prefix = "desc_seg",
                cache_depends = opciones$tabla_query
              )[["descriptiva"]]
            }
          }
        })
      })

      # Generar frecuencias
      observeEvent(input$exe, {
        withProgress(message = "Calculando frecuencias", {
          if (opciones$datos_cargados &&
            input$agrupador %notin% c("", "Ninguno") &&
            "frecuencias" %in% input$tablas) {
            agrupador <- input$agrupador
            if (input$episodios) episodios_col_rel <- input$episodios_col_rel
            separadores <- NULL
            episodios$agrupador <- agrupador
            episodios$separadores <- separadores
            # Validacion por episodio o tradicional
            if (input$episodios) {
              episodios$frecuencias <- cache_call(
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
                non_cache_params = list(data = opciones$tabla),
                prefix = "frec_ep",
                cache_depends = opciones$tabla_query
              )[["descriptiva"]]
            }
            if (!input$episodios) {
              episodios$frecuencias <- cache_call(
                fn = frecuencias,
                cache = cache,
                cache_params = list(
                  agrupador = c(agrupador, separadores),
                  columna_fecha = "fecha_prestacion",
                  columna_suma = input$unidades,
                  prestaciones = (input$unidades == "prestacion"),
                  frec_cantidad = opciones$cantidad,
                  intervalo = "mes"),
                non_cache_params = list(data = opciones$tabla),
                prefix = "frec",
                cache_depends = opciones$tabla_query
              )
            }
          }
        })
      })

      observe({
        if (input$nota_tecnica %notin% c("", "Ninguno")) {
          nt <- opciones$notas_tecnicas %>%
            filter(nt == input$nota_tecnica) %>%
            rename(cod_nt = nt)
          episodios$nt_current <- nt
        } else {
          episodios$nt_current <- data.frame()
        }
      }) %>%
        bindEvent(input$exe)

      # Se espera a que se generen las descripciones por frecuencia o valor
      # para empezar a hacer la comparación con la nota técnica
      observe({
        tryCatch(
          expr = {
            if (nrow(episodios$nt_current) > 0) {
              if (nrow(episodios$frecuencias) > 0) {
                episodios$comparar_frecs <- comparacion_frecuencias(
                  frecuencias_tabla = episodios$frecuencias,
                  nota_tecnica = episodios$nt_current,
                  agrupador = episodios$agrupador
                )
              }
              if (nrow(episodios$descriptiva) > 0) {
                episodios$comparar_valor <- comparacion_valor_facturado(
                  descriptiva_tabla = episodios$descriptiva,
                  nota_tecnica = episodios$nt_current,
                  agrupador = episodios$agrupador
                )
              }
            } else {
              showNotification("Seleccionar una nota técnica")
            }
          },
          error = function(e) {
            purrr::map(e, message)
            showNotification(
              "Error: validar que la nota técnica corresponda a los datos"
            )
          }
        )
      }) %>%
      bindEvent(
        episodios$frecuencias,
        episodios$descriptiva,
        episodios$nt_current)

      observe({
        if (input$nota_tecnica %notin% c("", "Ninguno")) {
          perfil_nt <-
            opciones$notas_tecnicas_lista[[input$nota_tecnica]][["perfil"]]
          if (is.null(perfil_nt)) perfil_nt <- "Ninguno"
          if (input$episodios &&
              perfil_nt %in% names(opciones$perfil_lista)) {
            if (!opciones$perfil_enable) {
              confirmSweetAlert(
                session = session,
                inputId = ns("cambiar_perfil"),
                title = "Utilizar perfil",
                text = "Esta nota técnica tiene un perfil asignado.
                        ¿Desea utilizarlo?",
                btn_labels = c("Cancelar", "Utilizar")
              )
            } else if (opciones$perfil_enable &&
                       opciones$perfil_selected != perfil_nt) {
              confirmSweetAlert(
                session = session,
                inputId = ns("cambiar_perfil"),
                title = "Utilizar perfil",
                text = "Esta nota técnica tiene un perfil asignado diferente al
                        seleccionado.
                        ¿Desea cambiarlo?",
                btn_labels = c("Cancelar", "Carmbiar")
              )
            }
          }
        }
      }) %>%
      bindEvent(input$episodios, input$nota_tecnica)

      observe({
        if (input$cambiar_perfil) {
          opciones$perfil_selected <- NULL
          opciones$perfil_selected <-
            opciones$notas_tecnicas_lista[[input$nota_tecnica]][["perfil"]]
        }
      }) %>%
      bindEvent(input$cambiar_perfil)

      # Se generan los UI elements y outputs de la sección de frecuencias
      observe({
        episodios$frecuencias_tab <- tagList(
          fluidRow(
            column(width = 4, uiOutput(ns("frecuencias_resumen"))),
            column(
              width = 8,
              plotlyOutput(ns("frecuencias_plot"), height = "450px") %>%
                withSpinner())
          ),
          tags$hr(),
          tags$br(),
          tags$h4("Ejecución de frecuencias:"),
          DT::dataTableOutput(ns("frec_ejecucion_base")) %>% withSpinner(),
          tags$hr(),
          tags$br(),
          tags$h4("Ejecución por costo medio:"),
          DT::dataTableOutput(ns("frec_ejecucion_por_cm")) %>%
            withSpinner(),
          tags$hr(),
          tags$br(),
          tags$h4("Diferencias de frecuencia con costos medios:"),
          DT::dataTableOutput(ns("frec_diferencias_por_cm")) %>%
            withSpinner(),
          tags$hr()
        )
        episodios$valor_tab <- tagList(
          fluidRow(
            column(width = 4, uiOutput(ns("valor_fac_resumen"))),
            column(
              width = 8,
              plotlyOutput(ns("valor_fac_plot"), height = "450px") %>%
                withSpinner())
          ),
          tags$hr(),
          tags$br(),
          tags$h4("Ejecución:"),
          DT::dataTableOutput(ns("valor_fac_total")) %>% withSpinner(),
          tags$hr(),
          tags$br(),
          tags$h4("Diferencias de valor:"),
          DT::dataTableOutput(ns("diferencias_valor_fac")) %>%
            withSpinner(),
          tags$hr(),
          tags$br(),
          tags$h4("Diferencias de valor en porcentaje:"),
          DT::dataTableOutput(ns("diferencias_valor_fac_perc")) %>%
            withSpinner(),
          tags$hr()
        )
        if ("frecuencias" %notin% input$tablas) {
          episodios$frecuencias_tab <- NULL
        }
        if ("valor" %notin% input$tablas) {
          episodios$valor_tab <- NULL
        }
      })

      # En la siguiente sección simplemente se encuentras los outputs
      # de las diferentes tablas y UI elements necesarios

      output$resultados_frec <- renderUI({
        episodios$frecuencias_tab
      }) %>%
      bindEvent(input$exe)

      output$resultados_valor <- renderUI({
        episodios$valor_tab
      }) %>%
      bindEvent(input$exe)

      output$frecuencias_resumen <- renderUI({
        episodios$comparar_frecs$ui$totales})

      output$frecuencias_plot <- renderPlotly({
        episodios$comparar_frecs$ui$plot_valor_acumulado
      })

      output$frec_ejecucion_base <-
        DT::renderDataTable({episodios$comparar_frecs$ui$ejecucion_base})

      output$frec_ejecucion_por_cm <- DT::renderDataTable({
        episodios$comparar_frecs$ui$ejecucion_base_por_cm
      })

      output$frec_diferencias_por_cm <-
        DT::renderDataTable({episodios$comparar_frecs$ui$diferencias_por_cm})

      output$valor_fac_resumen <- renderUI({
        episodios$comparar_valor$ui$totales
      })

      output$valor_fac_plot <- renderPlotly({
        episodios$comparar_valor$ui$plot_valor_acumulado
      })

      output$valor_fac_total <- DT::renderDataTable({
        episodios$comparar_valor$ui$comparacion_suma
      })

      output$diferencias_valor_fac <- DT::renderDataTable({
        episodios$comparar_valor$ui$comparacion_diff
      })

      output$diferencias_valor_fac_perc <- DT::renderDataTable({
        episodios$comparar_valor$ui$comparacion_porcentaje
      })

      # Output para descargar el informe
      # En este output se ve si existe un informe por valor facturado
      # y por frecuencias e incluye las respectivas tablas en la descarga
      output$descargar_informe <- downloadHandler(
        filename = paste0("Seguimiento de: ", episodios$nt_selected, ".xlsx"),
        content = function(file) {
          writexl::write_xlsx(
            x = list("Nota técnica" = episodios$nt_current) %>%
              # Se checkea si existen registros de frecuecnias
              {if (nrow(episodios$frecuencias) > 0) {
                append(., episodios$comparar_frecs$data)
              } else {.}} %>%
              # Se checkea si hay registros de valor facturado
              {if (nrow(episodios$descriptiva) > 0) {
                append(., episodios$comparar_valor$data)
              } else {.}},
            path = file
          )
        },
        contentType = "xlsx"
      )

     }
  )
}
