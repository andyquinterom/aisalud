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

composicion_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        width = 3,
        selectizeInput(
          inputId = ns("episodios_col_rel"),
          label = "Relacionar episodios por:",
          choices = NULL,
          multiple = FALSE,
          width = "100%"),
        selectizeInput(
          inputId = ns("agrupador"),
          label = "Agrupador:",
          choices = NULL,
          multiple = FALSE,
          width = "100%"),
        selectizeInput(
          inputId = ns("episodios_jerarquia_nivel_1"),
          label = "Episodios:",
          choices = NULL,
          multiple = TRUE),
        selectizeInput(
          inputId = ns("composicion_explorar"),
          label = "Columna a explorar:",
          choices = NULL,
          width = "100%"),
        actionButton(
          inputId = ns("composicion_exe"),
          label = "Ejecutar",
          width = "100%"
        ),
        tags$br(),
        tags$br(),
        downloadButton(
          outputId = ns("composicion_descargar_csv"),
          label = "CSV",
          style = "width:100%;"),
        tags$br(),
        tags$br(),
        downloadButton(
          outputId = ns("composicion_descargar_xlsx"),
          label = "Excel",
          style = "width:100%;")),
      box(
        width = 9,
        DT::DTOutput(ns("tabla_composicion"))
      )
    )
  )
}

composicion_server <- function(id, opciones, cache, conn) {
  ns <- NS(id)
  moduleServer(
    id = id,
    module = function(input, output, session) {

      composicion <- reactiveValues(agrupadores = c(), tabla = data.frame())
      episodios <- reactiveValues()

      # Se actualizan los selectize inputs al observar cambios en las columnas
      # de la tabla seleccionada
      observeEvent(opciones$colnames, {
        updateSelectizeInput(
          session = session,
          inputId = "agrupador",
          choices = c("Ninguno", opciones$colnames))
        updateSelectizeInput(
          session = session,
          inputId = "episodios_col_rel",
          selected = "nro_factura",
          choices = opciones$colnames)
        updateSelectizeInput(
          session = session,
          inputId = "composicion_explorar",
          choices = opciones$colnames)
      })

      # Cuando haya un cambio a la columna de agrupador se ejecutará
      # una busqueda en el cache.
      observe({
        # Si hay cambios al agrupador o a episodios se ejecutará
        if (!is.null(opciones$colnames) &&
            input$agrupador %notin% c("", "Ninguno")) {
          tryCatch(
            expr = {
              episodios$agrupadores_items <- cache_call(
                fn = pull_distinct,
                cache = cache,
                cache_params = list(col = input$agrupador),
                non_cache_params = list(data = opciones$tabla),
                cache_depends = opciones$tabla_query,
                prefix = "agrupadores-items"
              )
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
      }) %>%
      bindEvent(input$agrupador)

      # Se actualizan los agrupadores para los episodios
      observe({
        if (length(episodios$agrupadores_items) > 0) {
          # Se decide si utilizar jerarquia con perfil
          perfil_seleccionado <- NULL
          # Si el usuario tiene algun perfil seleccionado este se utilizará
          # cómo pre-selección de los episodios
          if (opciones$perfil_enable) {
            perfil_seleccionado <-
              opciones$perfil_lista[[opciones$perfil_selected]][["jerarquia"]]

          }
          updateSelectizeInput(
            inputId = "episodios_jerarquia_nivel_1",
            choices = episodios$agrupadores_items,
            selected = perfil_seleccionado[["episodio"]],
            server = TRUE
          )
        }
      }) %>%
      bindEvent(episodios$agrupadores_items, opciones$perfil_enable)

      observe({
        composicion$agrupadores <- input$episodios_jerarquia_nivel_1
      })

      observeEvent(input$composicion_exe, {
        tryCatch(
          expr = {
            # Validaciones para evitar errores
            if (input$agrupador != "" &&
                opciones$datos_cargados &&
                input$episodios_col_rel %in% opciones$colnames &&
                !is.null(input$episodios_jerarquia_nivel_1) &&
                input$composicion_explorar != "") {
              # Se busca si la misma tabla ya ha sido generada en el pasado
              # con el cache.
              composicion$tabla <- cache_call(
                fn = function(...) {
                  datos_composicion(...) %>%
                    collect()
                },
                cache = cache,
                cache_params = list(
                  columna_episodios = input$agrupador,
                  columna_valor = opciones$valor_costo,
                  columna_suma = input$episodios_col_rel,
                  columna_explorar = input$composicion_explorar,
                  prioridad = input$episodios_jerarquia_nivel_1,
                  frec_cantidad = opciones$cantidad),
                non_cache_params = list(data = opciones$tabla),
                cache_depends = opciones$tabla_query,
                prefix = "composicion"
              ) %>%
                mutate(participacion_en_episodios =
                         participacion_en_episodios / 100,
                       participacion_valor = participacion_valor / 100)
            }
          },
          error = function(e) {
            print(e)
            sendSweetAlert(
              session = session,
              title = "Error",
              type = "error",
              text = "Por favor revisar los parametros de carga de datos,
                columnas, formato de fecha y los datos. Si este problema
                persiste ponerse en contacto con un administrador."
            )
          })
      })

      # Código en javascript para uso de funciones de DT
      callback_js <- JS(
        "table.on('click', 'tr.dtrg-group', function () {",
        "  var rowsCollapse = $(this).nextUntil('.dtrg-group');",
        "  $(rowsCollapse).toggleClass('hidden');",
        "});"
      )

      # Render de la tabla de composicion
      output$tabla_composicion <- DT::renderDT({
        if (nrow(composicion$tabla) > 0) {
          style_color_valor <- styleColorBar(
            data = composicion$tabla$participacion_valor,
            color = "#87CEEB")
          style_color_participacion <- styleColorBar(
            data = composicion$tabla$participacion_en_episodios,
            color = "#87CEEB")
          datatable(
            composicion$tabla,
            rownames = FALSE,
            colnames = c(
              "Incluida en episodios" = "incluida_n_episodios",
              "Número de episodios" = "n_episodios",
              "% de participación" = "participacion_en_episodios",
              "Suma de valor a explorar" = "valor_explorar",
              "Suma de valor de los episodios" = "valor_episodios",
              "% del valor total" = "participacion_valor",
              "Número de registros" = "n_registros",
              "Registros por episodio" = "registros_por_episodios",
              "Valor medio a explorar de registro" = "media_explorar_registro",
              "Valor medio a explorar por episodio" = "media_explorar_episodio",
              "Valor medio de episodio" = "media_episodio"),
            extensions = c("FixedColumns"),
            options = list(
              pageLength = nrow(composicion$tabla),
              orderFixed = c(0, "desc"),
              scrollY = "600px",
              scrollX = TRUE,
              fixedColumns = list(leftColumns = 5),
              language = list(
                url = dt_spanish)),
            selection = "none"
          ) %>%
            formatPercentage(c("% de participación",
                               "% del valor total"),
                             dec.mark = ",", mark = ".") %>%
            formatCurrency(c("Suma de valor a explorar",
                             "Suma de valor de los episodios",
                             "Valor medio a explorar de registro",
                             "Valor medio de episodio",
                             "Valor medio a explorar por episodio"),
                           dec.mark = ",", mark = ".", digits = 0) %>%
            formatRound(c("Número de episodios", "Número de registros",
                          "Incluida en episodios"),
                        dec.mark = ",", mark = ".", digits = 0) %>%
            formatRound("Registros por episodio",
                        dec.mark = ",", mark = ".", digits = 2) %>%
            formatStyle(
              c("% de participación"),
              background = style_color_participacion,
              backgroundSize = "100% 90%",
              backgroundRepeat = "no-repeat",
              backgroundPosition = "center") %>%
            formatStyle(
              c("% del valor total"),
              background = style_color_valor,
              backgroundSize = "100% 90%",
              backgroundRepeat = "no-repeat",
              backgroundPosition = "center")
        } else {
          data.frame()
        }
      })

      output$composicion_descargar_csv <- downloadHandler(
        filename = function() {
          paste("Composicion",
                ".csv", sep = "")
        },
        content = function(file) {
          write.csv(
            x = composicion$tabla %>%
              rename(
                "Incluida en episodios" = "incluida_n_episodios",
                "Número de episodios" = "n_episodios",
                "% de participación" = "participacion_en_episodios",
                "Suma de valor a explorar" = "valor_explorar",
                "Suma de valor de los episodios" = "valor_episodios",
                "% del valor total" = "participacion_valor",
                "Número de registros" = "n_registros",
                "Registros por episodio" = "registros_por_episodios",
                "Valor medio a explorar de registro" =
                  "media_explorar_registro",
                "Valor medio a explorar por episodio" =
                  "media_explorar_episodio",
                "Valor medio de episodio" = "media_episodio"
              ),
            file = file,
            row.names = FALSE,
            na = "")
        },
        contentType = "text/csv"
      )

      output$composicion_descargar_xlsx <- downloadHandler(
        filename = function() {
          paste("Composicion",
                ".xlsx", sep = "")
        },
        content = function(file) {
          write_xlsx(
            x = as.data.frame(composicion$tabla)  %>%
              rename(
                "Incluida en episodios" = "incluida_n_episodios",
                "Número de episodios" = "n_episodios",
                "% de participación" = "participacion_en_episodios",
                "Suma de valor a explorar" = "valor_explorar",
                "Suma de valor de los episodios" = "valor_episodios",
                "% del valor total" = "participacion_valor",
                "Número de registros" = "n_registros",
                "Registros por episodio" = "registros_por_episodios",
                "Valor medio a explorar de registro" =
                  "media_explorar_registro",
                "Valor medio a explorar por episodio" =
                  "media_explorar_episodio",
                "Valor medio de episodio" = "media_episodio"
              ),
            path = file)
        },
        contentType = "xlsx"
      )

    }
  )
}
