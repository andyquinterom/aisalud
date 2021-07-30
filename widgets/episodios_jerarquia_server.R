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

episodios_jerarquia_server <- function(episodios, opciones, cache, id = NULL,
  session = getDefaultReactiveDomain(), separadores = FALSE) {

  input <- session$input
  output <- session$output
  ns <- NS(id)

  # Se observa cambios en los nombres de columnas para actulizar
  # selectizeInputs
  observeEvent(opciones$colnames, {
    # Solo se actualiza si los episodios estan prendidos
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
    if (separadores) {
      updateSelectizeInput(
        session = session,
        inputId = "separadores",
        choices = opciones$colnames
      )
    }
  })

  # Cambios a la seleccion de episodios
  observeEvent(input$episodios, {
    # Si es FALSE no se muestra el input de columnad de relacion
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

  # Reactive que observa cambbios a input$agrupador e input$episodios
  cambio_columnas <- reactive({
    list(input$agrupador, input$episodios)
  })

  observe({
    updateCheckboxInput(
        inputId = "episodios",
        value = FALSE
      )
  }) %>%
  bindEvent(opciones$tabla_query)

  observe({
    agrupador_valido <- !is.null(opciones$colnames) &&
      input$agrupador %notin% c("", "Ninguno")
    # Si hay cambios al agrupador o a episodios se ejecutará
    if (!agrupador_valido) episodios$agrupadores_items <- NULL
    if (agrupador_valido) {
      tryCatch(
        expr = {
          if  (!input$episodios) episodios$agrupadores_items <- NULL
          if (input$episodios) {
            episodios$agrupadores_items <- cache_call(
              fn = pull_distinct,
              cache = cache,
              cache_params = list(col = input$agrupador),
              non_cache_params = list(data = opciones$tabla),
              cache_depends = opciones$tabla_query,
              prefix = "agrupadores-items"
            )
          }
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
  bindEvent(cambio_columnas())

  observe({
    if (length(episodios$agrupadores_items) == 0) {
      # Widget de unidades de descriptiva
      episodios$widget_jerarquia <- radioButtons(
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
      updateCheckboxInput(
        inputId = "episodios",
        value = FALSE
      )
    }
    if (length(episodios$agrupadores_items) > 0) {
      # Se decide si utilizar jerarquia con perfil
      if (opciones$perfil_enable) {
        episodios$widget_jerarquia <- perfil_jerarquia(
          perfiles = opciones$perfil_lista,
          perfil_select = opciones$perfil_selected,
          items = episodios$agrupadores_items,
          funcion_jerarquia = jerarquia,
          ns = ns
        )
      }
      if (!opciones$perfil_enable) {
        episodios$widget_jerarquia <- jerarquia(
          ns = ns,
          items_nivel_4 = episodios$agrupadores_items
        )
      }
    }
  }) %>%
  bindEvent(episodios$agrupadores_items, cambio_columnas())

  output$episodios_jerarquia <- renderUI({
    episodios$widget_jerarquia
  })

  observe({
    episodios$widget_jerarquia <- jerarquia(
      ns = ns,
      items_nivel_1 = episodios$agrupadores_items)
  }) %>%
  bindEvent(input$seleccionar_episodio)

  observe({
    episodios$widget_jerarquia <- jerarquia(
      ns = ns,
      items_nivel_2 = episodios$agrupadores_items)
  }) %>%
  bindEvent(input$seleccionar_factura)

  observe({
    episodios$widget_jerarquia <- jerarquia(
      ns = ns,
      items_nivel_3 = episodios$agrupadores_items)
  }) %>%
  bindEvent(input$seleccionar_paciente)

  observe({
    episodios$widget_jerarquia <- jerarquia(
      ns = ns,
      items_nivel_4 = episodios$agrupadores_items)
  }) %>%
  bindEvent(input$seleccionar_prestacion)

}
