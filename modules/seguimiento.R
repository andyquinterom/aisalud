seguimiento_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        width = 4,
        selectizeInput(
          inputId =  ns("nota_tecnica"),
          label = "Comparar con nota técnica:",
          choices = NULL,
          multiple = FALSE),
        selectizeInput(
          inputId = ns("agrupador"),
          label = "Agrupador principal:",
          choices = NULL,
          multiple = FALSE),
        checkboxGroupInput(
          inputId = ns("tablas"),
          label = "Seguimiento:",
          choices = c(
            "Frecuencias" = "frecuencias",
            "Valor facturado" = "valor"),
          selected = "descriptiva",
          inline = TRUE,
          width = "100%"),
        checkboxInput(
          inputId = ns("episodios"),
          label = "Agrupador por episodios",
          value = FALSE),
        uiOutput(outputId = ns("episodios_col_rel")),
        tags$div(
          style = "overflow-y: scroll; max-height: 300px;",
          uiOutput(outputId = ns("episodios_jerarquia"))),
        tags$br(),
        actionButton(ns("exe"), "Generar"),
        tags$br()
      ),
      box(
        width = 8
      )
    )
  )
}

seguimiento_server <- function(id, opciones, conn) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- NS(id)
      episodios <- reactiveValues(
        widget_jerarquia = radioButtons(
                inputId = ns("unidades"),
                label = "Unidad de descriptiva",
                choiceNames = c("Prestación", "Paciente", "Factura"),
                choiceValues = c(
                  "prestacion",
                  "nro_identificacion",
                  "nro_factura"
                )
              ))

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
        updateSelectizeInput(
          session = session,
          inputId = "separadores",
          choices = opciones$colnames
        )
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

      observeEvent(cambio_columnas(), {
        cache_id <- digest(
          object = list("agrup", cambio_columnas(), opciones$tabla_query),
          algo = "xxhash32",
          seed = 1)
        check_cache <- cache_id %in% names(opciones$cache)
        # Si hay cambios al agrupador o a episodios se ejecutará
        if (!is.null(opciones$colnames) &&
            input$agrupador %notin% c("", "Ninguno") &&
            !check_cache) {
          tryCatch(
            expr = {
              episodios$agrupadores_items <- list()
              # Si la opción de episodios es verdadera y la columna contiene
              # menos de 60 agrupadores únicos entonces se genera widget de
              # jerarquia de episodios
              if (input$episodios) {
                episodios$agrupadores_items <- opciones$tabla %>%
                  select(!!as.name(input$agrupador)) %>%
                  distinct() %>%
                  pull(!!as.name(input$agrupador))
                if (length(episodios$agrupadores_items) > 60) {
                  # Si hay mas de 60 agrupadores únicos se agapará la opción
                  # de episodios y se volvera a correr el código del observer
                  # (tipo recursivo)
                  episodios$agrupadores_items <- list()
                }
              }
              opciones$cache[[cache_id]] <- episodios$agrupadores_items
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
        if (check_cache) {
          episodios$agrupadores_items <- opciones$cache[[cache_id]]
        }
      })

      observe({
        cambio_columnas()
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
              funcion_jerarquia = descriptiva_jerarquia,
              ns = ns
            )
          }
          if (!opciones$perfil_enable) {
            episodios$widget_jerarquia <- descriptiva_jerarquia(
              ns = ns,
              items_nivel_4 = episodios$agrupadores_items
            )
          }
        }
      })

      output$episodios_jerarquia <- renderUI({
        episodios$widget_jerarquia
      })

      # Se observa que el usuario haga click en los titulos de las unidades
      # de conteo en el widget de jerarquia.
      # De esta manera se pueden mover los diferentes agrupadores de manera
      # sencilla entre unidades.

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

    }
  )
}
