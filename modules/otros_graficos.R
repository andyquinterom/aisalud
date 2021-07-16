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

otros_graficos_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        width = 12,
        tabsetPanel(
          tabPanel(
            title = "Distribución de edades",
            tags$br(),
            fluidRow(
              column(
                width = 3,
                selectizeInput(
                  inputId = ns("edades_select_columna"),
                  label = "Columna de edad:",
                  choices = "Ninguno",
                  width = "100%"
                ),
                selectizeInput(
                  inputId = ns("edades_segmentacion_select_columna"),
                  label = "Variable de segmentación:",
                  choices = "Ninguno",
                  width = "100%"
                ),
                uiOutput(ns("edades_segmentacion_select_agrupadores_ui")),
                sliderTextInput(
                  inputId = ns("edades_numero_columnas"),
                  label = "Número de columnas",
                  choices = c("Auto", 5, 10, 20, 25),
                  width = "100%",
                  selected = "Auto"
                ),
                actionButton(
                  inputId = ns("edades_ejecutar"),
                  label = "Ejecutar",
                  width = "100%"
                )
              ),
              column(
                width = 9,
                tags$h3(
                  textOutput(ns("edades_titulo")), class = "titulo_center"),
                plotlyOutput(
                  outputId = ns("edades_render")
                ) %>%
                  withSpinner()
              )
            )
          )
        )
      )
    )
  )
}

otros_graficos_server <- function(id, opciones) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- NS(id)

      graficos <- reactiveValues()

      observeEvent(opciones$colnames, {
        updateSelectizeInput(
          session = session,
          inputId = "edades_select_columna",
          choices = opciones$colnames_num,
          selected = "edad"
        )
        updateSelectizeInput(
          session = session,
          inputId = "edades_segmentacion_select_columna",
          choices = c("Ninguno", opciones$colnames),
          selected = "Ninguno"
        )
      })

      observeEvent(input$edades_segmentacion_select_columna, {
        segmentacion_columna <- input$edades_segmentacion_select_columna
        if (segmentacion_columna != "Ninguno" && segmentacion_columna != "") {
          output$edades_segmentacion_select_agrupadores_ui <- renderUI({
            selectizeInput(
              inputId = ns("edades_segmentacion_select_agrupadores"),
              label = "Incluir:",
              choices = {
                opciones$tabla_original %>%
                  select({{ segmentacion_columna }}) %>%
                  distinct() %>%
                  collect() %>%
                  unlist() %>%
                  unname()
                },
              multiple = TRUE
            )
          })
        } else {
          output$edades_segmentacion_select_agrupadores_ui <- renderUI({})
        }
      })

      observeEvent(input$edades_ejecutar, {
        tryCatch(
          expr = {
            if (input$edades_select_columna != "" &&
                input$edades_select_columna != "Ninguno") {
              if (input$edades_numero_columnas == "Auto") {
                numero_bins <- NULL
              } else {
                numero_bins <- input$edades_numero_columnas
              }
              if (input$edades_segmentacion_select_columna == "Ninguno" ||
                  input$edades_segmentacion_select_columna == "") {
                segmentacion <- input$edades_segmentacion_select_columna
                columna_edades <- input$edades_select_columna
                edades_pacientes <- opciones$tabla %>%
                  group_by(nro_identificacion) %>%
                  summarise(edad = max(!!as.name(columna_edades),
                                       na.rm = TRUE))
                graficos$edades <- histograma_edades(
                  data = edades_pacientes,
                  columna_numero = "edad",
                  columna_sep = NULL,
                  numero_bins = numero_bins)
              } else {
                segmentacion <- input$edades_segmentacion_select_columna
                segmentacion_seleccionado <-
                  input$edades_segmentacion_select_agrupadores
                columna_edades <- input$edades_select_columna
                edades_pacientes <- opciones$tabla %>%
                  filter(!!as.name(segmentacion) %in% segmentacion_seleccionado) %>%
                  group_by(nro_identificacion, !!as.name(segmentacion)) %>%
                  summarise(edad = max(!!as.name(columna_edades),
                                       na.rm = TRUE))
                graficos$edades <- histograma_edades(
                  data = edades_pacientes,
                  columna_numero = "edad",
                  columna_sep = segmentacion,
                  numero_bins = numero_bins)
              }
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
      })

      output$edades_render <- renderPlotly({
        if (!is.null(graficos$edades)) {
          graficos$edades
        }
      })

      output$edades_titulo <- renderText({
        paste0(
          "Distribución de edades",
          ifelse(
            test = input$edades_segmentacion_select_columna != "Ninguno",
            yes = paste(
              " segmentada por", input$edades_segmentacion_select_columna),
            no = ""
          )
        )
      })



    }
  )
}