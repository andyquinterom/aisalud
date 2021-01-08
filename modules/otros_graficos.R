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
      
      observeEvent(input$edades_ejecutar, {
        tryCatch(
          expr = {
            if (input$edades_select_columna != "" && 
                input$edades_select_columna != "Ninguno") {
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
                  columna_numero = input$edades_select_columna,
                  columna_sep = NULL)
              } else {
                segmentacion <- input$edades_segmentacion_select_columna
                columna_edades <- input$edades_select_columna
                edades_pacientes <- opciones$tabla %>%
                  group_by(nro_identificacion, !!as.name(segmentacion)) %>%
                  summarise(edad = max(!!as.name(columna_edades),
                                       na.rm = TRUE))
                graficos$edades <- histograma_edades(
                  data = edades_pacientes,
                  columna_numero = columna_edades,
                  columna_sep = segmentacion)
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