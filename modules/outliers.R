outliers_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 3,
        radioButtons(
          inputId = ns("outliers_modo"),
          label = "Método de cálculo:",
          choiceNames = c("Percentil", "Rango intercuartil"),
          choiceValues = c("percentil", "iqr")
        ),
        uiOutput(ns("outliers_modo_opciones")),
        numericInput(
          inputId = ns("outliers_frecuencia"),
          label = "Frecuencia mínima:",
          min = 0,
          step = 1, 
          value = 0),
        actionButton(
          inputId = ns("outliers_exe"),
          label = "Ejecutar",
          width = "100%"),
        tags$br(),
        tags$br(),
        actionButton(
          inputId = ns("outliers_excluir"),
          label = "Excluir pacientes seleccionados",
          width = "100%"),
        tags$br(),
        tags$br(),
        downloadButton(
          outputId = ns("outliers_descargar_csv"),
          label = "CSV", 
          style = "width:100%;"),
        tags$br(),
        tags$br(),
        downloadButton(
          outputId = ns("outliers_descargar_xlsx"),
          label = "Excel",
          style = "width:100%;")),
      box(
        width = 9,
        tags$h2(textOutput(ns("outliers_titulo")), class = "titulo_center"),
        tags$br(),
          div(
            DT::dataTableOutput(ns("outliers_tabla")) %>%
              withSpinner(),
              style = "font-size:90%"
          )
      )
    )
  )
}

outliers_server <- function(id, opciones) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      ns <- NS(id)
      
      outliers <- reactiveValues(tabla = data.table())
      
      observe({
        output$outliers_modo_opciones <- renderUI({
          if (input$outliers_modo == "percentil") {
            sliderTextInput(
              inputId = ns("outliers_percentil"),
              label = "Outliers mayores que % de los usuarios:",
              choices = c(seq(75, 95, 5), 99),
              selected = 90
            )
          } else {
            sliderTextInput(
              inputId = ns("outliers_iqr"),
              label = "Outliers por IQR",
              choices = c(1.5, 3.0, 6.0, 12.0, 24.0),
              selected = 1.5
            )
          }
        })
      })
      
      observeEvent(input$outliers_excluir, {
        if(opciones$tabla_nombre != "Ninguno") {
          if (length(input$outliers_tabla_rows_selected) > 0) {
            opciones$pacientes_excluir <- unname(c(
              opciones$pacientes_excluir,
              unlist(
                outliers$tabla[
                  input$outliers_tabla_rows_selected, "nro_identificacion"])
          ))
            opciones$pacientes_excluir_exe <- TRUE
            opciones$pacientes_excluir_exe <- FALSE

          showNotification(
            ui = "Pacientes añadidos a la lista. Por favor aplicar filtros.",
            duration = 10
          )

          } else {
            sendSweetAlert(
              session = session,
              title = "Error",
              text = "Por favor seleccionar al menos un pacient a excluir.",
              type = "error"
            )
          }
        }
      })

      
      output$outliers_titulo <- renderText({
        outliers$titulo
      })
      
      observeEvent(input$outliers_exe, {
        outliers_cols <- "nro_identificacion"
        valor_costo <- opciones$valor_costo
        frecuencia <- input$outliers_frecuencia
        showNotification("Procesando outliers...")
        if (opciones$tabla_nombre != "Ninguno") {
          tryCatch(
            expr = {
              if (input$outliers_modo == "percentil") {
                outliers$tabla <- outliers_percentil(
                  data =          opciones$tabla,
                  columna =       outliers_cols,
                  columna_valor = valor_costo,
                  percentil =     input$outliers_percentil/100,
                  frecuencia =    frecuencia)
                outliers$titulo <- paste(
                  "Pacientes con un valor mayor que el",
                  formatAsPerc(input$outliers_percentil),
                  "del total por",
                  input$outliers_cols
                )
              } else {
                outliers$tabla <- outliers_iqr(
                  data =           opciones$tabla,
                  columna =        outliers_cols,
                  columna_valor =  valor_costo,
                  multiplicativo = input$outliers_iqr,
                  frecuencia =     frecuencia)
                outliers$titulo <- paste(
                  "Pacientes por fuera de",
                  input$outliers_iqr,
                  "veces el rango intercuartil por",
                  input$outliers_cols
                )
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
        }
      })
      
      output$outliers_tabla <- DT::renderDataTable({
        if (nrow(outliers$tabla) > 0) {
          DT::datatable(
            outliers$tabla,
            colnames = c('Valor' = 'valor_calculos',
                         "Prestaciones" = "frec",
                         "Contribución" = "porcentaje"),
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 50,
              autoWidth = FALSE,
              ordering = T,
              scrollX = TRUE,
              scrollY = "500px"),
            rownames = FALSE) %>%
          formatCurrency(c('Valor'), mark = ".", dec.mark = ",") %>%
          DT::formatRound(c('Prestaciones'),
                          mark = ".",
                          dec.mark = ",",
                          digits = 0)
        } else {
          data.table()
        }
      })
      

      output$outliers_descargar_csv <- downloadHandler(
        filename = function() {
          paste("Outliers para ",
                opciones$tabla_nombre,
                ".csv", sep = "")
        },
        content = function(file) {
          write.csv(
            x = outliers$tabla,
            file = file,
            row.names = FALSE,
            na = "")
        },
        contentType = "text/csv"
      )

      output$outliers_descargar_xlsx <- downloadHandler(
        filename = function() {
          paste("Outliers para ",
                opciones$tabla_nombre,
                ".xlsx", sep = "")
        },
        content = function(file) {
          write_xlsx(
            x = as.data.frame(outliers$tabla),
            path = file)
        },
        contentType = "xlsx"
      )
      
    }
  )
}