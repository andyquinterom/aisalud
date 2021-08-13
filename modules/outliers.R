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

outliers_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      uiOutput(ns("identificacion_check")),
      box(width = 3,
        tabsetPanel(
          tabPanel(title = "outliers",
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
              style = "width:100%;")
            ),
          tabPanel(
            title = "Identificación pacientes",
            tags$br(),
            radioButtons(
              inputId = ns("identificacion_pacientes"),
              label = "Agrupador:",
              choiceNames = c("Prestación", "Diagnóstico"),
              choiceValues = c("descrip_prest", "descrip_dx")
            ),
            selectizeInput(
              inputId = ns("episodios_jerarquia_nivel_1"),
              label = "Episodios:",
              choices = NULL,
              multiple = TRUE),
            actionButton(
              inputId = ns("identificacion_exe"),
              label = "Ejecutar",
              width = "100%"),
            actionButton(
              inputId = ns("identificacion_excluir"),
              label = "Excluir pacientes seleccionados",
              width = "100%"),
            )
          )
        ),
      box(
        width = 9,
        conditionalPanel(
          condition = '$("#outlier_change").attr("class") == "outlier_on"',
          tags$h2(textOutput(ns("outliers_titulo")), class = "titulo_center"),
          tags$br(),
          div(
            DT::dataTableOutput(ns("outliers_tabla")) %>%
              withSpinner(),
            style = "font-size:90%"
            )
        ),
        conditionalPanel(
          condition = 
            '$("#outlier_change").attr("class") == "outlier_off"',
          tags$h2("Identificacion de clientes"),
          tags$br(),
          div(
            DT::dataTableOutput(ns("identificacion_tabla")) %>%
              withSpinner(),
            style = "font-size:90%"
          )
        )
        )
      )
    )
}

outliers_server <- function(id, opciones, cache) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- NS(id)

      outliers <- reactiveValues(tabla = data.table(),
                                 outlier_activity = TRUE)

      observe({
        if (opciones$datos_cargados) {
        updateSelectizeInput(
          inputId = "episodios_jerarquia_nivel_1",
          choices = opciones$tabla %>% 
            pull_distinct(col = input$identificacion_pacientes),
          server = TRUE
            )
        }
      })
      
      observeEvent(input$outliers_exe,{
        outliers$outlier_activity <- TRUE
      })
      
      observeEvent(input$identificacion_exe,{
        outliers$outlier_activity <- FALSE
      })
      
      output$identificacion_check <- renderUI({
        tags$div(
          id = "outlier_change",
          class = ifelse(
            test = outliers$outlier_activity,
            yes = "outlier_on",
            no = "outlier_off"
          )
        )
      })
      
      output$limit_check <- renderUI({
        tags$div(
          id = "limites_exist",
          class = ifelse(
            test = episodios$limites,
            yes = "limites_exist",
            no = "limites_non"
          )
        )
      })
      
      observe({
        jeraquia <- input$episodios_jerarquia_nivel_1
        opciones$identificacion <- 
          identificar_episodios(
            data = opciones$tabla,
            agrupador = input$identificacion_pacientes,
            columna_suma = "nro_identificacion",
            jerarquia = jeraquia
          ) %>% collect()
      }) %>% bindEvent(input$identificacion_exe)
      
      output$identificacion_tabla <- DT::renderDataTable({
        if (nrow(opciones$identificacion) > 0) {
          DT::datatable(
            opciones$identificacion
          )
        }
      })
      
      identificacion_excluir_count <- counter()
      observeEvent(input$identificacion_excluir, {
        if (opciones$datos_cargados) {
          if (length(input$identificacion_tabla_rows_selected) > 0) {
            opciones$identificacion_excluir <- 
              opciones$identificacion %>% 
              mutate(fila = row_number()) %>% 
              filter(fila %in% input$identificacion_tabla_rows_selected) %>% 
              select(nro_identificacion)
                                      
            opciones$identificacion_excluir_exe <- identificacion_excluir_count()
            showNotification(
              ui = "Clientes añadidos a la lista. Por favor aplicar filtros.",
              duration = 10
            )
            
          } else {
            sendSweetAlert(
              session = session,
              title = "Error",
              text = "Por favor seleccionar al menos un cliente a excluir.",
              type = "error"
            )
          }
        }
      })
      
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

      outliers_excluir_count <- counter()
      observeEvent(input$outliers_excluir, {
        if (opciones$datos_cargados) {
          if (length(input$outliers_tabla_rows_selected) > 0) {
            opciones$pacientes_excluir <- unname(c(
              opciones$pacientes_excluir,
              unlist(
                outliers$tabla[
                  input$outliers_tabla_rows_selected, "nro_identificacion"])
          ))
            opciones$pacientes_excluir_exe <- outliers_excluir_count()

          showNotification(
            ui = "Pacientes añadidos a la lista. Por favor aplicar filtros.",
            duration = 10
          )

          } else {
            sendSweetAlert(
              session = session,
              title = "Error",
              text = "Por favor seleccionar al menos un paciente a excluir.",
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
        if (opciones$datos_cargados) {
          tryCatch(
            expr = {
              if (input$outliers_modo == "percentil") {
                outliers$tabla <- cache_call(
                  fn = outliers_percentil,
                  cache = cache,
                  cache_params = list(
                    columna =       outliers_cols,
                    columna_valor = valor_costo,
                    frec_cantidad = opciones$cantidad,
                    percentil =     input$outliers_percentil/100,
                    frecuencia =    frecuencia),
                  non_cache_params = list(data = opciones$tabla),
                  prefix = "outliers_per",
                  cache_depends = opciones$tabla_query
                )

                outliers$titulo <- paste(
                  "Pacientes con un valor mayor que el",
                  formatAsPerc(input$outliers_percentil),
                  "del total por",
                  input$outliers_cols
                )
              } else {
                outliers$tabla <- cache_call(
                  fn = outliers_iqr,
                  cache = cache,
                  cache_params = list(
                    columna =        outliers_cols,
                    columna_valor =  valor_costo,
                    frec_cantidad = opciones$cantidad,
                    multiplicativo = input$outliers_iqr,
                    frecuencia =     frecuencia),
                  non_cache_params = list(data = opciones$tabla),
                  prefix = "outliers_iqr",
                  cache_depends = opciones$tabla_query
                )
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
                url = dt_spanish),
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
