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
          inputId = ns("nota_tecnica_exe"),
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
        width = 9,
        valueBoxOutput(
          outputId = ns("nota_tecnica_suma"),
          width = 6),
        valueBoxOutput(
          outputId = ns("nota_tecnica_porcentaje"),
          width = 6),
        valueBoxOutput(
          outputId = ns("nota_tecnica_warnings"),
          width = 12),
        DT::dataTableOutput(ns("nota_tecnica_junta"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        actionButton(
          inputId = ns("nota_tecnica_juntar"),
          label = "Juntar",
          class = "nota_tecnica_juntar_btn")
      ),
      box(
        width = 12,
        div(
          style = "text-align: center;",
          column(
            width = 4,
            tags$h4("Escenarios a mes")
            ),
          column(
            width = 4,
            tags$h4("Media")
          ),
          column(
            width = 4,
            tags$h4("P75")
          )
        ),
        div(
          class = "escenarios_inline_div",
          column(
            width = 4,
            uiOutput(outputId = ns("nota_tecnica_escenarios_nombres"))),
          uiOutput(outputId = ns("nota_tecnica_escenarios"))
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

      observeEvent(input$nota_tecnica_juntar, {

        nota_tecnica$cols_sep <- input$nota_tecnica_cols_sep
        nota_tecnica$cols <- input$nota_tecnica_cols

        tryCatch(
          expr = {
            rows_selected <- NULL

            rows_selected <- list(
              "episodio" = list(),
              "factura" = list(),
              "paciente" = list(),
              "prestacion" = list())

            lapply(
              X = 1:4,
              FUN = function(i) {


                rows_episodio <-
                  input[[paste0("escenario_episodio_", i, "_rows_selected")]]
                rows_factura <-
                  input[[paste0("escenario_factura_", i, "_rows_selected")]]
                rows_paciente <-
                  input[[paste0("escenario_paciente_", i, "_rows_selected")]]
                rows_prestacion <-
                  input[[paste0("escenario_prestacion_", i, "_rows_selected")]]

                rows_selected[["episodio"]][[i]] <<-
                  if (!is.null(rows_episodio)) {
                    nota_tecnica$escenarios[["episodio"]][[i]][
                      rows_episodio]
                  }
                rows_selected[["factura"]][[i]] <<-
                  if (!is.null(rows_factura)) {
                    nota_tecnica$escenarios[["factura"]][[i]][
                      rows_factura]
                  }
                rows_selected[["paciente"]][[i]] <<-
                  if (!is.null(rows_paciente)) {
                    nota_tecnica$escenarios[["paciente"]][[i]][
                      rows_paciente]
                  }
                rows_selected[["prestacion"]][[i]] <<-
                  if (!is.null(rows_prestacion)) {
                    nota_tecnica$escenarios[["prestacion"]][[i]][
                      rows_prestacion]
                  }
              }
            )

            nota_tecnica$tabla_junta <-
              rbindlist(
                fill = TRUE,
                lapply(
                  X = c("episodio", "factura", "paciente", "prestacion"),
                  FUN = function(i) {
                    if (!is.null(rows_selected[[i]])) {
                      seleccionados_juntos <- rbindlist(rows_selected[[i]])
                      if (nrow(seleccionados_juntos) > 0) {
                        return(
                          cbind(
                            "Tipo" = toupper(i),
                            seleccionados_juntos
                          )
                        )
                      } else {
                        return(data.table())
                      }
                    } else {
                      return(data.table())
                    }
                  }
                  )[nota_tecnica$escenarios_activos]
              )

            suma_valor_mes <- sum(
              numerize(nota_tecnica$tabla_junta[["Valor a mes"]]), na.rm = TRUE)

            nota_tecnica$tabla_junta[
              , "Participacion" := `Valor a mes`/suma_valor_mes,
              by = c("Valor a mes")]

          },
          error = function(e) {
            print(e)
            sendSweetAlert(
              session = session,
              title = "Error",
              type = "error",
              text = "Por favor revisar los parametros de carga de datos,
                      columnas, formato de fecha y los datos. Si este problema persiste
                      ponerse en contacto con un administrador.")
            }
          )
      })

      output$nota_tecnica_junta <- DT::renderDataTable({
          if (nrow(nota_tecnica$tabla_junta) == 0 ||
              "CM" %notin% names(nota_tecnica$tabla_junta)) {
            datatable(data = data.table())
          } else {
            datatable(
              data = nota_tecnica$tabla_junta[, -c("Coe")],
              rownames = FALSE,
              options = list(
                ordering = T,
                scrollY = "50vh",
                scrollX = TRUE,
                pageLength = 1000,
                dom = "ft"
              )
            ) %>%
              formatCurrency(
                c("CM", "Valor a mes"),
                dec.mark = ",",
                mark = ".",
                currency = "$",
                digits = 0
              ) %>%
              formatPercentage(
                c("Participacion"),
                dec.mark = ",",
                mark = "."
              )
          }
        }
      )

      output$nota_tecnica_suma <- renderValueBox({
        if (opciones$datos_cargados) {
          valueBox(
            subtitle = "Valor total a mes.",
            value = {
              if (nrow(nota_tecnica$tabla_junta) >= 1) {
                formatAsCurrency(
                  sum(nota_tecnica$tabla_junta[["Valor a mes"]], na.rm = TRUE)
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
          nota_tecnica$valor_total <- opciones$tabla %>%
            transmute(suma_valores = sum(!!as.name(opciones$valor_costo),
                                         na.rm = TRUE)) %>%
            distinct() %>%
            collect() %>%
            unlist() %>%
            as.numeric()
          valueBox(
            subtitle = "Porcentaje del valor de los datos.",
            value = {
              if (nrow(nota_tecnica$tabla_junta) >= 1) {
                formatAsPerc(
                  100 * sum(nota_tecnica$tabla_junta[["Valor a mes"]], na.rm = TRUE) /
                    (nota_tecnica$valor_total /
                       input$nota_tecnica_meses)
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

      output$nota_tecnica_warnings <- renderValueBox({
        if (nrow(nota_tecnica$tabla_junta >= 1) &&
            all(c(nota_tecnica$cols, nota_tecnica$cols_sep) %in%
                names(nota_tecnica$tabla_junta))) {

          repetidos <-sum(duplicated(nota_tecnica$tabla_junta[
            , c(nota_tecnica$cols, nota_tecnica$cols_sep),
            with = FALSE]))

          if (repetidos == 0) {
            valueBox(
              subtitle = "Correcto",
              value = "Estado.",
              color = "green",
              icon = icon("thumbs-up", "font-awesome")
            )
          } else {
            valueBox(
              value = "¡Advertencia!",
              subtitle = paste0(
                "Tienes ", repetidos, " agrupadores duplicados."
              ),
              color = "red",
              icon = icon("exclamation-circle", "font-awesome")
            )
          }
        } else {
          valueBox(
            subtitle = "Correcto",
            value = "Estado.",
            color = "green",
            icon = icon("thumbs-up", "font-awesome")
          )
        }
      })

      output$nota_tecnica_descargar_xlsx <- downloadHandler(
        filename = function() {
          paste("Nota técnica",
                ".xlsx", sep="")
        },
        content = function(file) {
          write_xlsx(
            x = append(
              list("Nota tecnica" = nota_tecnica$tabla_junta[, -c("Coe")]),
              nota_tecnica$descriptiva_escenarios
            ),
            path = file)
        },
        contentType = "xlsx"
      )

      output$nota_tecnica_descargar_csv <- downloadHandler(
        filename = function() {
          paste("Nota técnica",
                ".csv", sep="")
        },
        content = function(file) {
          write_csv(
            x = rbindlist(nota_tecnica$descriptiva_escenarios, idcol = TRUE),
            file = file)
        },
        contentType = "text/csv"
      )

    }
  )
}

clean_datatable <- function(data, length = 1000, columnDefs = NULL) {
  return(
    datatable(
      data = data,
      colnames = c(
        "Valor" = 'Valor a mes'
      ),
      class = "display nowrap",
      rownames = FALSE,
      options = list(
        ordering = F,
        scrollX = TRUE,
        pageLength = length,
        dom = "t",
        columnDefs = columnDefs
      )
    )
  )
}

formato_escenarios <- function(x) {
  brks <- c(1:3)
  clrs <- round(seq(255, 90, length.out = 4), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
  return(
    x %>%
      formatCurrency(
        c("CM", "Valor"),
        dec.mark = ",",
        mark = ".",
        currency = "$",
        digits = 0
      ) %>%
      formatStyle(
        columns = 1:ncol(x[["x"]][["data"]]),
        fontSize = '95%',
        "white-space"="nowrap"
      ) %>%
      formatStyle(
        "Coe",
        target = "row",
        backgroundColor = styleInterval(
          cuts = brks, values = clrs
        )
      ) %>%
      formatStyle(
        "Frecuencia a mes",
        target = "row",
        backgroundColor = styleEqual(
          levels = c(0, 1),
          values = c('rgb(255, 218, 84)',
                     'rgb(255, 218, 84)')
        )
      )
  )
}

nota_tecnica_cajas_jerarquia <- function(
  ns,
  items_nivel_1 = NULL,
  items_nivel_2 = NULL,
  items_nivel_3 = NULL,
  items_nivel_4 = NULL) {
  return(
    tags$div(
      class = "nota_tecnica_jerarquia_row",
      box(
        width = 3,
        orderInput(
          inputId = ns("nota_tecnica_jerarquia_nivel_1"),
          label = actionLink(ns("seleccionar_episodio"), label = "Episodio"),
          items = items_nivel_1,
          width = "100%",
          height = "100%",
          connect = c(
            ns("nota_tecnica_jerarquia_nivel_2"),
            ns("nota_tecnica_jerarquia_nivel_3"),
            ns("nota_tecnica_jerarquia_nivel_4")))
      ),
      box(
        width = 3,
        orderInput(
          inputId = ns("nota_tecnica_jerarquia_nivel_2"),
          label = actionLink(ns("seleccionar_factura"), label = "Factura"),
          items = items_nivel_2,
          width = "100%",
          height = "100%",
          connect = c(
            ns("nota_tecnica_jerarquia_nivel_1"),
            ns("nota_tecnica_jerarquia_nivel_3"),
            ns("nota_tecnica_jerarquia_nivel_4")))
      ),
      box(
        width = 3,
        orderInput(
          inputId = ns("nota_tecnica_jerarquia_nivel_3"),
          label = actionLink(ns("seleccionar_paciente"), label = "Paciente"),
          items = items_nivel_3,
          width = "100%",
          height = "100%",
          connect = c(
            ns("nota_tecnica_jerarquia_nivel_1"),
            ns("nota_tecnica_jerarquia_nivel_2"),
            ns("nota_tecnica_jerarquia_nivel_4")))
      ),
      box(
        width = 3,
        orderInput(
          inputId = ns("nota_tecnica_jerarquia_nivel_4"),
          label = actionLink(ns("seleccionar_prestacion"), label = "Prestación"),
          items = items_nivel_4,
          width = "100%",
          height = "100%",
          connect = c(
            ns("nota_tecnica_jerarquia_nivel_1"),
            ns("nota_tecnica_jerarquia_nivel_2"),
            ns("nota_tecnica_jerarquia_nivel_3")))
      )
    )
  )
}
