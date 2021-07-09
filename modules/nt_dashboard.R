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

nt_dashboard_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        width = 7,
        height = "500px",
        DT::dataTableOutput(
          outputId = ns("indice_tabla"),
          height = "auto") %>%
          withSpinner()),
      box(
        width = 5,
        height = "500px",
        mapviewOutput(
          height = "480px",
          outputId = ns("indice_mapa")) %>%
          withSpinner())
    ),
    fluidRow(
      valueBoxOutput(outputId = ns("entidad"), width = 6),
      valueBoxOutput(outputId = ns("valor_mes"), width = 6),
      valueBoxOutput(outputId = ns("poblacion"), width = 4),
      valueBoxOutput(outputId = ns("costo_usuario"), width = 4),
      valueBoxOutput(outputId = ns("departamento"), width = 4)),
    fluidRow(
      box(
        width = 12,
        selectizeInput(
          inputId = ns("board_select"),
          width = "100%",
          choices = "Ninguno",
          label = "Nota técnica")),
      box(
        width = 12,
        title = "Nota técnica",
        fluidRow(
          column(
            width = 6,
            DT::dataTableOutput(outputId = ns("board_datos"))),
          column(
            width = 6,
            plotlyOutput(
              outputId = ns("plot_agrupadores"),
              width = "100%",
              height = "600px")
          )
        ),
        uiOutput(ns("otra_informacion"))
      )
    )
  )

}

nt_dashboard_server <- function(id, opciones) {

  ns <- NS(id)

  moduleServer(
    id = id,
    module = function(input, output, session) {

      nt_opciones <- reactiveValues(
        # "indice" = indice,
        # "datos" = nota_tecnica,
        # "inclusiones" = inclusiones
      )

      observeEvent(names(opciones$notas_tecnicas_lista), {
        updateSelectizeInput(
          session = session,
          inputId = "board_select",
          choices = names(opciones$notas_tecnicas_lista)
        )
      })

      output$indice_tabla <- DT::renderDataTable({
        fields_nt <- colnames(opciones$indice_todos)
        if(!is.null(opciones$indice_todos)) {
          datatable(
            opciones$indice_todos %>%
              mutate(vigencia = case_when(
                vigente ~ "Vigente",
                TRUE ~ "No vigente")) %>%
              select(-c(cod_departamento, vigente)),
            rownames = F,
            selection = 'none',
            colnames = c(
              "Nombre NT",
              if ("nom_prestador" %in% fields_nt) "Prestador",
              if ("nom_asegurador" %in% fields_nt) "Asegurador",
              "Población",
              "Departamento",
              "Ciudades",
              "Valor a mes",
              "Vigencia"),
            options = list(
              dom='ft',
              language = list(
                url = dt_spanish),
              pageLength = 10,
              ordering = FALSE,
              scrollX = TRUE,
              scrollY = "400px")) %>%
            DT::formatCurrency(
              columns = c("valor_mes")
              , digits = 0, mark = ".", dec.mark = ",") %>%
            DT::formatRound(columns = "poblacion", mark = ".", dec.mark = ",",
                            digits = 0)
        }
      })

      observe({
        indice_todos <- opciones$indice_todos %>%
          filter(vigente)
        output$indice_mapa <- renderMapview({
          mapa_valores(indice_todos, departamentos)
        })
      })

      observe({
        if (input$board_select %notin% c("Ninguno", "")) {
          nt_opciones$datos <- opciones$notas_tecnicas %>%
            filter(nt == input$board_select) %>%
            rename(cod_nt = nt)
          nt_opciones$indice <- opciones$indice_todos %>%
            filter(cod_nt == input$board_select)
        }
      })

      output$entidad <- renderValueBox({
        if(!is.null(nt_opciones$indice)) {
          prestador <- nt_opciones$indice$nom_prestador
          asegurador <- nt_opciones$indice$nom_asegurador
          valueBox(
            value = paste(
              c(prestador, asegurador),
              collapse = " - "),
            subtitle = paste(
              c(if (!is.null(prestador)) prestador,
                if (!is.null(asegurador)) asegurador),
              collapse = " - "),
            icon = icon("stethoscope", lib = "font-awesome"),
            color = "yellow"
          )
        } else {
          valueBox(
            value = "",
            subtitle = "Prestador",
            icon = icon("stethoscope", lib = "font-awesome"),
            color = "yellow"
          )
        }
      })

      output$valor_mes <- renderValueBox({
        if(!is.null(nt_opciones$indice)) {
          valueBox(
            value = formatAsCurrency(nt_opciones$indice$valor_mes),
            subtitle = "Valor total a mes",
            icon = icon("dollar-sign", lib = "font-awesome"),
            color = "green"
          )
        } else {
          valueBox(
            value = "",
            subtitle = "Valor total a mes",
            icon = icon("dollar-sign", lib = "font-awesome"),
            color = "green"
          )
        }
      })

      output$poblacion <- renderValueBox({
        if (!is.null(nt_opciones$indice)) {
          valueBox(
            value = format(
              nt_opciones$indice$poblacion,
              scientific = F,
              big.mark = ".",
              decimal.mark = ","),
            subtitle = "Población",
            icon = icon("users", lib = "font-awesome"),
            color = "blue"
          )
        } else {
          valueBox(
            value = "",
            subtitle = "Población",
            icon = icon("users", lib = "font-awesome"),
            color = "blue"
          )
        }
      })

      output$costo_usuario <- renderValueBox({
        if (!is.null(nt_opciones$indice)) {
          valueBox(
            value = formatAsCurrency(
              nt_opciones$indice$valor_mes /
                nt_opciones$indice$poblacion),
            subtitle = "Costo por usuario por mes",
            icon = icon("users", lib = "font-awesome"),
            color = "blue"
          )
        } else {
          valueBox(
            value = "",
            subtitle = "Costo por usuario por mes",
            icon = icon("users", lib = "font-awesome"),
            color = "blue"
          )
        }
      })


      output$departamento <- renderValueBox({
        if(!is.null(nt_opciones$indice)) {
          valueBox(
            value = nt_opciones$indice$departamento,
            subtitle = nt_opciones$indice$ciudades,
            icon = icon("city", lib = "font-awesome"),
            color = "aqua"
          )
        } else {
          valueBox(
            value = "",
            subtitle = "",
            icon = icon("city", lib = "font-awesome"),
            color = "aqua"
          )
        }
      })

      output$otra_informacion <- renderUI({

        if (!is.null(opciones$indice_todos) &&
            input$board_select %notin% c("Ninguno", "")) {

          otra_informacion_datos <-
            opciones$notas_tecnicas_lista[[input$board_select]]



          tagList(
            if (!is.null(otra_informacion_datos$inclusiones) ||
                !is.null(otra_informacion_datos$exclusiones)) {
                column(
                  width = 12,
                  tags$hr(),
                  fluidRow(
                    column(
                      width = 6,
                      tags$h3("Inclusiones") %>% tags$u(),
                      tags$ol(
                        purrr::map(
                          .x = otra_informacion_datos$inclusiones,
                          tags$li))),
                    column(
                      width = 6,
                      tags$h3("Exclusiones") %>% tags$u(),
                      tags$ol(
                        purrr::map(
                          .x = otra_informacion_datos$exclusiones,
                          tags$li)))
                    )
                  )
            },
            if (!is.null(otra_informacion_datos$notas)) {
                column(
                  width = 12,
                  tags$hr(),
                  tags$h2("Notas") %>% tags$u(),
                  tags$p(otra_informacion_datos$notas)
                )
            },
            if (!is.null(otra_informacion_datos$perfil)) {
              if (otra_informacion_datos$perfil %in%
                  names(opciones$perfil_lista)) {
                perfil_nota_tecnica <-
                  opciones$perfil_lista[[otra_informacion_datos$perfil]]

                width_row <- 12 /
                  length(names(perfil_nota_tecnica[["jerarquia"]]))

                  column(
                    width = 12,
                    tags$hr(),
                    tags$h2("Conteos especiales y jerarquía") %>% tags$u(),
                    fluidRow(
                      purrr::map2(
                        .x = perfil_nota_tecnica[["jerarquia"]],
                        .y = names(perfil_nota_tecnica[["jerarquia"]]),
                        .f = function(x, y) {
                          if (!is.null(x)) {
                            column(
                              width = width_row,
                              tags$h3(toupper(y)),
                              tags$ol(
                                purrr::map(
                                  .x = intersect(
                                    x = x,
                                    y = pull(nt_opciones$datos, agrupador)),
                                  tags$li))
                            ) %>%
                              return()
                          }
                        })
                    )
                  )

              }
            }
          )

        }

      })

      output$plot_agrupadores <- renderPlotly({
        if (!is.null(nt_opciones$datos)) {
          pie_chart(
            paquetes = nt_opciones$datos,
            columna = "agrupador",
            valor_costo = "valor_mes")
        }
      })

      output$board_datos <- DT::renderDataTable({
        if(!is.null(nt_opciones$datos)) {
          datatable(
            nt_opciones$datos %>%
              select(agrupador, frec_mes, frecuencia_pc, cm, valor_mes),
            rownames = F,
            selection = 'none',
            colnames = c("Agrupador", "Frecuencia a mes",
                         "Frecuencia per capita", "cm", "Valor a mes"),
            options = list(
              dom='ft',
              language = list(
                url = dt_spanish),
              pageLength = nrow(nt_opciones$datos),
              ordering = FALSE,
              scrollX = TRUE,
              scrollY = "600px")) %>%
            DT::formatCurrency(
              columns = c("cm", "valor_mes"),
              digits = 0, mark = ".", dec.mark = ","
            ) %>%
            DT::formatStyle(
              columns = 1:5,
              valueColumns = 1,
              backgroundColor = "white")
        }
      })

  })

}
