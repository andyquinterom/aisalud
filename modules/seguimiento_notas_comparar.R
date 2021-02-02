seguimiento_notas_comparar_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        uiOutput(ns("comparar_jerarquia")) 
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          width = 4,
          selectizeInput(
            inputId = ns("comparar_select"),
            width = "100%",
            choices = "Ninguno", 
            label = "Nota técnica"),
          checkboxInput(
            inputId = ns("comparar_episodios"),
            label = "Agrupar por episodios",
            value = F
          ),
          uiOutput(
            outputId = ns("comparar_col_valor_out")
          ),
          selectizeInput(
            inputId = ns("comparar_agrupador"),
            label = "Agrupar por:",
            choices = c("Ninguno"),
            multiple = FALSE),
          actionButton(
            inputId = ns("comparar_exe"),
            "Ejecutar",
            width = "100%"),
          tags$br(),
          tags$br(),
          downloadButton(
            outputId = ns("comparar_descargar_xlsx"), 
            label = "Excel",
            style = "width:100%;")),
        box(
          width = 8,
          DT::dataTableOutput(
            outputId = ns("comparar_totales"),
            width = "100%",
            height = "100%") %>%
            withSpinner()))),
    
    fluidRow(
      column(
        width = 12,
        box(
          width = 12, 
          title = "Resultados a mes",
          fluidRow(
            column(
              width = 6,
              tags$h3("Totales RIPS"),
              DT::dataTableOutput(
                outputId = ns("comparar_total_mes_rips"),
                width = "100%")),
            column(
              width = 6,
              tags$h3("Totales CME"),
              DT::dataTableOutput(
                outputId = ns("comparar_total_mes_cme"),
                width = "100%")))),
        box(
          width = 12,
          title = "Resultados por agrupador",
          fluidRow(
            column(
              width = 6,
              tags$h3("Totales RIPS"),
              DT::dataTableOutput(
                outputId = ns("comparar_total_agrup_rips"),
                width = "100%")),
            column(
              width = 6,
              tags$h3("Totales CME"),
              DT::dataTableOutput(
                outputId = ns("comparar_total_agrup_cme"),
                width = "100%")))),
        box(
          width = 12, 
          title = "Suma de valor a mes",
          DT::dataTableOutput(
            outputId = ns("comparar_desc_sumas"),
            width = "100%")),
        box(
          width = 12, 
          title = "Frecuencias a mes",
          DT::dataTableOutput(
            outputId = ns("comparar_desc_frecs"),
            width = "100%")),
        box(
          width = 12, 
          title = "Diferencias de valor con RIPS",
          DT::dataTableOutput(
            outputId = ns("comparar_diferencias_rips_sumas"),
            width = "100%"),
          tags$br(),
          DT::dataTableOutput(
            outputId = ns("comparar_diferencias_rips_percent"), 
            width = "100%")),
        box(
          width = 12,
          title = "Diferencias de valor con CME",
          DT::dataTableOutput(
            outputId = ns("comparar_diferencias_cme_sumas"),
            width = "100%"),
          tags$br(),
          DT::dataTableOutput(
            outputId = ns("comparar_diferencias_cme_percent"),
            width = "100%"))))
  )
}


seguimiento_notas_comparar_server <- function(id, nota_tecnica, indice, opciones) {
  
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- NS(id)
      
      style_interval <-ifelse(
        test = (Sys.getenv("NT_MODO_IPS") != "") %>% rep(2),
        yes = c("rgb(145, 255, 145)", "rgb(255, 145, 145)"),
        no = c("rgb(255, 145, 145)", "rgb(145, 255, 145)"))
      
      observeEvent(comparar$indice, {
        updateSelectizeInput(
          session = session,
          choices = comparar$indice$cod_nt,
          inputId = "comparar_select"
        )
      })
      
      comparar <- reactiveValues(
        datos = data.table(),
        agrupadores_items = NULL,
        indice = indice
      )
      
      observeEvent(opciones$colnames, {
        if (input$comparar_episodios) {
          updateSelectizeInput(
            session = session,
            inputId = "comparar_col_valor",
            choices = opciones$colnames
          )
        }
        updateSelectizeInput(
          session = session,
          inputId = "comparar_agrupador",
          choices = opciones$colnames
        )
      })
      
      observeEvent(input$comparar_episodios, {
        if (input$comparar_episodios) {
          output$comparar_col_valor_out <- renderUI({
            selectizeInput(
              inputId = ns("comparar_col_valor"),
              label = "Sumar valor por:",
              selected = "nro_identificacion",
              choices = opciones$colnames,
              multiple = FALSE)
          })
        } else {
          output$comparar_col_valor_out <- renderUI({})
        }
      })
      
      cambio_columnas <- reactive({
        list(input$comparar_agrupador, input$comparar_episodios)
      })
      
      observeEvent(cambio_columnas(), {
        if (opciones$datos_cargados && 
            input$comparar_agrupador != "") {
          tryCatch(
            expr = {
              agrupadores_items_length <- opciones$tabla %>%
                select(!!as.name(input$comparar_agrupador)) %>%
                distinct() %>%
                transmute(count = n()) %>%
                distinct() %>%
                collect() %>%
                unlist()
              if (agrupadores_items_length <= 60 &&
                  input$comparar_episodios) {
                agrupadores_items <- opciones$tabla %>%
                  select(!!as.name(input$comparar_agrupador)) %>%
                  distinct() %>%
                  collect() %>%
                  as.list()
                comparar$agrupadores_items <- agrupadores_items[[1]]
                output$comparar_jerarquia <- renderUI({
                  tagList(
                    comparar_cajas_jerarquia(
                      ns = ns,
                      items_nivel_4 = comparar$agrupadores_items)
                  )
                })
              } else {
                comparar$agrupadores_items <- NULL
                output$comparar_jerarquia <- renderUI({
                  box(
                    width = 12,
                    radioButtons(
                      inputId = ns("descriptiva_unidades"),
                      label = "Unidad de descriptiva",
                      choiceNames = c(
                        "Prestación",
                        "Paciente",
                        "Factura"
                      ),
                      choiceValues = c(
                        "prestacion",
                        "nro_identificacion",
                        "nro_factura"
                      )
                    ))
                })
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
      
      observeEvent(input$seleccionar_episodio, {
        output$comparar_jerarquia <- renderUI({
          tagList(
            comparar_cajas_jerarquia(
              ns = ns,
              items_nivel_1 = comparar$agrupadores_items)
          )
        })
      })
      
      observeEvent(input$seleccionar_factura, {
        output$comparar_jerarquia <- renderUI({
          tagList(
            comparar_cajas_jerarquia(
              ns = ns,
              items_nivel_2 = comparar$agrupadores_items)
          )
        })
      })
      
      observeEvent(input$seleccionar_paciente, {
        output$comparar_jerarquia <- renderUI({
          tagList(
            comparar_cajas_jerarquia(
              ns = ns,
              items_nivel_3 = comparar$agrupadores_items)
          )
        })
      })
      
      observeEvent(input$seleccionar_prestacion, {
        output$comparar_jerarquia <- renderUI({
          tagList(
            comparar_cajas_jerarquia(
              ns = ns,
              items_nivel_4 = comparar$agrupadores_items)
          )
        })
      })
      
      observeEvent(input$comparar_exe, {
        if(!is.null(opciones$colnames) && input$comparar_select != "Ninguno") {
          comparar$select <- input$comparar_select
          comparar$agrupador <- input$comparar_agrupador
          comparar$col_valor <- input$comparar_col_valor
          tryCatch(
            expr = {
              
              comparar$datos <- nota_tecnica[
                cod_nt == input$comparar_select]
              
              print(comparar$datos)
              
              if (!is.null(comparar$agrupadores_items)) {
                descriptiva_basica_tabla <- descriptiva_basica_jerarquia(
                  data = opciones$tabla,
                  columnas = comparar$agrupador,
                  columna_valor = opciones$valor_costo,
                  columna_suma = comparar$col_valor,
                  nivel_1 = input$comparar_jerarquia_nivel_1_order,
                  nivel_2 = input$comparar_jerarquia_nivel_2_order,
                  nivel_3 = input$comparar_jerarquia_nivel_3_order,
                  nivel_4 = input$comparar_jerarquia_nivel_4_order
                )
              } else {
                descriptiva_basica_tabla <- descriptiva_basica(
                  data = opciones$tabla,
                  agrupador = comparar$agrupador,
                  columna_valor = opciones$valor_costo,
                  columna_suma = input$descriptiva_unidades,
                  prestaciones = (input$descriptiva_unidades == "prestacion"),
                  columna_fecha = "fecha_prestacion"
                )
              }
              
             comparar$descriptiva_sumas <- descriptiva_basica_trans(
                data = descriptiva_basica_tabla,
                agrupador = comparar$agrupador,
                frec = FALSE
              )
             
              comparar$descriptiva_frecuencias <- descriptiva_basica_trans(
                data = descriptiva_basica_tabla,
                agrupador = comparar$agrupador,
                suma = FALSE
              )
              
              comparar$diferencias_rips_sumas <- diferencia_valor_rips(
                sumas = comparar$descriptiva_sumas,
                nota_tecnica = comparar$datos,
                porcentaje = FALSE)
    
              comparar$diferencias_rips_percent <- diferencia_valor_rips(
                sumas = comparar$descriptiva_sumas,
                nota_tecnica = comparar$datos,
                porcentaje = TRUE)
    
              comparar$diferencias_cme_sumas <- diferencia_valor_cme(
                frecs = comparar$descriptiva_frecuencias,
                nota_tecnica = comparar$datos,
                porcentaje = FALSE)
    
              comparar$diferencias_cme_percent <- diferencia_valor_cme(
                frecs = comparar$descriptiva_frecuencias,
                nota_tecnica = comparar$datos,
                porcentaje = TRUE)
    
              comparar$totales <- diferencias_totales(
                frecs = comparar$descriptiva_frecuencias,
                sumas = comparar$descriptiva_sumas,
                nota_tecnica = comparar$datos)
              
            },
            error = function(e) {
              print(e)
              sendSweetAlert(
                session = session,
                title = "Error",
                text = e,
                type = "error"
              )
            }
            
          )
          
        }
      })
      
      output$comparar_totales <- DT::renderDataTable({
        if (!is.null(comparar$totales)) {
          DT::datatable(
            comparar$totales[["totales"]],
            class = 'cell-border stripe',
            rownames = FALSE,
            extensions = 'ColReorder',
            selection = 'none',
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15,
              autoWidth = FALSE,
              ordering=F,
              scrollX = TRUE,
              colReorder = TRUE,
              dom = 't')) %>%
            DT::formatStyle(
              columns = 3,
              backgroundColor = styleInterval(
                cuts = c(0),
                values = style_interval)) %>%
            DT::formatStyle(
              columns = 4,
              backgroundColor = styleInterval(
                cuts = c(1),
                values = style_interval)) %>%
            formatCurrency(
              columns = c(2,3),
              mark = ".",
              dec.mark = ",",
              digits = 0) %>%
            formatPercentage(
              columns = 4,
              digits = 0,
              mark = ".",
              dec.mark = ",")
        }
      })
      
      output$comparar_total_mes_rips <- DT::renderDataTable({
        if (!is.null(comparar$totales)) {
          DT::datatable(
            comparar$totales[["total_mes_rips"]], 
            class = 'cell-border stripe', 
            rownames = FALSE, 
            extensions = 'ColReorder',
            selection = 'none',
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15,
              autoWidth = FALSE, 
              ordering=F, 
              scrollX = TRUE, 
              colReorder = TRUE,
              dom = 't',
              scrollY = "50vh", 
              scrollCollapse = TRUE)) %>%
            DT::formatStyle(
              columns = 3, 
              backgroundColor = styleInterval(
                cuts = c(0), 
                values = style_interval)) %>%
            DT::formatStyle(
              columns = 4, 
              backgroundColor = styleInterval(
                cuts = c(1), 
                values = style_interval)) %>%
            formatCurrency(
              columns = c(2,3), 
              mark = ".",
              dec.mark = ",",
              digits = 0) %>%
            formatPercentage(
              columns = 4, 
              digits = 0,
              mark = ".",
              dec.mark = ",")
        }
      })
      
      output$comparar_total_agrup_rips <- DT::renderDataTable({
        if (!is.null(comparar$totales)) {
          DT::datatable(
            comparar$totales[["total_agrupador_rips"]], 
            class = 'cell-border stripe', 
            rownames = FALSE, 
            extensions = 'ColReorder',
            selection = 'none',
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15,
              autoWidth = FALSE,
              ordering=F,
              scrollX = TRUE,
              colReorder = TRUE,
              dom = 't', 
              scrollY = "50vh", 
              scrollCollapse = TRUE)) %>%
            DT::formatStyle(
              columns = 3, 
              backgroundColor = styleInterval(
                cuts = c(0),
                values = style_interval)) %>%
            DT::formatStyle(
              columns = 4, 
              backgroundColor = styleInterval(
                cuts = c(1), 
                values = style_interval)) %>%
            formatCurrency(
              columns = c(2,3),
              mark = ".", 
              dec.mark = ",",
              digits = 0) %>%
            formatPercentage(
              columns = 4,
              digits = 0, 
              mark = ".", 
              dec.mark = ",")
        }
      })
      
      output$comparar_total_mes_cme <- DT::renderDataTable({
        if (!is.null(comparar$totales)) {
          DT::datatable(
            comparar$totales[["total_mes_cme"]], 
            class = 'cell-border stripe', 
            rownames = FALSE, 
            selection = 'none',
            extensions = 'ColReorder',
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15, 
              autoWidth = FALSE, 
              ordering=F,
              scrollX = TRUE, 
              colReorder = TRUE,
              dom = 't',
              scrollY = "50vh",
              scrollCollapse = TRUE)) %>%
            DT::formatStyle(
              columns = 3,
              backgroundColor = styleInterval(
                cuts = c(0), 
                values = style_interval)) %>%
            DT::formatStyle(
              columns = 4,
              backgroundColor = styleInterval(
                cuts = c(1), 
                values = style_interval)) %>%
            formatCurrency(
              columns = c(2,3),
              mark = ".", 
              dec.mark = ",",
              digits = 0) %>%
            formatPercentage(
              columns = 4, 
              digits = 0,
              mark = ".", 
              dec.mark = ",")
        }
      })
      
      output$comparar_total_agrup_cme <- DT::renderDataTable({
        if (!is.null(comparar$totales)) {
          DT::datatable(
            comparar$totales[["total_agrupador_cme"]], 
            class = 'cell-border stripe', 
            rownames = FALSE, 
            extensions = 'ColReorder',
            selection = 'none',
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15,
              autoWidth = FALSE, 
              ordering=F, 
              scrollX = TRUE,
              colReorder = TRUE,
              dom = 't', 
              scrollY = "50vh",
              scrollCollapse = TRUE)) %>%
            DT::formatStyle(
              columns = 3, 
              backgroundColor = styleInterval(
                cuts = c(0), 
                values = style_interval)) %>%
            DT::formatStyle(
              columns = 4, 
              backgroundColor = styleInterval(
                cuts = c(1),
                values = style_interval)) %>%
            formatCurrency(
              columns = c(2,3),
              mark = ".", 
              dec.mark = ",",
              digits = 0) %>%
            formatPercentage(
              columns = 4, 
              digits = 0, 
              mark = ".", 
              dec.mark = ",")
        }
      })
      
      output$comparar_desc_sumas <- DT::renderDataTable({
        if (!is.null(comparar$descriptiva_sumas)) {
          DT::datatable(
            comparar$descriptiva_sumas, 
            class = 'cell-border stripe', 
            rownames = FALSE, 
            extensions = 'ColReorder',
            selection = 'none',
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15, 
              autoWidth = FALSE,
              ordering=T, 
              scrollX = TRUE, 
              colReorder = TRUE,
              scrollY = "50vh", 
              scrollCollapse = TRUE)) %>%
            formatCurrency(
              columns = -c(1), 
              mark = ".",
              dec.mark = ",",
              digits = 0)
        }
      })
      
      output$comparar_desc_frecs <- DT::renderDataTable({
        if (!is.null(comparar$descriptiva_frecuencias)) {
          DT::datatable(
            comparar$descriptiva_frecuencias, 
            class = 'cell-border stripe', 
            rownames = FALSE, 
            extensions = 'ColReorder',
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15, 
              autoWidth = FALSE, 
              ordering=T, 
              scrollX = TRUE,
              colReorder = TRUE,
              scrollY = "50vh", 
              scrollCollapse = TRUE)) %>%
            DT::formatRound(
              columns = -c(1),
              mark = ".",
              dec.mark = ",",
              digits = 0)
        }
      })
      
      output$comparar_diferencias_rips_sumas <- DT::renderDataTable({
        if (!is.null(comparar$diferencias_rips_sumas)) {
          DT::datatable(
            comparar$diferencias_rips_sumas, 
            class = 'cell-border stripe', 
            rownames = FALSE, 
            extensions = 'ColReorder',
            selection = 'none',
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15,
              autoWidth = FALSE,
              ordering=T,
              scrollX = TRUE,
              colReorder = TRUE,
              scrollY = "50vh",
              scrollCollapse = TRUE)) %>%
            DT::formatStyle(
              columns = -c(1),
              backgroundColor = styleInterval(
                cuts = c(0),
                values = style_interval)) %>%
            DT::formatCurrency(
              columns = -c(1),
              mark = ".",
              dec.mark = ",",
              digits = 0)
        }
      })
      
      output$comparar_diferencias_rips_percent <- DT::renderDataTable({
        if (!is.null(comparar$diferencias_rips_percent)) {
          DT::datatable(
            comparar$diferencias_rips_percent, 
            class = 'cell-border stripe', 
            rownames = FALSE, 
            extensions = 'ColReorder',
            selection = 'none',
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15,
              autoWidth = FALSE,
              ordering=T, 
              scrollX = TRUE, 
              colReorder = TRUE,
              scrollY = "50vh", 
              scrollCollapse = TRUE)) %>%
            DT::formatStyle(
              columns = -c(1),
              backgroundColor = styleInterval(
                cuts = c(1),
                values = style_interval)) %>%
            DT::formatPercentage(
              columns = -c(1),
              mark = ".",
              dec.mark = ",",
              digits = 0)
        }
      })
      
      output$comparar_diferencias_cme_sumas <- DT::renderDataTable({
        if (!is.null(comparar$diferencias_cme_sumas)) {
          DT::datatable(
            comparar$diferencias_cme_sumas, 
            class = 'cell-border stripe', 
            rownames = FALSE, 
            extensions = 'ColReorder',
            selection = 'none',
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15,
              autoWidth = FALSE,
              ordering=T, 
              scrollX = TRUE, 
              colReorder = TRUE,
              scrollY = "50vh", 
              scrollCollapse = TRUE)) %>%
            DT::formatStyle(
              columns = -c(1),
              backgroundColor = styleInterval(
                cuts = c(0),
                values = style_interval)) %>%
            DT::formatCurrency(
              columns = -c(1),
              mark = ".",
              dec.mark = ",", 
              digits = 0)
        }
      })
      
      output$comparar_diferencias_cme_percent <- DT::renderDataTable({
        if (!is.null(comparar$diferencias_cme_percent)) {
          DT::datatable(
            comparar$diferencias_cme_percent, 
            class = 'cell-border stripe', 
            rownames = FALSE, 
            extensions = 'ColReorder',
            selection = 'none',
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15,
              autoWidth = FALSE,
              ordering=T, 
              scrollX = TRUE, 
              colReorder = TRUE,
              scrollY = "50vh",
              scrollCollapse = TRUE)) %>%
            DT::formatStyle(
              columns = -c(1),
              backgroundColor = styleInterval(
                cuts = c(1), 
                values = style_interval)) %>%
            DT::formatPercentage(
              columns = -c(1),
              mark = ".", 
              dec.mark = ",",
              digits = 0)
        }
      })
      
      output$comparar_descargar_xlsx <- downloadHandler(
        filename = function() {
          paste0("Seguimiento de ", comparar$select, ".xlsx")
        },
        content = function(file) {
          write_xlsx(
            x = list(
              "Nota tecnica" = comparar$datos,
              "Totales" = comparar$totales[["totales"]],
              "Total mes rips" = comparar$totales[["total_mes_rips"]],
              "Total mes CME" = comparar$totales[["total_mes_cme"]],
              "Total agrupador rips" = comparar$totales[["total_agrupador_rips"]],
              "Total agrupador cme" = comparar$totales[["total_agrupador_cme"]],
              "Sumas" = comparar$descriptiva_sumas,
              "Frecuencias" = comparar$descriptiva_frecuencias,
              "Diferencias RIPS" = comparar$diferencias_rips_sumas,
              "Diferencias RIPS %" = comparar$diferencias_rips_percent,
              "Diferencias CME" = comparar$diferencias_cme_sumas,
              "Diferencias CME %" = comparar$diferencias_cme_percent
            ),
            path = file
          )
        },
        contentType = "xlsx"
      )
      
      }
  )
  
}

