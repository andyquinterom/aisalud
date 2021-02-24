composicion_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 3,
        selectizeInput(
          inputId = ns("composicion_suma_valor"),
          label = "Sumar valor por:",
          choices = NULL,
          multiple = FALSE, 
          width = "100%"),
        selectizeInput(
          inputId = ns("composicion_episodios"),
          label = "Episodios:",
          choices = NULL,
          multiple = FALSE, 
          width = "100%"),
        selectizeInput(
          inputId = ns("composicion_episodios_agrupadores"),
          label = "Agrupadores:",
          choices = NULL,
          multiple = TRUE, 
          width = "100%"),
        selectizeInput(
          inputId = ns("composicion_grupos"),
          label = "Columna a explorar:",
          choices = NULL,
          width = "100%"),
        actionButton(
          inputId = ns("composicion_ejecutar"),
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

composicion_server <- function(id, opciones, conn) {
  ns <- NS(id)
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      composicion <- reactiveValues(agrupadores = c(), tabla = data.frame())
      
      observeEvent(opciones$colnames, {
        updateSelectizeInput(
          session = session,
          inputId = "composicion_suma_valor",
          choices = opciones$colnames,
          selected = "nro_identificacion")
        updateSelectizeInput(
          session = session,
          inputId = "composicion_episodios",
          choices = opciones$colnames)
        updateSelectizeInput(
          session = session,
          inputId = "composicion_grupos",
          choices = opciones$colnames)
      })
      
      observe({
        if (input$composicion_episodios != "" &&
            opciones$datos_cargados &&
            input$composicion_episodios %in% opciones$colnames &&
            input$composicion_grupos != "") {
          if (!identical(composicion$agrupadores,
                         input$composicion_episodios_agrupadores) ||
              is.null(composicion$agrupadores)) {
            updateSelectizeInput(
              session = session,
              inputId = "composicion_episodios_agrupadores",
              server = TRUE,
              selected = composicion$agrupadores,
              choices = {
                opciones$tabla %>%
                  select(!!as.name(input$composicion_episodios)) %>%
                  distinct() %>%
                  pull(!!as.name(input$composicion_episodios))
              }
            )
          }
        }
      })
      
      observe({
        composicion$agrupadores <- input$composicion_episodios_agrupadores
      })
      
      observeEvent(input$composicion_ejecutar, {
        tryCatch(
          expr = {
            if (input$composicion_episodios != "" &&
                opciones$datos_cargados &&
                input$composicion_episodios %in% opciones$colnames &&
                !is.null(input$composicion_episodios_agrupadores) &&
                input$composicion_grupos != "") {
              composicion$tabla <- datos_composicion(
                data = opciones$tabla,
                columna_episodios = input$composicion_episodios,
                columna_valor = opciones$valor_costo,
                columna_suma = input$composicion_suma_valor,
                columna_explorar = input$composicion_grupos,
                prioridad = input$composicion_episodios_agrupadores
              ) %>%
                collect() %>%
                mutate(participacion_en_episodios = 
                         participacion_en_episodios/100,
                       participacion_valor = participacion_valor/100)
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
          })
      })
      
      callback_js <- JS(
        "table.on('click', 'tr.dtrg-group', function () {",
        "  var rowsCollapse = $(this).nextUntil('.dtrg-group');",
        "  $(rowsCollapse).toggleClass('hidden');",
        "});"
      )
      
      output$tabla_composicion <- DT::renderDT({
        
        if (nrow(composicion$tabla) > 0) {
          style_color_participacion_valor <- styleColorBar(
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
            extensions = c('FixedColumns'),
            options = list(pageLength = nrow(composicion$tabla),
                           orderFixed = c(0, "desc"),
                           scrollY = "600px",
                           scrollX = TRUE,
                           fixedColumns = list(leftColumns = 5)),
            selection = 'none'
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
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>%
            formatStyle(
              c("% del valor total"),
              background = style_color_participacion_valor,
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center')
        } else {
          data.frame()
        }
        
      })
      
      output$composicion_descargar_csv <- downloadHandler(
        filename = function() {
          paste("Composicion",
                ".csv", sep="")
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
                "Valor medio a explorar de registro" = "media_explorar_registro",
                "Valor medio a explorar por episodio" = "media_explorar_episodio",
                "Valor medio de episodio" = "media_episodio"
              ),
            file = file, 
            row.names = FALSE,
            na="")
        }, 
        contentType = "text/csv"
      )
      
      output$composicion_descargar_xlsx <- downloadHandler(
        filename = function() {
          paste("Composicion",
                ".xlsx", sep="")
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
                "Valor medio a explorar de registro" = "media_explorar_registro",
                "Valor medio a explorar por episodio" = "media_explorar_episodio",
                "Valor medio de episodio" = "media_episodio"
              ),
            path = file)
        }, 
        contentType = "xlsx"
      )
        
    }
  )
}