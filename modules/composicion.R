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
          multiple = TRUE, 
          width = "100%"),
        actionButton(
          inputId = ns("composicion_ejecutar"),
          label = "Ejecutar",
          width = "100%"
        )
        ),
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
      
      composicion <- reactiveValues(agrupadores = c())
      
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
            opciones$tabla_nombre != "Ninguno" &&
            input$composicion_episodios %in% opciones$colnames) {
          updateSelectizeInput(
            session = session,
            inputId = "composicion_episodios_agrupadores",
            server = TRUE,
            choices = {
              opciones$tabla %>%
                select(!!as.name(input$composicion_episodios)) %>%
                distinct() %>%
                collect() %>%
                unname() %>%
                unlist()
            }
          )
        }
      })
      
      observeEvent(input$composicion_ejecutar, {
        tryCatch(
          expr = {
            if (input$composicion_episodios != "" &&
                opciones$tabla_nombre != "Ninguno" &&
                input$composicion_episodios %in% opciones$colnames &&
                !is.null(input$composicion_episodios_agrupadores)) {
              composicion$tabla <- datos_composicion(
                data = opciones$tabla,
                columna_episodios = input$composicion_episodios,
                columna_valor = opciones$valor_costo,
                columna_suma = input$composicion_suma_valor,
                columna_explorar = input$composicion_grupos,
                prioridad = input$composicion_episodios_agrupadores
              ) %>%
                collect() %>%
                mutate(participacion = participacion/100,
                       participacion_valor = participacion_valor/100)
            }
          })
      })
      
      callback_js <- JS(
        "table.on('click', 'tr.dtrg-group', function () {",
        "  var rowsCollapse = $(this).nextUntil('.dtrg-group');",
        "  $(rowsCollapse).toggleClass('hidden');",
        "});"
      )
      
      output$tabla_composicion <- DT::renderDT({
        style_color_participacion_valor <- styleColorBar(
          data = composicion$tabla$participacion_valor,
          color = "#87CEEB")
        
        style_color_participacion <- styleColorBar(
          data = composicion$tabla$participacion,
          color = "#87CEEB")
        
        datatable(
          composicion$tabla,
          rownames = FALSE,
          colnames = c(
            "Incluida en episodios:" = "count",
            "Número de episodios:" = "n_episodios",
            "% de participación:" = "participacion",
            "Suma de valor del agrupador:" = "valor_explorar",
            "Suma de valor de los episodios" = "valor_calculos",
            "% del valor total" = "participacion_valor"),
          extensions = c('RowGroup', 'FixedColumns'),
          options = list(rowGroup = list(dataSrc = 0),
                         pageLength = nrow(composicion$tabla),
                         orderFixed = c(0, "desc"),
                         scrollY = "700px",
                         fixedColumns = list(leftColumns = 2)),
          callback = callback_js,
          selection = 'none'
        ) %>%
          formatPercentage(c("% de participación:",
                             "% del valor total")) %>%
          formatCurrency(c("Suma de valor del agrupador:",
                           "Suma de valor de los episodios")) %>%
          formatStyle(
            c("% de participación:"),
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
      })
      
    }
  )
}