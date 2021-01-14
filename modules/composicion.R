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
        DT::DTOutput(ns("test"))
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
      
      output$test <- DT::renderDT({
        datatable(
          composicion$tabla,
          extensions = 'RowGroup',
          options = list(rowGroup = list(dataSrc = 1),
                         pageLength = nrow(composicion$tabla),
                         orderFixed = c(1, "desc")),
          callback = callback_js,
          selection = 'none'
        ) %>%
          formatPercentage(c("participacion", "participacion_valor")) %>%
          formatCurrency(c("valor_explorar", "valor_calculos"))
      })
      
    }
  )
}