base_de_datos_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    box(
      style = "min-height: 450px;",
      width = 4,
      tags$br(),
      tags$br(),
      selectizeInput(
        width = "100%",
        inputId = ns("tabla"),
        label = "Seleccionar datos",
        choices = "Ninguno"
      ),
      selectizeInput(
        inputId = ns("columna_valor"),
        label = "Columna de valor:",
        width = "100%",
        choices = "valor",
        selected = "valor"
      ),
      dateRangeInput(
        inputId = ns("fecha_rango"),
        label = "Fechas:",
        min = NULL, 
        max = NULL, 
        format = "dd/mm/yyyy",
        language = "es"),
      tags$style(HTML(".datepicker {z-index:99999 !important;}"))
    ),
    box(
      width = 12,
      height = "300px",
      plotlyOutput(outputId = ns("valor_con_tiempo"), height = "280px") %>%
        withSpinner()
    )
  )
}

base_de_datos_server <- function(id, opciones, conn) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      ns <- NS(id)
      
      base_de_datos <- reactiveValues()
      
      prepara_opciones <- reactiveValues()
      
      observe({
        
        tables_query <- dbGetQuery(
          conn,
          paste0("SELECT table_name FROM information_schema.tables
       WHERE table_schema='", 
                 Sys.getenv("DATABASE_SCHEMA"), "'")) %>%
          unlist() %>%
          unname()
        if (identical(character(0), tables_query)) {
          tables_query <- NULL
          updateSelectizeInput(
            session = session,
            inputId = "tabla",
            choices = "Ninguno"
          )
        } else {
          updateSelectizeInput(
            session = session,
            inputId = "tabla",
            choices = tables_query
          )
        }
      })
      
      observeEvent(input$tabla, {
        if (!is.null(conn) && 
            input$tabla != "Ninguno" &&
            input$tabla != "") {
          prepara_opciones$colnames <- dbListFields(
            conn,
            input$tabla)
          updateSelectizeInput(
            session = session,
            inputId = "columna_valor",
            selected = opciones$valor_costo,
            choices = prepara_opciones$colnames
          )
        }
      })
      
      observe({
        if (input$tabla != "Ninguno") {
          opciones$fecha_rango <- input$fecha_rango
          opciones$valor_costo <- ifelse(
            test = input$columna_valor != "",
            yes = input$columna_valor,
            no = opciones$valor_costo)
          opciones$tabla <- input$tabla
          opciones$colnames <- dbListFields(
            conn,
            opciones$tabla)
        }
      })
      
      observeEvent(input$valor_a_numerico, {
        if (input$valor_a_numerico) {
          opciones$data_table[, "valor" := numerize(valor)]
          opciones$data_original[, "valor" := numerize(valor)]
        }
        columnas_num <- unlist(lapply(opciones$data_table[1,], is.numeric))
        opciones$colnames_num <- opciones$colnames[columnas_num]
        opciones$valor_costo <- "valor"
      })
      
      observeEvent(opciones$colnames, {
        updateSelectizeInput(
          session = session,
          inputId = "columna_valor",
          choices = opciones$colnames_num,
          selected = "valor"
        )
      })
      
      output$valor_con_tiempo <- renderPlotly({
        if (input$tabla != "Ninguno") {
            tabla <- input$tabla
            valor_costo <- opciones$valor_costo
            fecha_min <- opciones$fecha_rango[1]
            fecha_max <- opciones$fecha_rango[2]
            datos <- conn %>%
              tbl(tabla) %>%
              mutate(fecha_prestacion = as.Date(fecha_prestacion)) %>%
              filter(fecha_prestacion >= fecha_min) %>%
              filter(fecha_prestacion <= fecha_max) %>%
              mutate(
                mes_temporal = year(fecha_prestacion)*100 +
                  month(fecha_prestacion)) %>%
              group_by(mes_temporal) %>%
              summarise(suma = sum(
                as.numeric(!!as.name(valor_costo)), na.rm = TRUE)) %>%
              collect() %>%
              mutate(mes_label_temporal = paste(
                sep = " - ",
                substr(mes_temporal, 1, 4),
                mes_spanish(
                as.numeric(substr(mes_temporal, 5, 6))))) %>%
              arrange(mes_temporal) %>%
              plot_ly(x = ~mes_label_temporal,
                      y = ~suma,
                      type = "bar") %>%
              config(locale = "es") %>%
              layout(
                title = "Suma del valor a mes",
                xaxis = list(
                  title = "Mes",
                  categoryorder = "array",
                  categoryarray = ~mes_temporal),
                yaxis = list(title = "Suma",
                             tickformat = ",.2f"))
        }
      })
    }
  )
}

# Funciones --------------------------------------------------------------------

