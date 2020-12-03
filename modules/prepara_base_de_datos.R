base_de_datos_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    box(
      style = "min-height: 450px;",
      width = 4,
      actionButton(
        inputId = ns("establecer_conexion"),
        label = "Establecer conexión",
        width = "100%"
      ),
      tags$br(),
      tags$br(),
      selectizeInput(
        width = "100%",
        inputId = ns("tabla"),
        label = "Seleccionar datos",
        choices = "Ninguno"
      ),
      selectizeInput(
        width = "100%",
        inputId = ns("tabla_columnas"),
        label = "Columnas",
        choices = "Ninguno",
        multiple = TRUE
      ),
      checkboxInput(
        inputId = ns("tabla_columnas_todos"),
        label = "Cargar todas las columnas",
        value = TRUE
      ),
      dateRangeInput(
        inputId = ns("fecha_rango"),
        label = "Fechas:",
        min = NULL, 
        max = NULL, 
        format = "dd/mm/yyyy",
        language = "es"),
      tags$style(HTML(".datepicker {z-index:99999 !important;}")),
      textInput(
        inputId = ns("formato_fecha"),
        label = "Formato de Fecha",
        value = "%d/%m/%Y"),
      actionButton(inputId = ns("file_load"), label = "Aplicar")
    ),
    box(
      width = 3,
      style = "min-height: 450px;",
      selectizeInput(
        inputId = ns("columna_valor"),
        label = "Columna de valor:",
        width = "100%",
        choices = "valor",
        selected = "valor"
      ),
      actionButton(
        inputId = ns("columna_valor_cambiar"),
        label = "Cambiar"
      )
    ),
    box(
      width = 5,
      style = "min-height: 450px;",
      tags$h3("Prevista:"),
      DT::dataTableOutput(
        outputId = ns("preview"),
        width = "100%")
    ),
    box(
      width = 12,
      height = "300px",
      plotOutput(outputId = ns("valor_con_tiempo"), height = "280px") %>%
        withSpinner()
    )
  )
}

base_de_datos_server <- function(input, output, session, opciones, nombre_id,
                                 datos) {
  
  base_de_datos_con <- NULL
  
  session$onSessionEnded(function() {
    dbDisconnect(base_de_datos_con)
  })
  
  id <- nombre_id
  ns <- NS(id)
  
  base_de_datos <- reactiveValues()
  
  prepara_opciones <- reactiveValues()
  
  observeEvent(input$establecer_conexion, {
    withProgress(
      message = "Estableciendo conexión con base de datos...", {
        tryCatch(
          expr = {
            if (is.null(base_de_datos_con)) {
              base_de_datos_con <<- dbConnect(
                RPostgres::Postgres(),
                dbname = Sys.getenv("DATABASE_NAME"),
                user = Sys.getenv("DATABASE_USER"),
                password = Sys.getenv("DATABASE_PW"),
                host = Sys.getenv("DATABASE_HOST"),
                port = Sys.getenv("DATABASE_PORT"),
                sslmode = "require",
                options = paste0("-c search_path=", Sys.getenv("DATABASE_SCHEMA")))
            }
            base_de_datos$schema <- Sys.getenv("DATABASE_SCHEMA")
            tables_query <- dbGetQuery(
              base_de_datos_con,
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
          }
        )
    })
  })
  
  observeEvent(input$tabla, {
    if (!is.null(base_de_datos_con) && input$tabla != "Ninguno") {
      prepara_opciones$colnames <- dbListFields(
        base_de_datos_con,
        input$tabla)
      updateSelectizeInput(
        session = session,
        inputId = "tabla_columnas",
        choices = prepara_opciones$colnames
      )
    }
  })
  
  observeEvent(input$file_load, {
    tryCatch(
      expr = {
        withProgress(
          message = "Cargando datos...", {
            if (!is.null(base_de_datos_con) && input$tabla != "Ninguno") {
              if (input$tabla_columnas_todos) {
                datos$data_original <- as.data.table(
                  tbl(base_de_datos_con, input$tabla))
              } else {
                datos$data_original <- as.data.table(
                  tbl(base_de_datos_con, input$tabla) %>%
                    select(!!input$tabla_columnas)
                )
              }
              setnames(
                datos$data_original,
                tolower(colnames(datos$data_original)))
              datos$data_original[, "fecha_prestacion" := as.Date(
                fecha_prestacion, 
                format = input$formato_fecha)]
              datos$data_original <- datos$data_original[
                fecha_prestacion >= as.Date(input$fecha_rango[1]) &
                  fecha_prestacion <= as.Date(input$fecha_rango[2])]
              datos$data_table <- datos$data_original
              datos$valores_unicos <- lapply(datos$data_table, unique)
              datos$colnames <- colnames(datos$data_table)
              columnas_num <- unlist(lapply(datos$data_table[1,], is.numeric))
              datos$colnames_num <- datos$colnames[columnas_num]
            }
          }
        )
      },
      error = function(e) {
        print(e[1])
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Por favor revisar las columnas seleccionadas.",
          type = "error"
        )
      }
    )
  })
  
  observeEvent(datos$colnames, {
    updateSelectizeInput(
      session = session,
      inputId = "columna_valor",
      choices = datos$colnames_num,
      selected = "valor"
    )
  })
  
  observeEvent(input$columna_valor_cambiar, {
    if (!is.null(datos$colnames)) {
      opciones$valor_costo <- input$columna_valor
    }
  })
  
  output$valor_con_tiempo <- renderPlot({
    if (!is.null(datos$colnames)) {
      if (opciones$valor_costo %in% datos$colnames) {
        ggplot(data = datos$data_table, 
               aes(cut(fecha_prestacion, "1 month"), 
                   get(opciones$valor_costo))) +
          geom_col() +
          xlab("Fecha") +
          ylab(opciones$valor_costo) +
          scale_x_discrete(labels = function(x) mes_spanish(month(x))) +
          scale_y_continuous(labels = formatAsCurrency)
      }
    }
  })
  
  output$preview <- DT::renderDataTable({
    if (is.null(datos$colnames)) {
      data.table()
    } else {
      tryCatch(
        expr = {
          columnas <- intersect(
            x = c(
              "nro_identificacion",
              "fecha_prestacion",
              "valor"),
            y = names(datos$data_original[1])
          )
          DT::datatable(
            data = datos$data_original[
              1:5,
              columnas,
              with = FALSE],
            rownames = FALSE,
            options = list(
              columnDefs = list(
                list(
                  className = 'dt-center',
                  targets = "_all")),
              dom = 't',
              pageLength = 5,
              ordering = FALSE,
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
            )) %>%
            DT::formatStyle(
              columns = 1:length(columnas),
              valueColumns = 1,
              backgroundColor = "white")
        },
        error = function(e) {
          print(e[1])
          sendSweetAlert(
            session = session,
            title = "Error",
            text = e[1],
            type = "error"
          )
        }
      )
    }
  })
  
  return(datos)
  
}

# Funciones --------------------------------------------------------------------

