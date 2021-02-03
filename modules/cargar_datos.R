base_de_datos_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    box(
      style = "min-height: 650px;",
      width = 4,
      tabsetPanel(
        tabPanel(
          title = "Nube",
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
        tabPanel(
          title = "Subir datos",
          fileInput(
            inputId = ns("file"),
            label = "", 
            buttonLabel = "Subir archivo",
            placeholder = "Ningún archivo"),
          radioButtons(
            inputId = ns("file_type"),
            label = "Tipo de archivo",
            inline = TRUE, 
            choices = c("csv", "feather")),
          actionButton(
            inputId = ns("file_options_open"),
            label = "Opciones",
            width = "100%"),
          tags$br(),
          tags$br(),
          dateRangeInput(
            inputId = ns("file_fecha_rango"),
            label = "Fechas:",
            min = NULL, 
            max = NULL, 
            format = "dd/mm/yyyy",
            language = "es"),
          tags$style(HTML(".datepicker {z-index:99999 !important;}")),
          textInput(
            inputId = ns("file_formato_fecha"),
            label = "Formato de Fecha",
            value = "%d/%m/%Y"),
          actionButton(inputId = ns("file_load"), label = "Aplicar",
                       width = "100%"),
          tags$br(),
          tags$br(),
          selectizeInput(
            inputId = ns("file_columna_valor"),
            label = "Columna de valor:",
            width = "100%",
            choices = "valor",
            selected = "valor")
        ),
        tabPanel(
          title = "Perfiles",
          checkboxInput(
            inputId = ns("perfil_enable"),
            label = "Utilizar perfil:"
          ),
          selectizeInput(
            inputId = ns("perfil"),
            width = "100%",
            "Perfil:",
            choices = "Ninguno"
          ),
          aceEditor(
            outputId = ns("perfil_editor"),
            mode = "json",
            value = ""
          ),
          actionButton(
            inputId = ns("perfil_actualizar"),
            label = "Guardar perfiles"
          )
        )
      )
    ),
    box(
      width = 8,
      style = "min-height: 650px;",
      plotlyOutput(outputId = ns("valor_con_tiempo"), height = "630px") %>%
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
            choices = c("Ninguno", tables_query)
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
        if (input$tabla != "Ninguno" &&
            input$tabla != "") {
          opciones$valor_costo <- ifelse(
            test = input$columna_valor != "",
            yes = input$columna_valor,
            no = opciones$valor_costo)
        } else if (opciones$datos_cargados) {
          opciones$valor_costo <- ifelse(
            test = input$file_columna_valor != "",
            yes = input$file_columna_valor,
            no = opciones$valor_costo)
        }
      })
      
      observe({
        if (input$tabla %notin% c("Ninguno", "")) {
          opciones$fecha_rango <- input$fecha_rango
          tabla <- input$tabla
          opciones$colnames <- dbListFields(
            conn,
            tabla)
          opciones$colnames_num <- dbListNumericFields(
            conn,
            tabla)
          fecha_min <- opciones$fecha_rango[1]
          fecha_max <- opciones$fecha_rango[2]
          opciones$tabla_nombre <- tabla
          opciones$tabla_nombre 
          opciones$tabla_original <- conn %>%
            tbl(tabla) %>%
            mutate(fecha_prestacion = as.Date(fecha_prestacion)) %>%
            filter(fecha_prestacion >= fecha_min) %>%
            filter(fecha_prestacion <= fecha_max)
          opciones$tabla <- conn %>%
            tbl(tabla) %>%
            mutate(fecha_prestacion = as.Date(fecha_prestacion)) %>%
            filter(fecha_prestacion >= fecha_min) %>%
            filter(fecha_prestacion <= fecha_max)
        }
      })
      
      observe({
        if (input$tabla != "Ninguno" &&
            input$tabla != "") {
          updateSelectizeInput(
            session = session,
            inputId = "columna_valor",
            choices = opciones$colnames_num,
            selected = "valor"
          )
        } else if (opciones$datos_cargados) {
          updateSelectizeInput(
            session = session,
            inputId = "file_columna_valor",
            choices = opciones$colnames_num,
            selected = "valor"
          )
        }
      })
      
      output$valor_con_tiempo <- renderPlotly({
        if (opciones$datos_cargados) {
            tabla <- opciones$tabla_nombre
            valor_costo <- opciones$valor_costo
            fecha_min <- opciones$fecha_rango[1]
            fecha_max <- opciones$fecha_rango[2]
            datos <- opciones$tabla %>%
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
      
      # Cargar datos locales -----------------
      
      file_opciones <- reactiveValues(
        "value_decimal" = ".",
        "value_delimitador" = ",",
        "value_sheet" = NULL,
        "value_range" = NULL,
        "enabled" = FALSE
      )
      
      observeEvent(input$file_options_open, {
        showModal(
          session = session,
          ui = modalDialog(
            title = "Opciones archivo",
            easyClose = TRUE,
            fade = TRUE,
            datos_opciones_ui(
              id = id,
              file_type = input$file_type,
              value_decimal = file_opciones$value_decimal,
              value_delimitador = file_opciones$value_delimitador,
              value_range = file_opciones$value_range,
              value_sheet = file_opciones$value_sheet,
              value_file = file_opciones$value_file),
            footer = actionButton(
              inputId = ns("file_opciones_guardar"),
              label = "Guardar")
          )
        )
      })
      
      observeEvent(input$datos_opciones_guardar, {
        file_opciones$value_decimal <- input$value_decimal
        file_opciones$value_delimitador <- input$value_delimitador
        file_opciones$value_sheet <- input$value_sheet
        file_opciones$value_range <- input$value_range
        file_opciones$value_file <- input$value_file
        removeModal(session = session)
      })
      
      observeEvent(input$file_load, {
        tryCatch(expr = {
          if (!is.null(input$file)) {
            
            opciones$fecha_rango <- input$file_fecha_rango
            
            value_delimitador <- ifelse(
              test = file_opciones$value_delimitador == "Espacios",
              yes = "\t",
              no = file_opciones$value_delimitador)
            
            if (input$file_type == "csv") {
              datos_read <- readr::read_delim(
                file = input$file$datapath, 
                delim = value_delimitador, 
                locale = locale(
                  decimal_mark = file_opciones$value_decimal))
            } else if (input$file_type == "feather") {
              datos_read <- read_feather(path = input$file$datapath)
            }
            
            opciones$tabla_original <- datos_read %>%
              rename_with(tolower) %>%
              mutate(fecha_prestacion = as.Date(
                fecha_prestacion, format = input$file_formato_fecha)) %>%
              filter(fecha_prestacion >= as.Date(input$file_fecha_rango[1]) &
                       fecha_prestacion <= as.Date(input$file_fecha_rango[2]))
            
            opciones$tabla <- opciones$tabla_original
            
            opciones$colnames <- opciones$tabla %>%
              colnames()
            
            testfor_numeric <- opciones$tabla %>%
              summarise_all(class) == "numeric"
            
            opciones$colnames_num <- opciones$colnames[testfor_numeric]
            
            file_opciones$enabled <- TRUE
            
          }},
          error = function(e) {
            print(e[1])
            sendSweetAlert(
              session = session,
              title = "Error",
              text = e[1],
              type = "error"
            )
          })
      })
      
      
      # Enable tabla
      
      observe({
        if (input$tabla != "Ninguno" &&
            input$tabla != "") {
          opciones$datos_cargados <- TRUE
        } else if (input$tabla == "Ninguno" &&
                   file_opciones$enabled) {
          opciones$datos_cargados <- TRUE
        } else {
          opciones$datos_cargados <- FALSE
        }
      })
      
      # Perfiles ----------------------------------------------------
      
      observe({
        opciones$perfil_raw <- tbl(conn, "perfiles_usuario") %>%
          pull(perfiles)
        
        opciones$perfil_lista <- opciones$perfil_raw %>%
          parse_json(simplifyVector = TRUE)
        
        updateSelectizeInput(
          session = session,
          inputId = "perfil",
          choices = c("Ninguno", names(opciones$perfil_lista))
        )
      })
      
      observe({
        updateAceEditor(
          session = session,
          editorId = "perfil_editor",
          value = opciones$perfil_raw
        )
      })
      
      observe({
        if (input$perfil != "Ninguno" &&
            input$perfil_enable) {
          opciones$perfil_enable <- TRUE
          opciones$perfil_selected <- input$perfil
        } else {
          opciones$perfil_enable <- FALSE
        }
      })
      
      observeEvent(input$perfil_actualizar, {
        perfil_nuevo <- data.frame("perfiles" = input$perfil_editor)
        
        tryCatch(
          expr = {
            parse_json(input$perfil_editor, simplifyVector = TRUE)
            
            dbWriteTable(
              conn = conn,
              Id(schema = "config", table = "perfiles_usuario"),
              perfil_nuevo,
              overwrite = TRUE
            )
            
            opciones$perfil_raw <- tbl(conn, "perfiles_usuario") %>%
              pull(perfiles)
            
            opciones$perfil_lista <- opciones$perfil_raw %>%
              parse_json(simplifyVector = TRUE)
            
            updateSelectizeInput(
              session = session,
              inputId = "perfil",
              choices = c("Ninguno", names(opciones$perfil_lista))
            )
            
          },
          error = function(e) {
            print(e)
            sendSweetAlert(
              session = session,
              title = "Error",
              text = e[1],
              type = "error"
            )
          }
        )
        
      })
      
    }
  )
}

# Funciones --------------------------------------------------------------------

dbListNumericFields <- function(conn, table_name) {
  sql <- "select
       col.column_name
from information_schema.columns col
join information_schema.tables tab on tab.table_schema = col.table_schema
                                   and tab.table_name = col.table_name
                                   and tab.table_type = 'BASE TABLE'
where col.data_type in ('smallint', 'integer', 'bigint', 
                        'decimal', 'numeric', 'real', 'double precision',
                        'smallserial', 'serial', 'bigserial', 'money')
      and col.table_schema not in ('information_schema', 'pg_catalog')
      and col.table_name in (?id)
order by col.table_schema,
         col.table_name,
         col.ordinal_position"
  query <- sqlInterpolate(conn, sql, id = table_name)
  dbGetQuery(conn, str_replace_all(query, "#####", table_name)) %>%
    unlist() %>%
    unname()
}

datos_opciones_ui <- function(
  id, file_type, value_decimal, value_delimitador, value_sheet, value_range,
  value_file) {
  
  if (file_type == "csv") {
    return(
      datos_opciones_csv_ui(
        id = id,
        value_decimal = value_decimal,
        value_delimitador = value_delimitador)
    )
  }
  
  if (file_type == "datos didacticos") {
    return(
      datos_opciones_cloud_ui(
        id = id,
        value_file = value_file
      )
    )
  }
  
}

datos_opciones_csv_ui <- function(id, value_delimitador, value_decimal) {
  ns <- NS(id)
  
  tagList(
    radioButtons(
      inputId = ns("value_delimitador"),
      choices = c(",", ";", "|", "Espacios"),
      label = "Delimitador",
      inline = TRUE,
      selected = value_delimitador
    ),
    
    radioButtons(
      inputId = ns("value_decimal"),
      choiceNames = c("Punto", "Coma"),
      choiceValues = c(".", ","),
      label = "Separador decimal",
      inline = TRUE,
      selected = value_decimal
    )
  )
  
}

datos_opciones_cloud_ui <- function(id, value_file) {
  ns <- NS(id)
  
  tagList(
    selectizeInput(
      inputId = ns("value_file"),
      choices = list.files("datos/saved/"),
      label = "Archivo:",
      selected = value_file
    )
  )
  
}