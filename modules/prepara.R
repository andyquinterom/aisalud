prepara_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    box(
      width = 12,
      fileInput(
        inputId = ns("file"),
        label = "", 
        buttonLabel = "Subir archivo",
        placeholder = "Ningún archivo"),
      radioButtons(
        inputId = ns("file_type"),
        label = "Tipo de archivo",
        inline = TRUE, 
        choices = c("feather", "csv", "xlsx")),
      actionButton(
        inputId = ns("file_options_open"),
        label = "Opciones")
      ),
    box(
      width = 12,
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
      width = 12,
      fluidRow(
        column(
          width = 2,
          tags$h3("Preview:")),
        column(
          width = 10,
          br(),
          tags$a(
            "Si se genera un error, el archivo no es feather o tu base de datos no contiene las columnas: NRO_IDENTIFICACION, FECHA_PRESTACION, VALOR o COSTO.",
            style = "color: black;"))),
      DT::dataTableOutput(
        outputId = ns("preview"),
        width = "100%")
      )
    )
}

prepara_server <- function(input, output, session, nombre_id) {
  
  id <- nombre_id
  ns <- NS(id)
  
  opciones <- reactiveValues(
    "value_decimal" = ".",
    "value_delimitador" = ",",
    "value_sheet" = NULL,
    "value_range" = NULL
  )
  
  datos <- reactiveValues(
    "data_table" = data.table(),
    "data_original" = data.table(),
    "colnames" = NULL
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
          value_decimal = opciones$value_decimal,
          value_delimitador = opciones$value_delimitador,
          value_range = opciones$value_range,
          value_sheet = opciones$value_sheet),
        footer = actionButton(
          inputId = ns("datos_opciones_guardar"),
          label = "Guardar")
      )
    )
  })
  
  observeEvent(input$datos_opciones_guardar, {
    opciones$value_decimal <- input$value_decimal
    opciones$value_delimitador <- input$value_delimitador
    opciones$value_sheet <- input$value_sheet
    opciones$value_range <- input$value_range
    removeModal(session = session)
  })
  
  observeEvent(input$file_load, {
    if (!is.null(input$file)) {
      if (input$file_type == "csv") {
        datos$data_original <- fread(
          input = input$file$datapath, 
          sep = opciones$value_delimitador, 
          dec = opciones$value_decimal,
          data.table = TRUE)
        datos$data_original[, "FECHA_PRESTACION" := as.Date(
          FECHA_PRESTACION, 
          format = input$formato_fecha)]
        datos$data_original <- datos$data_original[
          FECHA_PRESTACION >= as.Date(input$fecha_rango[1]) &
            FECHA_PRESTACION <= as.Date(input$fecha_rango[2])]
        datos$data_table <- datos$data_original
        datos$valores_unicos <- lapply(datos$data_table, unique)
        datos$colnames <- colnames(datos$data_table)
      } 
      if (input$file_type == "feather") {
        datos$data_original <- as.data.table(
          read_feather(
            path = input$file$datapath)
        )
        datos$data_original[, "FECHA_PRESTACION" := as.Date(
          FECHA_PRESTACION, 
          format = input$formato_fecha)]
        datos$data_original <- datos$data_original[
          FECHA_PRESTACION >= as.Date(input$fecha_rango[1]) &
            FECHA_PRESTACION <= as.Date(input$fecha_rango[2])]
        datos$data_table <- datos$data_original
        datos$valores_unicos <- lapply(datos$data_table, unique)
        datos$colnames <- colnames(datos$data_table)
      }
      if (input$file_type == "xlsx") {
        datos$data_original <- as.data.table(
          read_excel(
            path = input$file$datapath, 
            sheet = opciones$value_sheet, 
            range = opciones$value_range)
        )
        datos$data_original[, "FECHA_PRESTACION" := as.Date(
          FECHA_PRESTACION, 
          format = input$formato_fecha)]
        datos$data_original <- datos$data_original[
          FECHA_PRESTACION >= as.Date(input$fecha_rango[1]) &
            FECHA_PRESTACION <= as.Date(input$fecha_rango[2])]
        datos$data_table <- datos$data_original
        datos$valores_unicos <- lapply(datos$data_table, unique)
        datos$colnames <- colnames(datos$data_table)
      }
    }
  })
  
  output$preview <- DT::renderDataTable({
    if(is.null(datos$colnames)) {
      data.table()
    } else {
      tryCatch(
        expr = {
          columnas <- intersect(
            x = c(
              "NRO_IDENTIFICACION",
              "FECHA_PRESTACION",
              "VALOR",
              "COSTO"),
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

datos_opciones_ui <- function(
  id, file_type, value_decimal, value_delimitador, value_sheet, value_range) {
  
  if (file_type == "csv") {
    return(
      datos_opciones_csv_ui(
        id = id,
        value_decimal = value_decimal,
        value_delimitador = value_delimitador)
    )
  }
  
  if (file_type == "xlsx") {
    return(
      datos_opciones_xlsx_ui(
        id = id,
        value_sheet = value_sheet,
        value_range = value_range
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

datos_opciones_xlsx_ui <- function(id, value_sheet, value_range) {
  ns <- NS(id)
  
  tagList(
    textInput(
      inputId = ns("value_sheet"),
      label = "Nombre de la hoja",
      placeholder = "Sheet1",
      value = value_sheet
    ),
    
    textInput(
      inputId = ns("value_range"),
      label = "Rango",
      placeholder = "A1:A1",
      value = value_range
    ),
  )
  
}