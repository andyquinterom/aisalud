prepara_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    box(
      width = 5,
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
        label = "Opciones")),
    fluidRow(
      box(
        width = 2,
        dateRangeInput(
          inputId = ns("fecha_min"),
          label = "Fecha Inicial:",
          min = NULL, 
          max = NULL, 
          format = "dd/mm/yyyy",
          language = "es"),
        tags$style(HTML(".datepicker {z-index:99999 !important;}")),
        textInput(
          inputId = ns("formato_fecha"),
          label = "Formato de Fecha",
          value = "%d/%m/%Y"),
        actionButton(inputId = ns("ejecutar_opciones"), label = "Aplicar")
        ),
    br(),
    fluidRow(
      column(
        width = 5,
        tags$div()),
      column(
        width = 6,
        box(
          width = "100%",
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
      )
    )
  )
}

prepara_server <- function(input, output, session, nombre_id) {
  
  id <- nombre_id
  
  opciones <- reactiveValues(
    "value_decimal" = ".",
    "value_delimitador" = ",",
    "value_sheet" = NULL,
    "value_range" = NULL
  )
  
  datos <- reactiveValues(
    "data_table" = data.table()
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
        footer = datos_opciones_guardar(id = id)
      )
    )
  })
  
  observeEvent(input$datos_opciones_guardar, {
    opciones$value_decimal <- input$value_decimal
    opciones$value_delimitador <- input$value_delimitador
    opciones$value_sheet <- input$value_sheet
    opciones$value_range <- input$value_range
  })
  
  observeEvent(input$file_load, {
    if (!is.null(input$file)) {
      if (input$file_type == "csv") {
        datos$data_table <- fread(
          input = input$file$datapath, 
          sep = opciones$value_delimitador, 
          dec = opciones$value_decimal,
          data.table = TRUE)
      } 
      if (input$file_type == "feather") {
        datos$data_table <- as.data.table(
          read_feather(
            path = input$file$datapath)
        )
      }
      if (input$file_type == "xlsx") {
        datos$data_table <- as.data.table(
          read_excel(
            path = input$file$datapath, 
            sheet = opciones$value_sheet, 
            range = opciones$value_range)
        )
      }
    }
  })
  
  return(datos)
  
}