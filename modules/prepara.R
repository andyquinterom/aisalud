prepara_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    box(
      width = 5,
      height = "240px",
      fileInput(
        inputId = ns("file"),
        label = "", 
        buttonLabel = "Subir archivo",
        placeholder = "Ningún archivo"),
      radioButtons(
        inputId = ns("file_type"),
        label = "Tipo de archivo",
        inline = TRUE, 
        choices = c("feather", "csv", "datos didacticos")),
      actionButton(
        inputId = ns("file_options_open"),
        label = "Opciones")
      ),
    box(
      width = 3,
      height = "240px",
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
      width = 4,
      height = "240px",
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
      width = 12,
      fluidRow(
        column(
          width = 2,
          tags$h3("Prevista:")),
        column(
          width = 10,
          br(),
          tags$a(
            "Si se genera un error, el archivo no es feather o tu base de datos no contiene las columnas: nro_identificacion, fecha_prestacion o valor.",
            style = "color: black;"))),
      DT::dataTableOutput(
        outputId = ns("preview"),
        width = "100%")
      ),
    box(
      width = 12,
      height = "300px",
      plotOutput(outputId = ns("valor_con_tiempo"), height = "280px")
    )
    )
}

prepara_server <- function(input, output, session, opciones, nombre_id) {
  
  id <- nombre_id
  ns <- NS(id)
  
  opciones_prepara <- reactiveValues(
    "value_decimal" = ".",
    "value_delimitador" = ",",
    "value_sheet" = NULL,
    "value_range" = NULL
  )
  
  datos <- reactiveValues(
    "data_table" = data.table(),
    "data_original" = data.table(),
    "colnames" = NULL,
    "pacientes_excluir" = "Ninguno"
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
          value_decimal = opciones_prepara$value_decimal,
          value_delimitador = opciones_prepara$value_delimitador,
          value_range = opciones_prepara$value_range,
          value_sheet = opciones_prepara$value_sheet,
          value_file = opciones_prepara$value_file),
        footer = actionButton(
          inputId = ns("datos_opciones_guardar"),
          label = "Guardar")
      )
    )
  })
  
  observeEvent(input$datos_opciones_guardar, {
    opciones_prepara$value_decimal <- input$value_decimal
    opciones_prepara$value_delimitador <- input$value_delimitador
    opciones_prepara$value_sheet <- input$value_sheet
    opciones_prepara$value_range <- input$value_range
    opciones_prepara$value_file <- input$value_file
    removeModal(session = session)
  })
  
  observeEvent(input$file_load, {
    if (input$file_type == "datos didacticos" &&
        !is.null(opciones_prepara$value_file)) {
      datos$data_original <- as.data.table(
        read_feather(
          path = paste0("datos/saved/", opciones_prepara$value_file))
      )
      setnames(datos$data_original, tolower(colnames(datos$data_original)))
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
    if (!is.null(input$file)) {
      tryCatch(
        expr = {
          value_delimitador <- ifelse(
            test = opciones_prepara$value_delimitador == "Espacios",
            yes = "\t",
            no = opciones_prepara$value_delimitador
          )
          if (input$file_type == "csv") {
            datos$data_original <- fread(
              input = input$file$datapath, 
              sep = value_delimitador, 
              dec = opciones_prepara$value_decimal,
              data.table = TRUE)
            setnames(datos$data_original, tolower(colnames(datos$data_original)))
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
          if (input$file_type == "feather") {
            datos$data_original <- as.data.table(
              read_feather(
                path = input$file$datapath)
            )
            setnames(datos$data_original, tolower(colnames(datos$data_original)))
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
        ggplot(data = datos$data_original, 
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