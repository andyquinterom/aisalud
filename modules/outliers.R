outliers_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      width = 3,
      pickerInput(
        inputId = ns("outliers_cols"),
        label = "Agrupar por:",
        choices = c("NA"),
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE)),
      sliderInput(
        inputId = ns("outliers_percentil"),
        label = "Outliers por percentil:",
        min = 0.75, 
        max = 0.99, 
        value = c(0.9)),
      radioButtons(
        inputId = ns("outliers_iqr"),
        "Outliers por IQR",
        choiceNames = list("1.5","3.0"),
        choiceValues = list(1.5, 3.0),
        inline = TRUE,
        width='75%'),
      numericInput(
        inputId = ns("outliers_frecuencia"),
        label = "Frecuencia Mínima",
        min = 0,
        step = 1, 
        value = 0),
      actionButton(
        inputId = ns("outliers_percentil_exe"),
        label = "Calcular por percentil",
        width = "100%"),
      actionButton(
        inputId = ns("outliers_iqr_exe"),
        label = "Calcular por IQR",
        width = "100%"),
      tags$br(),
      tags$br(),
      downloadButton(
        outputId = ns("outliers_descargar_csv"),
        label = "CSV", 
        style = "width:100%;"),
      tags$br(),
      tags$br(),
      downloadButton(
        outputId = ns("outliers_descargar_xlsx"),
        label = "Excel",
        style = "width:100%;")),
    box(
      width = 9,
      div(DT::dataTableOutput(ns("outliers_tabla")),
          style = "font-size:90%"))
  )
}

outliers_server <- function(input, output, session, datos, opciones) {
  
  outliers <- reactiveValues()
  
  observeEvent(datos$colnames, {
    updatePickerInput(
      session = session,
      inputId = "outliers_cols",
      choices = datos$colnames
    )
  })
  
  observeEvent(input$outliers_percentil_exe, {
    if(!is.null(datos$colnames)) {
      if(!is.null(input$outliers_cols) && input$outliers_cols != "NA") {
        opciones$outliers_cols <- input$outliers_cols
        outliers$tabla <- outliers_percentil(
          data =          datos$data_table,
          columna =       opciones$outliers_cols,
          columna_valor = opciones$valor_costo,
          percentil =     input$outliers_percentil,
          frecuencia =    input$outliers_frecuencia)
        
        output$outliers_tabla <- DT::renderDataTable({
          DT::datatable(
            outliers$tabla,
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15, 
              autoWidth = FALSE,
              ordering=T, 
              scrollX = TRUE),
            rownames= FALSE) %>%
            formatCurrency(c('VALOR'), mark = ".", dec.mark = ",")
        })   
      }
    }
  })
  
  observeEvent(input$outliers_iqr_exe, {
    if(!is.null(datos$colnames)) {
      if(!is.null(input$outliers_cols) && input$outliers_cols != "NA") {
        opciones$outliers_cols <- input$outliers_cols
        outliers$tabla <- outliers_iqr(
          data =           datos$data_table,
          columna =        opciones$outliers_cols,
          columna_valor =  opciones$valor_costo,
          multiplicativo = input$outliers_iqr,
          frecuencia =     input$outliers_frecuencia)
        
        output$outliers_tabla <- DT::renderDataTable({
          DT::datatable(
            outliers$tabla,
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15, 
              autoWidth = FALSE, 
              ordering=T,
              scrollX = TRUE),
            rownames= FALSE) %>%
            formatCurrency(c('VALOR'), mark = ".", dec.mark = ",")
        })   
      }
    }
  })
  
  output$outliers_descargar_csv <- downloadHandler(
    filename = function() {
      paste("Outliers por ",
            opciones$outliers_cols,
            ".csv", sep="")
    },
    content = function(file) {
      write.csv(
        x = outliers$tabla,
        file = file, 
        row.names = FALSE,
        na="")
    }, 
    contentType = "text/csv"
  )
  
  output$outliers_descargar_csv <- downloadHandler(
    filename = function() {
      paste("Outliers por ",
            opciones$outliers_cols,
            ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(
        x = outliers$tabla,
        path = file)
    }, 
    contentType = "xlsx"
  )
  
}