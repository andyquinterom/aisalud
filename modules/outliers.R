outliers_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 3,
        selectizeInput(
          inputId = ns("outliers_cols"),
          label = "Extraer outliers por:",
          choices = c("Ninguno")),
        radioButtons(
          inputId = ns("outliers_modo"),
          label = "Método de cálculo:",
          choiceNames = c("Percentil", "Rango intercuartil"),
          choiceValues = c("percentil", "iqr")
        ),
        uiOutput(ns("outliers_modo_opciones")),
        numericInput(
          inputId = ns("outliers_frecuencia"),
          label = "Frecuencia mínima:",
          min = 0,
          step = 1, 
          value = 0),
        actionButton(
          inputId = ns("outliers_exe"),
          label = "Ejecutar",
          width = "100%"),
        tags$br(),
        tags$br(),
        actionButton(
          inputId = ns("outliers_excluir"),
          label = "Excluir pacientes seleccionados",
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
        column(
          width = 9,
          div(
            DT::dataTableOutput(ns("outliers_tabla")),
              style = "font-size:90%")),
        column(
          width = 3,
          plotOutput(
            height = "550px",
            outputId = ns("outliers_box_plot")))
      )
    )
  )
}

outliers_server <- function(input, output, session, datos, opciones, nombre_id) {
  
  ns <- NS(nombre_id)
  
  outliers <- reactiveValues()
  
  observeEvent(datos$colnames, {
    updateSelectizeInput(
      session = session,
      inputId = "outliers_cols",
      choices = datos$colnames
    )
  })
  
  observeEvent(input$outliers_modo, {
    output$outliers_modo_opciones <- renderUI({
      if (input$outliers_modo == "percentil") {
        sliderInput(
          inputId = ns("outliers_percentil"),
          label = "Outliers mayores que % de los usuarios:",
          min = 0.75,
          max = 0.99,
          value = c(0.9),
          step = 0.01,
          post = "%")
      } else {
        radioButtons(
          inputId = ns("outliers_iqr"),
          "Outliers por IQR",
          choiceNames = list("1.5","3.0"),
          choiceValues = list(1.5, 3.0),
          inline = TRUE,
          width='75%')
      }
    })
  })
  
  observeEvent(input$outliers_excluir, {
    if(!is.null(datos$colnames)) {
      if (length(input$outliers_tabla_rows_selected) > 0) {
      datos$pacientes_excluir <- unname(c(
        datos$pacientes_excluir, 
        unlist(
          outliers$tabla[
            input$outliers_tabla_rows_selected, "NRO_IDENTIFICACION"])
      ))
      datos$pacientes_excluir_exe <- TRUE
      datos$pacientes_excluir_exe <- FALSE
      
      showNotification(
        ui = "Pacientes añadidos a la lista. Por favor aplicar filtros.",
        duration = 10
      )
      
      } else {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Por favor seleccionar al menos un pacient a excluir.",
          type = "error"
        )
      }
    }
  })
  
  observeEvent(input$outliers_exe, {
    if(!is.null(datos$colnames)) {
      if(!is.null(input$outliers_cols) && input$outliers_cols != "Ninguno") {
        if (input$outliers_modo == "percentil") {
          outliers$tabla <- outliers_percentil(
            data =          datos$data_table,
            columna =       input$outliers_cols,
            columna_valor = opciones$valor_costo,
            percentil =     input$outliers_percentil,
            frecuencia =    input$outliers_frecuencia)
          
          output$outliers_tabla <- DT::renderDataTable({
            DT::datatable(
              colnames = c('Valor' = 'VALOR_CALCULOS'),
              outliers$tabla,
              options = list(
                language = list(
                  url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 50, 
                autoWidth = FALSE,
                ordering = T, 
                scrollX = TRUE,
                scrollY = "500px"),
              rownames = FALSE) %>%
              formatCurrency(c('Valor'), mark = ".", dec.mark = ",")
          })
        } else {
          outliers$tabla <- outliers_iqr(
            data =           datos$data_table,
            columna =        input$outliers_cols,
            columna_valor =  opciones$valor_costo,
            multiplicativo = input$outliers_iqr,
            frecuencia =     input$outliers_frecuencia)
          
          output$outliers_tabla <- DT::renderDataTable({
            DT::datatable(
              colnames = c('Valor' = 'VALOR_CALCULOS'),
              outliers$tabla,
              options = list(
                language = list(
                  url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 50, 
                autoWidth = FALSE, 
                ordering = T,
                scrollX = TRUE,
                scrollY = "500px"),
              rownames = FALSE) %>%
              formatCurrency(c('Valor'), mark = ".", dec.mark = ",")
          })
        }
        
        lista_pacientes <- agregar(
          data = datos$data_table,
          columna_valor = opciones$valor_costo,
          columnas = "NRO_IDENTIFICACION",
          columna_suma = "",
          prestaciones = TRUE
        )
        
        output$outliers_box_plot <- renderPlot({
          ggplot(
            data = lista_pacientes,
            aes(y = get(opciones$valor_costo))
          ) +
            geom_boxplot() +
            scale_y_continuous(labels = function(x) number(
              x, big.mark = ".", decimal.mark = ",")) +
            ylab(label = opciones$valor_costo)
        })
        
      }
    }
  })
  
  output$outliers_descargar_csv <- downloadHandler(
    filename = function() {
      paste("Outliers por ",
            opciones$outliers_cols,
            ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        x = outliers$tabla,
        file = file, 
        row.names = FALSE,
        na = "")
    }, 
    contentType = "text/csv"
  )
  
  output$outliers_descargar_csv <- downloadHandler(
    filename = function() {
      paste("Outliers por ",
            opciones$outliers_cols,
            ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(
        x = outliers$tabla,
        path = file)
    }, 
    contentType = "xlsx"
  )
  
}