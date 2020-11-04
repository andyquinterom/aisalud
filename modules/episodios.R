episodios_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 3,
        box(width = "100%",
            pickerInput(
              inputId = ns("episodios_col_valor"),
              label = "Sumar valor por:",
              choices = c("NA"),
              multiple = FALSE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE)),
            pickerInput(
              inputId = ns("episodios_cols"),
              label = "Agrupar por:",
              choices = c("NA"),
              multiple = FALSE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE)),
            pickerInput(
              inputId = ns("episodios_cols_sep"),
              label = "Separar por:",
              choices = c("NA"),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                `select-all-text` = "Seleccionar todos",
                `deselect-all-text` = "Deseleccionar todos")),
            uiOutput(
              outputId = ns("episodios_jerarquia")
            ),
            tags$br(),
            actionButton(ns("episodios_exe"), "Confirmar"),
            tags$br(),
            tags$br(),
            downloadButton(
              outputId = ns("episodios_descargar_csv"),
              label = "CSV",
              style = "width:100%;"),
            tags$br(),
            tags$br(),
            downloadButton(
              outputId = ns("episodios_descargar_xlsx"),
              label = "Excel",
              style = "width:100%;"))),
      column(
        width = 9,
        box(
          width = "100%",
          div(
            DT::dataTableOutput(outputId = ns("episodios_tabla")),
            style = "font-size:90%"))))
  )
}

episodios_server <- function(input, output, session, datos, opciones, 
                             nombre_id) {
  
  ns <- NS(nombre_id)
  
  episodios <- reactiveValues(tabla = data.table())
  
  observeEvent(datos$colnames, {
    print(datos$colnames)
    updatePickerInput(
      session = session,
      inputId = "episodios_col_valor",
      choices = datos$colnames
    )
    updatePickerInput(
      session = session,
      inputId = "episodios_cols",
      choices = datos$colnames
    )
    updatePickerInput(
      session = session,
      inputId = "episodios_cols_sep",
      choices = datos$colnames
    )
  })
  
  observeEvent(input$episodios_cols, {
    if (!is.null(datos$colnames)) {
      output$episodios_jerarquia <- renderUI({
        tagList(
          orderInput(
            inputId = ns("episodios_jerarquia_nivel_1"),
            label = "Nivel 1",
            items = NULL,
            width = "100%", 
            connect = c(
              ns("episodios_jerarquia_nivel_2"),
              ns("episodios_jerarquia_nivel_3"))
          ),
          orderInput(
            inputId = ns("episodios_jerarquia_nivel_2"),
            label = "Nivel 2",
            items = NULL,
            width = "100%",
            connect = c(
              ns("episodios_jerarquia_nivel_1"),
              ns("episodios_jerarquia_nivel_3"))
          ),
          orderInput(
            inputId = ns("episodios_jerarquia_nivel_3"),
            label = "Nivel 3",
            items = datos$valores_unicos[[input$episodios_cols]],
            width = "100%",
            connect = c(
              ns("episodios_jerarquia_nivel_1"),
              ns("episodios_jerarquia_nivel_2"))
          )
        )
      })
    }
  })
  
  observeEvent(input$episodios_exe, {
    print(input$episodios_jerarquia_nivel_1_order)
    print(input$episodios_jerarquia_nivel_2_order)
    print(input$episodios_jerarquia_nivel_3_order)
    if(!is.null(datos$colnames)) {
      if(!is.null(input$episodios_col_valor) && input$episodios_cols != "NA") {
        opciones$episodios_cols <- input$episodios_cols
        opciones$episodios_col_valor <- input$episodios_col_valor
        opciones$episodios_cols_sep <- input$episodios_cols_sep
        withProgress(message = "Calculando descriptiva por episodio",{
          episodios$tabla <- episodios_jerarquia(
            data = datos$data_table,
            columnas =      opciones$episodios_cols, 
            columna_valor = opciones$valor_costo, 
            columna_sep =   opciones$episodios_cols_sep,
            columna_suma =  opciones$episodios_col_valor,
            nivel_1 = input$episodios_jerarquia_nivel_1_order,
            nivel_2 = input$episodios_jerarquia_nivel_2_order,
            nivel_3 = input$episodios_jerarquia_nivel_3_order)
          
          print(episodios$tabla)
          
          output$episodios_tabla <- DT::renderDataTable({
            DT::datatable(
              episodios$tabla,
              options = list(
                language = list(
                  url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 50,
                autoWidth = FALSE,
                ordering=T, 
                scrollX = TRUE,
                scrollY = "60vh"),
              rownames= FALSE) %>%
              formatCurrency(
                c('P50','P75','P90','Media','Media truncada 10%',
                  'Media truncada 5%','Desv.tipica'),
                mark = ".",
                dec.mark = ",") %>%
              formatCurrency(c('Suma','Min.','Max.','Rango'),
                             digits=0,
                             mark = ".",
                             dec.mark = ",")
          })   
        })
      }
    }
  })
}