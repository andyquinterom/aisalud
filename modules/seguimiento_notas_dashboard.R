seguimiento_notas_dashboard_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        valueBoxOutput(outputId = ns("entidad"), width = 12))),
    fluidRow(
      column(
        width = 12,
        valueBoxOutput(
          outputId = ns("valor_mes"), width = 4),
        valueBoxOutput(
          outputId = ns("poblacion"),
          width = 4),
        valueBoxOutput(
          outputId = ns("departamento"),
          width = 4))),
    fluidRow(
      column(
        width = 12,
        box(
          width = 12,
          pickerInput(
            inputId = ns("board_select"), 
            width = "100%",
            choices = "NA",
            label = "Nota técnica")),
        box(
          width = 12,
          title = "Nota técnica:",
          fluidRow(
            column(
              width = 5,
              DT::dataTableOutput(outputId = ns("board_datos"))),
            column(
              width = 7,
              plotlyOutput(
                outputId = ns("plot_agrupadores"),
                width = "100%",
                height = "600px")))),
        box(
          title = "Inclusiones:",
          width = 6,
          DT::dataTableOutput(outputId = ns("inclusiones"))),
        box(
          title = "Exclusiones:",
          width = 6,
          DT::dataTableOutput(outputId = ns("exclusiones")))))
  )
  
}

seguimiento_notas_dashboard_server <- function(
  input, output, session, indice, nota_tecnica, inclusiones) {
  
  observeEvent(indice, {
    updatePickerInput(
      session = session,
      inputId = "board_select",
      choices = indice$cod_nt
    )
  })
  
  dash_nt_valores <- reactiveValues(
    "indice" = indice,
    "datos" = nota_tecnica, 
    "inclusiones" = inclusiones
  )
  
  observeEvent(input$board_select, {
    dash_nt_valores$datos <- nota_tecnica[
      cod_nt == input$board_select]
    dash_nt_valores$indice <- indice[
      cod_nt == input$board_select]
    dash_nt_valores$inclusiones <- inclusiones[
      cod_nt == input$board_select]
  })
  
  output$entidad <- renderValueBox({
    if(!is.null(dash_nt_valores$indice)) {
      valueBox(
        value = dash_nt_valores$indice$nom_prestador,
        subtitle = "Prestador", 
        icon = icon("stethoscope", lib = "font-awesome"), 
        color = "yellow"
      )
    }
  })
  
  output$valor_mes <- renderValueBox({
    if(!is.null(dash_nt_valores$indice)) {
      valueBox(
        value = formatAsCurrency(dash_nt_valores$indice$valor_mes),
        subtitle = "Valor total a mes", 
        icon = icon("dollar-sign", lib = "font-awesome"),
        color = "green"
      )
    }
  })
  
  output$poblacion <- renderValueBox({
    if(!is.null(dash_nt_valores$indice)) {
      valueBox(
        value = format(
          dash_nt_valores$indice$poblacion,
          scientific = F,
          big.mark = ".", 
          decimal.mark = ","),
        subtitle = "Pobalción", 
        icon = icon("users", lib = "font-awesome"),
        color = "blue"
      )
    }
  })
  
  output$departamento <- renderValueBox({
    if(!is.null(dash_nt_valores$indice)) {
      valueBox(
        value = dash_nt_valores$indice$departamento,
        subtitle = dash_nt_valores$indice$ciudades, 
        icon = icon("city", lib = "font-awesome"),
        color = "aqua"
      )
    }
  })
  
  output$inclusiones <- DT::renderDataTable({
    if(!is.null(dash_nt_valores$inclusiones)) {
      datatable(
        dash_nt_valores$inclusiones[incluido == 1, c("objeto", "notas")],
        rownames = F,
        selection = 'none',
        colnames = c("Observación", "Notas"),
        options = list(
          dom='ft',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = nrow(dash_nt_valores$inclusiones),
          ordering = FALSE, 
          scrollX = TRUE,
          scrollY = "60vh")) %>%
        DT::formatStyle(
          columns = 1:4,
          valueColumns = 1, 
          backgroundColor = "white")
    }
  })
  
  output$exclusiones <- DT::renderDataTable({
    if(!is.null(dash_nt_valores$inclusiones)) {
      datatable(
        dash_nt_valores$inclusiones[incluido == 0, c("objeto", "notas")],
        rownames = F,
        selection = 'none',
        colnames = c("Observación", "Notas"),
        options = list(
          dom='ft',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = nrow(dash_nt_valores$inclusiones),
          ordering = FALSE, 
          scrollX = TRUE,
          scrollY = "60vh")) %>%
        DT::formatStyle(
          columns = 1:4,
          valueColumns = 1, 
          backgroundColor = "white")
    }
  })
  
  output$plot_agrupadores <- renderPlotly({
    pie_chart(
      paquetes = dash_nt_valores$datos,
      columna = "agrupador",
      valor_costo = "valor_mes")
  })
  
  output$board_datos <- DT::renderDataTable({
    if(!is.null(dash_nt_valores$datos)) {
      datatable(
        dash_nt_valores$datos[, c("agrupador", "frec_mes", "cm", "valor_mes")],
        rownames = F, 
        selection = 'none',
        colnames = c("Agrupador", "Frecuencia a mes", "cm", "Valor a mes"),
        options = list(
          dom='ft', 
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = nrow(dash_nt_valores$datos),
          ordering = FALSE,
          scrollX = TRUE,
          scrollY = "600px")) %>%
        DT::formatCurrency(
          columns = c("cm", "valor_mes"),
          digits = 0, mark = ".", dec.mark = ","
        ) %>%
        DT::formatStyle(
          columns = 1:4,
          valueColumns = 1,
          backgroundColor = "white")
    }
  })
  
  
  
  
}