seguimiento_notas_indice_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          width = 7,
          DT::dataTableOutput(
            outputId = ns("indice_tabla"),
            height = "auto") %>%
            withSpinner()),
        box(width = 5, leafletOutput(
          outputId = ns("indice_mapa")) %>%
            withSpinner())
      )
    )
  )
}


seguimiento_notas_indice_server <- function(input, output, session, indice,
                                            nombre_id) {
  
  ns <- NS(nombre_id)
  
  output$indice_tabla <- DT::renderDataTable({
    if(!is.null(indice)) {
      datatable(
        indice[, -c("cod_departamento")],
        rownames = F, 
        selection = 'none', 
        colnames = c(
          "Nombre NT",
          "Prestador",
          "Población", 
          "Departamento",
          "Ciudades",
          "Valor a mes"),
        options = list(
          dom='ft',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = nrow(indice), 
          ordering = FALSE, 
          scrollX = TRUE,
          scrollY = "100%")) %>%
        DT::formatCurrency(
          columns = c("valor_mes")
          , digits = 0, mark = ".", dec.mark = ",") %>%
        DT::formatRound(columns = "poblacion", mark = ".", dec.mark = ",")
    }
  })
  
  output$indice_mapa <- renderLeaflet({
    mapa_valores(indice)
  })
  
}