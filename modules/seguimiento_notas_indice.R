seguimiento_notas_indice_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    box(
      width = 7,
      DT::dataTableOutput(
        outputId = ns("indice_tabla"),
        height = "auto")),
    box(width = 5, plotlyOutput(
      outputId = ns("indice_mapa"),
      height = "auto"))
  )
}


seguimiento_notas_indice_server <- function(input, output, session, indice,
                                            mapa) {
  
  output$indice_tabla <- DT::renderDataTable({
    if(!is.null(indice)) {
      datatable(
        indice[, -c("COD_DEPARTAMENTO")],
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
          columns = c("VALOR_MES")
          , digits = 0, mark = ".", dec.mark = ",") %>%
        DT::formatRound(columns = "POBLACION", mark = ".")
    }
  })
  
  output$indice_mapa <- renderPlotly(
    mapa
  )
  
}