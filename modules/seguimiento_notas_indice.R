seguimiento_notas_indice_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          width = 12,
          actionButton(
            ns("dash_nt_actualizar"),
            "Actualizar",
            width = "100%")),
        box(
          width = 7,
          DT::dataTableOutput(
            outputId = ns("indice_tabla"),
            height = "auto") %>%
            withSpinner()),
        box(width = 5, plotlyOutput(
          outputId = ns("indice_mapa"),
          height = "auto") %>%
            withSpinner())
      )
    )
  )
}


seguimiento_notas_indice_server <- function(input, output, session, indice,
                                            mapa, nombre_id) {
  
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
  
  output$indice_mapa <- renderPlotly(
    mapa
  )
  
  observeEvent(input$dash_nt_actualizar, {
    confirmSweetAlert(
      session,
      inputId = ns("dash_nt_actualizar_confirmar"), 
      title = "Confirmar", 
      text = "¿Seguro que quieres actualizar las notas técnicas?
              Al final, deberá reiniciar la aplicación.", 
      showCloseButton = TRUE,
      btn_labels = c("Cancelar", "Confirmar"))
  })
  
  observeEvent(input$dash_nt_actualizar_confirmar, {
    if (input$dash_nt_actualizar_confirmar) {
      unlink("datos/nts/", recursive = TRUE)
      dir.create("datos/nts")
      withProgress(
        value = 0,
        message = "Actualizando notas técnicas...", {
          write_feather(sheets_read(nts_path,
                                    sheet = "notas_tecnicas",
                                    col_types = "ccddd"),
                        "datos/nts/notas_tecnicas.feather")
          incProgress(0.3)
          write_feather(sheets_read(nts_path, 
                                    sheet = "indice", 
                                    col_types = "ccdcccd"),
                        "datos/nts/indice.feather")
          incProgress(0.3)
          write_feather(sheets_read(nts_path, 
                                    sheet = "inclusiones", 
                                    col_types = "ccdc") ,
                        "datos/nts/inclusiones.feather")
          incProgress(0.1)
          saveRDS(
            mapaValoresNT(
              as.data.table(
                sheets_read(nts_path,
                            sheet = "indice",
                            col_types = "ccdcccd"
                )
              )
            ) %>% 
              layout(autosize = TRUE),
            "datos/nts/nt_mapa.rds")
          incProgress(0.3)
          
          sendSweetAlert(
            session,
            title = "¡Notas técnicas actualizados efectivamente!",
            text = "Para ver los datos y gráficos actualizados,
                por favor recargar la página.",
            type = "success")
          stopApp()
        }
      )
    }
  })
  
}