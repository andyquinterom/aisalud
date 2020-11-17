paquetes_indice_ui <- function(id) {
  ns <- NS(id)
  
  
  
  tagList(
    fluidRow(
      box(
        width = 12,
        actionButton(ns("paquetes_actualizar"), "Actualizar", width = "100%")),
      box(
        width = 12,
        DT::dataTableOutput(outputId = ns("paquetes_indice_tabla"))))
  )
}

paquetes_indice_server <- function(
  input, output, session, paquetes, nombre_id, paquete_path) {
  
  ns <- NS(nombre_id)
  
  observeEvent(input$paquetes_actualizar, {
    confirmSweetAlert(
      session,
      inputId = ns("paquetes_actualizar_confirmar"), 
      title = "Confirmar", 
      text = "¿Seguro que quieres actualizar los paquetes?
              Al final, deberá reiniciar la aplicación.", 
      showCloseButton = TRUE,
      btn_labels = c("Cancelar", "Confirmar"))
  })
  
  observeEvent(input$paquetes_actualizar_confirmar, {
    if (input$paquetes_actualizar_confirmar) {
      unlink("datos/paquetes/", recursive = TRUE)
      dir.create("datos/paquetes")
      withProgress(
        value = 0,
        message = "Actualizando paquetes...", {
          write_feather(
            x = sheets_read(
              paquete_path,
              sheet = "PAQUETES",
              col_types = "cccdcccccccdd"),
            "datos/paquetes/paquetes.feather")
          incProgress(0.3)
          write_feather(
            sheets_read(
              paquete_path,
              sheet = "REFERENTE-PAQUETES"),
            "datos/paquetes/referente-paquetes.feather")
          incProgress(0.3)
          write_feather(
            sheets_read(
              paquete_path,
              sheet = "REFERENTE"),
            "datos/paquetes/referente.feather")
          incProgress(0.3)
        })
      sendSweetAlert(
        session,
        title = "¡Paquete actualizados efectivamente!",
        text = "Para ver los datos y gráficos actualizados,
                por favor recargar la página.",
        type = "success")
      stopApp()
    }
  })
  
  output$paquetes_indice_tabla <- DT::renderDataTable(
    datatable(
      unique(paquetes[, list(
        `CODIGO PAQUETE`,
        ESPECIALIDAD,
        SERVICIO,
        DESCRIPCION,
        INCLUSIONES,
        EXCLUSIONES)]),
      rownames = F,
      options = list(
        dom = 'ft',
        ordering = FALSE,
        scrollX = TRUE,
        scrollY = "80vh",
        pageLength = nrow(
          unique(paquetes[, list(
            `CODIGO PAQUETE`,
            ESPECIALIDAD, SERVICIO,
            DESCRIPCION, INCLUSIONES,
            EXCLUSIONES)]))
      )) %>%
      DT::formatStyle(1:6, backgroundColor = 'white')
  )
  
}