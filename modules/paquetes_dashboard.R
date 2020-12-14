paquetes_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  # Paquetes ---------------------------------------------------------
  
  tagList(
    fluidRow(
      column(
        width = 12,
        valueBoxOutput(outputId = ns("paquetes_especialidad"), width = 12))),
    fluidRow(
      column(
        width = 4,
        valueBoxOutput(outputId = ns("paquetes_valortotal"), width = 12)),
      column(
        width = 4,
        valueBoxOutput(outputId = ns("paquetes_costototal"), width = 12)),
      column(
        width = 4,
        valueBoxOutput(outputId = ns("paquetes_codpaquete"), width = 12))),
    fluidRow(
      column(
        width = 12,
        valueBoxOutput(outputId = ns("paquetes_descripcion"), width = 12))),
    fluidRow(
      column(
        width = 9,
        box(
          width = 12,
          height = "100%",
          selectInput(
            inputId = ns("paquetes_select"),
            label = "Paquete:",
            choices = "Ninguno"),
          radioButtons(
            inputId = ns("paquetes_valor_costo"),
            label = NULL,
            choiceNames = c("Valor", "Costo"),
            choiceValues = c("valor", "costo")
          ),
          ggiraph::ggiraphOutput(
            outputId = ns("paquetes_plot_ref"),
            width = "100%",
            height = "100%") %>%
            withSpinner(),
          tags$p(style = "text-align: justify;",
                 tags$em("Este gráfico corresponde a la comparación del valor o costo de la institución para el paquete seleccionado comparado con paquetes ofertados en otras instituciones hospitalarias. También, muestra la media del mercado, el valor mínimo y el máximo, que permite evaluar la diferencia entre el valor o costo ofertado por la institución frente a otros paquetes que existan actualmente en el mercado."
                 )))),
      column(
        width = 3,
        box(
          title = "Servicio:",
          width = 12,
          tags$h3(textOutput(outputId = ns("paquetes_servicios")))),
        box(
          title = "Inclusiones:",
          width = 12,
          height = "100%",
          tags$p(textOutput(outputId = ns("paquetes_inclusiones")))),
        box(
          title = "Exclusiones:",
          width = 12,
          height = "100%",
          tags$p(textOutput(ns("paquetes_exclusiones")))))),
    fluidRow(
      column(
        width = 6,
        box(
          width = 12,
          selectizeInput(
            inputId = ns("paquetes_componenete_select"),
            label = "Componente:",
            choices = "Ninguno",
            multiple = T),
          selectInput(
            inputId = ns("paquetes_componenete_datos"),
            label = "Datos:",
            choices = c("PRESTACIÓN" = "prestacion",
                        "TIPO DE COSTO" = "tipo_de_costo")),
          plotlyOutput(
            outputId = ns("paquetes_componenete_plot"),
            width = "100%",
            height = "100%"),
          dataTableOutput(outputId = ns("paquetes_componenete_tabla")),
          br(),
          tags$p(style = "text-align: justify;",
                 tags$em(
                   "En la sección de componentes se podrán seleccionar uno o varios de los componentes que conforman el paquete. Al escoger varios, se pueden observar las prestaciones o los tipos de costo que hacen parte de esta categoría."
                 )))),
      column(
        width = 6,
        box(
          width = 12,
          selectizeInput(
            inputId = ns("paquetes_tipo_costo_select"),
            label = "Tipo de costo:",
            choices = "Ninguno",
            multiple = T),
          selectInput(
            inputId = ns("paquetes_tipo_costo_datos"),
            label = "Datos:",
            choice = c("PRESTACION" = "prestacion", 
                       "COMPONENTE" = "componente")),
          plotlyOutput(
            outputId = ns("paquetes_tipo_costo_plot"),
            width = "100%",
            height = "100%"),
          dataTableOutput(outputId = ns("paquetes_tipo_costo_tabla")),
          br(),
          tags$p(style = "text-align: justify;",
                 tags$em(
                   "En la sección de tipos de costo se podrán seleccionar todos los tipos de costo que conforman el paquete. Al escoger varios, se pueden observar las prestaciones o los componentes que hacen parte de esta categoría."
                 ))))),
    fluidRow(
      column(
        width = 7,
        box(
          width = 12,
          DT::dataTableOutput(
            outputId = ns("paquetes_tabla"),
            width = "100%"))),
      column(
        width = 5,
        box(
          width = 12,
          ggiraph::girafeOutput(
            outputId = ns("paquetes_ref_cups"),
            width = "100%"),
          tags$p(style = "text-align: justify;",
                 tags$em(
                   "La tabla de prestaciones discriminada permite seleccionar cada prestación que hace parte del paquete generando un gráfico comparativo de cada prestación individual con los valores de referencia de los cuales se tenga información en el análisis del mercado."
                 ))),
        box(width = 12,
            selectInput(
              inputId = ns("paquetes_resumen_select"),
              label = "",
              choices = c("COMPONENTE" = "componente",
                          "TIPO DE COSTO" = "tipo_de_costo")),
            plotlyOutput(
              outputId = ns("paquetes_resumen_plot"),
              width = "100%",
              height = "100%"),
            dataTableOutput(outputId = ns("paquetes_resumen_tabla")),
            br(),
            tags$p(style = "text-align: justify;",
                   tags$em(
                     'Al seleccionar "Tipo de cost" o "Componente" se genera un gráfico circular mostrando las diferentes partes de la categoría y sus proporciones.'
                     )
                   )
            )
        )
      )
  )
  
  
}

paquetes_dashboard_server <- function(
  input, output, session) {
  
  
#unique(paquetes_cups$componente)     paquetes_componenete_select
# unique(paquetes_cups$tipo_de_costo) paquetes_tipo_costo_select
  
  paquetes <- 
    as.data.table(read_feather("datos/paquetes/paquetes.feather"))
  
  paquetes_ref <- 
    as.data.table(read_feather("datos/paquetes/referente-paquetes.feather"))
  
  paquetes_ref_cups <- 
    as.data.table(read_feather("datos/paquetes/referente.feather"))
  
  paquetes_paquetes <- 
    paquetes[componente == "PAQUETE"]
  
  paquetes_cups <- 
    paquetes[componente != "PAQUETE"]
  
  observeEvent(paquetes, {
    updateSelectizeInput(
      session = session,
      inputId = "paquetes_select",
      choices = na.omit(
        unique(paquetes$codigo_paquete))
    )
    updateSelectizeInput(
      session = session,
      inputId = "paquetes_tipo_costo_select",
      choices = unique(paquetes_cups$tipo_de_costo)
    )
    updateSelectizeInput(
      session = session,
      inputId = "paquetes_componenete_select",
      choices = unique(paquetes_cups$componente)
    )
  })
  
  observeEvent(input$paquetes_actualizar, {
    confirmSweetAlert(
      session,
      inputId = "paquetes_actualizar_confirmar", 
      title = "Confirmar", 
      text = "¿Seguro que quieres actualizar los paquetes?
              Al final, deberá reiniciar la aplicación.", 
      showCloseButton = TRUE,
      btn_labels = c("Cancelar", "Confirmar"))
  })
  
  observeEvent(input$paquetes_actualizar_confirmar, {
    if (paquetes_actualizar_confirmar) {
      unlink("datos/paquetes/", recursive = TRUE)
      dir.create("datos/paquetes")
      withProgress(
        value = 0,
        message = "Actualizando paquetes...", {
          write_feather(
            x = sheets_read(
              paquete_path,
              sheet = "paquetes",
              col_types = "cccdcccccccdd"),
            "datos/paquetes/paquetes.feather")
          incProgress(0.3)
          write_feather(
            sheets_read(
              paquete_path,
              sheet = "referente_paquetes"),
            "datos/paquetes/referente-paquetes.feather")
          incProgress(0.3)
          write_feather(
            sheets_read(
              paquete_path,
              sheet = "referente"),
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
  
  paquetes_valores <- reactiveValues(
    "paquete" = "",
    "servicio" = "",
    "descripcion" = "",
    "inclusiones" = "",
    "exclusiones" = "",
    "especialidad" = ""
  )
  
  observeEvent(input$paquetes_select, {
    if(!is.null(paquetes)) {
      paquetes_valores$paquete <- input$paquetes_select
      
      paquetes_valores$paquete_datos <- paquetes_paquetes[
        codigo_paquete == input$paquetes_select]
      paquetes_valores$paquete_cups <- paquetes_cups[
        codigo_paquete == input$paquetes_select]
      
      paquetes_valores$servicio <-
        paquetes_valores$paquete_datos$servicio
      paquetes_valores$especialidad <-
        paquetes_valores$paquete_datos$especialidad
      paquetes_valores$descripcion <-
        paquetes_valores$paquete_datos$descripcion
      paquetes_valores$inclusiones <- 
        paquetes_valores$paquete_datos$inclusiones
      paquetes_valores$exclusiones <- 
        paquetes_valores$paquete_datos$exclusiones
    }
  })
  
  output$paquetes_tabla <- DT::renderDataTable({
    if(!is.null(paquetes)) {
      datatable(
        paquetes_valores$paquete_cups[,
                                       c("cums_cups",
                                         "prestacion",
                                         "componente", 
                                         "tipo_de_costo",
                                         "valor",
                                         "costo")],
        colnames = c(
          "CUMS/CUPS",
          "Prestación",
          "Componente",
          "Tipo de costo",
          "Valor",
          "Costo"),
        rownames = F,
        selection = 'single',
        options = list(
          dom='ft',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = nrow(paquetes_valores$paquete_cups),
          ordering = FALSE,
          scrollX = TRUE,
          scrollY = "60vh")) %>%
        DT::formatCurrency(
          columns = c("valor", "costo"),
          digits = 0,
          mark = ".",
          dec.mark = "," )
    }
  })
  
  output$paquetes_especialidad <- renderValueBox({
    if(!is.null(paquetes)) {
      valueBox(
        value = paquetes_valores$especialidad, 
        subtitle = "Especialidad", 
        icon = icon("stethoscope", lib = "font-awesome"), 
        color = "yellow"
      )
    }
  })
  
  output$paquetes_valortotal <- renderValueBox({
    if(!is.null(paquetes)) {
      valueBox(
        value = formatAsCurrency(paquetes_valores$paquete_datos$valor),
        subtitle = "Valor del paquete", 
        icon = icon("tags", lib = "font-awesome"), 
        color = "green"
      )
    }
  })
  
  output$paquetes_costototal <- renderValueBox({
    if(!is.null(paquetes)) {
      valueBox(
        value = formatAsCurrency(paquetes_valores$paquete_datos$costo),
        subtitle = "Costo del paquete", 
        icon = icon("money-check", lib = "font-awesome"),
        color = "red"
      )
    }
  })
  
  output$paquetes_codpaquete <- renderValueBox({
    if(!is.null(paquetes)) {
      valueBox(
        value = paquetes_valores$paquete,
        subtitle = "Código del paquete", 
        icon = icon("qrcode", lib = "font-awesome"),
        color = "blue"
      )
    }
  })
  
  output$paquetes_servicios <- renderText({
    if(!is.null(paquetes))
      paquetes_valores$servicio
  })
  
  output$paquetes_descripcion <- renderValueBox({
    if(!is.null(paquetes)) {
      valueBox(
        value = str_wrap(paquetes_valores$descripcion, width = 30),
        subtitle = "Descripción del paquete",
        icon = icon("hashtag", lib = "font-awesome"),
        color = "teal"
      )
    }
  })
  
  output$paquetes_inclusiones <- renderText({
    if(!is.null(paquetes)) {
      paquetes_valores$inclusiones
    }
  })
  
  output$paquetes_exclusiones <- renderText({
    if(!is.null(paquetes)) {
      paquetes_valores$exclusiones
    }
  })
  
  output$paquetes_plot_ref <- renderggiraph({
    if(!is.null(paquetes)) {
      paquete_ref_plot(
        paquetes = paquetes_valores$paquete_datos,
        referente = paquetes_ref,
        cups = paquetes_valores$paquete,
        valor_costo = input$paquetes_valor_costo)
    }
  })
  
  output$paquetes_ref_cups <- renderggiraph({
    if(!is.null(input$paquetes_tabla_rows_selected))
      ref_plot(
        paquetes = paquetes_valores$paquete_cups,
        referente = paquetes_ref_cups[
          codigo_paquete == paquetes_valores$paquete],
        cups = paquetes_valores$paquete_cups[
          input$paquetes_tabla_rows_selected,
          cums_cups],
        valor_costo = input$paquetes_valor_costo)
  })
  
  output$paquetes_resumen_plot <- renderPlotly({
    if(!is.null(paquetes)) 
      pie_chart(
        paquetes = paquetes_valores$paquete_cups,
        columna = input$paquetes_resumen_select,
        valor_costo = input$paquetes_valor_costo,
        nombre_legend = ifelse(
          test = input$paquetes_resumen_select == "componente",
          yes = "Componente",
          no = "Tipo de costo"
        ))
  })
  
  output$paquetes_resumen_tabla <- DT::renderDataTable({
    if(!is.null(paquetes)) {
      datatable(
        resumenComp(
          tabla = paquetes_valores$paquete_cups,
          columna = input$paquetes_resumen_select, 
          colsum = input$paquetes_valor_costo),
        rownames = F,
        selection = 'none',
        options = list(
          dom='ft',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = nrow(paquetes),
          ordering = FALSE,
          scrollX = TRUE,
          scrollY = "30vh")) %>%
        formatStyle(
          columns = c(
            input$paquetes_resumen_select,
            "SUMA",
            "PARTICIPACIÓN (%)"),
          backgroundColor = "white")
    }
  })
  
  output$paquetes_componenete_plot <- renderPlotly({
    if(!is.null(input$paquetes_componenete_select))
      pie_chart(
        paquetes = paquetes_valores$paquete_cups[
          componente %in% input$paquetes_componenete_select],
        columna = input$paquetes_componenete_datos,
        valor_costo = input$paquetes_valor_costo,
        nombre_legend = ifelse(
          test = input$paquetes_componenete_datos == "prestacion",
          yes = "Prestación",
          no = "Tipo de costo"
        ))
  })
  
  output$paquetes_componenete_tabla <- DT::renderDataTable({
    if(!is.null(input$paquetes_componenete_select)) {
      datatable(
        resumenComp(
          tabla = paquetes_valores$paquete_cups[
            componente %in% input$paquetes_componenete_select],
          columna = input$paquetes_componenete_datos,
          colsum = input$paquetes_valor_costo),
        rownames = F, 
        selection = 'none',
        options = list(
          dom='ft',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = nrow(paquetes), 
          ordering = FALSE,
          scrollX = TRUE,
          scrollY = "20vh") ) %>%
        formatStyle(
          columns = c(input$paquetes_componenete_datos, "SUMA", "PARTICIPACIÓN (%)"),
          backgroundColor = "white")
    }
  })
  
  output$paquetes_tipo_costo_plot <- renderPlotly({
    if(!is.null(input$paquetes_tipo_costo_select))
      pie_chart(
        paquetes = paquetes_valores$paquete_cups[
          tipo_de_costo %in% input$paquetes_tipo_costo_select],
        columna = input$paquetes_tipo_costo_datos,
        valor_costo = input$paquetes_valor_costo,
        nombre_legend = ifelse(
          test = input$paquetes_tipo_costo_datos == "componente",
          yes = "Componente",
          no = "Prestación"
        ))
  })
  
  output$paquetes_tipo_costo_tabla <- DT::renderDataTable({
    if(!is.null(input$paquetes_tipo_costo_select)) {
      datatable(
        resumenComp(
          tabla = paquetes_valores$paquete_cups[
            tipo_de_costo %in% input$paquetes_tipo_costo_select],
          columna = input$paquetes_tipo_costo_datos,
          colsum = input$paquetes_valor_costo),
        rownames = F,
        selection = 'none',
        options = list(
          dom='ft',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = nrow(paquetes),
          ordering = FALSE,
          scrollX = TRUE,
          scrollY = "20vh")) %>%
        formatStyle(
          columns = c(input$paquetes_tipo_costo_datos, 
                      "SUMA",
                      "PARTICIPACIÓN (%)"),
          backgroundColor = "white")
    }
  })
  
}