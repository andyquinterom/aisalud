seguimiento_notas_dashboard_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          width = 7,
          height = "500px",
          DT::dataTableOutput(
            outputId = ns("indice_tabla"),
            height = "auto") %>%
            withSpinner()),
        box(
          width = 5, 
          height = "500px",
          leafletOutput(
          height = "480px",
          outputId = ns("indice_mapa")) %>%
            withSpinner())
      )
    ),
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
          selectizeInput(
            inputId = ns("board_select"), 
            width = "100%",
            choices = "Ninguno",
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

seguimiento_notas_dashboard_server <- function(id, opciones) {
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      nt_opciones <- reactiveValues(
        # "indice" = indice,
        # "datos" = nota_tecnica, 
        # "inclusiones" = inclusiones
      )

      observe({
        opciones$notas_tecnicas_raw <- tbl(conn, "notas_tecnicas") %>%
          pull(notas_tecnicas) %>%
          prettify()
        
        opciones$notas_tecnicas_lista <- opciones$notas_tecnicas_raw %>%
          parse_json(simplifyVector = TRUE)
        
        opciones$notas_tecnicas <- opciones$notas_tecnicas_lista %>%
          parse_nt()
        
        opciones$indice_todos <- parse_nt_indice(
          opciones$notas_tecnicas_lista,
          tabla_agrupadores = opciones$notas_tecnicas
        )
        
      })
      
      observeEvent(names(opciones$notas_tecnicas_lista), {
        updateSelectizeInput(
          session = session,
          inputId = "board_select",
          choices = names(opciones$notas_tecnicas_lista)
        )
      })
      
      output$indice_tabla <- DT::renderDataTable({
        if(!is.null(opciones$indice_todos)) {
          datatable(
            opciones$indice_todos %>%
              mutate(vigencia = case_when(
                vigente ~ "Vigente",
                TRUE ~ "No vigente")) %>%
              select(-c(cod_departamento, vigente)),
            rownames = F, 
            selection = 'none', 
            colnames = c(
              "Nombre NT",
              "Prestador",
              "Población", 
              "Departamento",
              "Ciudades",
              "Valor a mes",
              "Vigencia"),
            options = list(
              dom='ft',
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 10, 
              ordering = FALSE, 
              scrollX = TRUE,
              scrollY = "400px")) %>%
            DT::formatCurrency(
              columns = c("valor_mes")
              , digits = 0, mark = ".", dec.mark = ",") %>%
            DT::formatRound(columns = "poblacion", mark = ".", dec.mark = ",",
                            digits = 0)
        }
      })
      
      output$indice_mapa <- renderLeaflet({
        mapa_valores(opciones$indice_todos %>%
                       filter(vigente))
      })
      
      observeEvent(input$board_select, {
        if (input$board_select %notin% c("Ninguno", "")) {
          nt_opciones$datos <- opciones$notas_tecnicas %>%
            filter(nt == input$board_select) %>%
            rename(cod_nt = nt)
          nt_opciones$indice <- opciones$indice_todos %>%
            filter(cod_nt == input$board_select)
        }
      })

      output$entidad <- renderValueBox({
        if(!is.null(nt_opciones$indice)) {
          valueBox(
            value = nt_opciones$indice$nom_prestador,
            subtitle = "Prestador",
            icon = icon("stethoscope", lib = "font-awesome"),
            color = "yellow"
          )
        }
      })

      output$valor_mes <- renderValueBox({
        if(!is.null(nt_opciones$indice)) {
          valueBox(
            value = formatAsCurrency(nt_opciones$indice$valor_mes),
            subtitle = "Valor total a mes",
            icon = icon("dollar-sign", lib = "font-awesome"),
            color = "green"
          )
        }
      })

      output$poblacion <- renderValueBox({
        if(!is.null(nt_opciones$indice)) {
          valueBox(
            value = format(
              nt_opciones$indice$poblacion,
              scientific = F,
              big.mark = ".",
              decimal.mark = ","),
            subtitle = "Población",
            icon = icon("users", lib = "font-awesome"),
            color = "blue"
          )
        }
      })

      output$departamento <- renderValueBox({
        if(!is.null(nt_opciones$indice)) {
          valueBox(
            value = nt_opciones$indice$departamento,
            subtitle = nt_opciones$indice$ciudades,
            icon = icon("city", lib = "font-awesome"),
            color = "aqua"
          )
        }
      })

      # output$inclusiones <- DT::renderDataTable({
      #   if(!is.null(nt_opciones$inclusiones)) {
      #     datatable(
      #       nt_opciones$inclusiones[incluido == 1, c("objeto", "notas")],
      #       rownames = F,
      #       selection = 'none',
      #       colnames = c("Observación", "Notas"),
      #       options = list(
      #         dom='ft',
      #         language = list(
      #           url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
      #         pageLength = nrow(nt_opciones$inclusiones),
      #         ordering = FALSE, 
      #         scrollX = TRUE,
      #         scrollY = "60vh")) %>%
      #       DT::formatStyle(
      #         columns = 1:4,
      #         valueColumns = 1, 
      #         backgroundColor = "white")
      #   }
      # })
      # 
      # output$exclusiones <- DT::renderDataTable({
      #   if(!is.null(nt_opciones$inclusiones)) {
      #     datatable(
      #       nt_opciones$inclusiones[incluido == 0, c("objeto", "notas")],
      #       rownames = F,
      #       selection = 'none',
      #       colnames = c("Observación", "Notas"),
      #       options = list(
      #         dom='ft',
      #         language = list(
      #           url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
      #         pageLength = nrow(nt_opciones$inclusiones),
      #         ordering = FALSE, 
      #         scrollX = TRUE,
      #         scrollY = "60vh")) %>%
      #       DT::formatStyle(
      #         columns = 1:4,
      #         valueColumns = 1, 
      #         backgroundColor = "white")
      #   }
      # })

      output$plot_agrupadores <- renderPlotly({
        pie_chart(
          paquetes = nt_opciones$datos,
          columna = "agrupador",
          valor_costo = "valor_mes")
      })

      output$board_datos <- DT::renderDataTable({
        if(!is.null(nt_opciones$datos)) {
          datatable(
            nt_opciones$datos %>%
              select(agrupador, frec_mes, frecuencia_pc, cm, valor_mes),
            rownames = F,
            selection = 'none',
            colnames = c("Agrupador", "Frecuencia a mes",
                         "Frecuencia per capita", "cm", "Valor a mes"),
            options = list(
              dom='ft',
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = nrow(nt_opciones$datos),
              ordering = FALSE,
              scrollX = TRUE,
              scrollY = "600px")) %>%
            DT::formatCurrency(
              columns = c("cm", "valor_mes"),
              digits = 0, mark = ".", dec.mark = ","
            ) %>%
            DT::formatStyle(
              columns = 1:5,
              valueColumns = 1,
              backgroundColor = "white")
        }
      })
  
  })
  
}