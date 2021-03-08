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
          tabsetPanel(
            tabPanel(
              tags$br(),
              title = "Nota técnica",
              fluidRow(
                column(
                  width = 5,
                  DT::dataTableOutput(outputId = ns("board_datos"))),
                column(
                  width = 7,
                  plotlyOutput(
                    outputId = ns("plot_agrupadores"),
                    width = "100%",
                    height = "600px"))),
              uiOutput(ns("otra_informacion"))
            ),
            tabPanel(
              tags$br(),
              title = "Seguimiento",
              fluidRow(
                column(
                  width = 4,
                  checkboxInput(
                    inputId = ns("comparar_episodios"),
                    label = "Agrupar por episodios",
                    value = F
                  ),
                  uiOutput(
                    outputId = ns("comparar_col_valor_out")
                  ),
                  selectizeInput(
                    inputId = ns("comparar_agrupador"),
                    label = "Agrupar por:",
                    choices = c("Ninguno"),
                    multiple = FALSE),
                  radioButtons(
                    inputId = ns("comparar_tipo"),
                    label = "Seguimiento de",
                    choices = c(
                      "Frecuencias", "Valor facturado", "Costos medios"), 
                    inline = TRUE, 
                    width = "100%"),
                  actionButton(
                    inputId = ns("comparar_exe"),
                    "Ejecutar",
                    width = "100%"),
                  tags$br(),
                  tags$br(),
                  downloadButton(
                    outputId = ns("comparar_descargar_xlsx"), 
                    label = "Excel",
                    style = "width:100%;")
                ),
                column(
                  width = 8,
                  uiOutput(ns("comparar_jerarquia"))
                )
              ),
              uiOutput(ns("comparar_resultados")) %>%
                withSpinner()
            )
          ))))
  )
  
}

seguimiento_notas_dashboard_server <- function(id, opciones) {
  
  ns <- NS(id)
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      nt_opciones <- reactiveValues(
        # "indice" = indice,
        # "datos" = nota_tecnica, 
        # "inclusiones" = inclusiones
      )

      observe({
        
        opciones$notas_tecnicas_updated
        
        opciones$notas_tecnicas_raw <- tbl(conn, "notas_tecnicas") %>%
          pull(notas_tecnicas)
        
        tryCatch(
          expr = {

            opciones$notas_tecnicas_lista <- opciones$notas_tecnicas_raw %>%
              parse_json(simplifyVector = TRUE)
            
            opciones$notas_tecnicas <- opciones$notas_tecnicas_lista %>%
              parse_nt()
            
            opciones$indice_todos <- parse_nt_indice(
              opciones$notas_tecnicas_lista,
              tabla_agrupadores = opciones$notas_tecnicas
            )
        
          },
          
          error = function(e) {
            print(e)
            sendSweetAlert(
              session = session,
              title = "Error",
              text = e[1],
              type = "error"
            )
          }
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
      
      observe({
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
        } else {
          valueBox(
            value = "",
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
        } else {
          valueBox(
            value = "",
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
        } else {
          valueBox(
            value = "",
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
        } else {
          valueBox(
            value = "",
            subtitle = "",
            icon = icon("city", lib = "font-awesome"),
            color = "aqua"
          )
        }
      })
      
      output$otra_informacion <- renderUI({
        
        if (!is.null(opciones$indice_todos) &&
            input$board_select %notin% c("Ninguno", "")) {
          
          otra_informacion_datos <-
            opciones$notas_tecnicas_lista[[input$board_select]]
          
          
          
          tagList(
            if (!is.null(otra_informacion_datos$inclusiones) ||
                !is.null(otra_informacion_datos$exclusiones)) {
                column(
                  width = 12,
                  tags$hr(),
                  fluidRow(
                    column(
                      width = 6,
                      tags$h3("Inclusiones") %>% tags$u(),
                      tags$ol(
                        purrr::map(
                          .x = otra_informacion_datos$inclusiones,
                          tags$li))),
                    column(
                      width = 6,
                      tags$h3("Exclusiones") %>% tags$u(),
                      tags$ol(
                        purrr::map(
                          .x = otra_informacion_datos$exclusiones,
                          tags$li)))
                    )
                  )
            },
            if (!is.null(otra_informacion_datos$notas)) {
                column(
                  width = 12,
                  tags$hr(),
                  tags$h2("Notas") %>% tags$u(),
                  tags$p(otra_informacion_datos$notas)
                )
            },
            if (!is.null(otra_informacion_datos$perfil)) {
              if (otra_informacion_datos$perfil %in%
                  names(opciones$perfil_lista)) {
                perfil_nota_tecnica <- 
                  opciones$perfil_lista[[otra_informacion_datos$perfil]]
                
                width_row <- 12/length(names(perfil_nota_tecnica[["jerarquia"]]))
                
                  column(
                    width = 12,
                    tags$hr(),
                    tags$h2("Conteos especiales y jerarquía") %>% tags$u(),
                    fluidRow(
                      purrr::map2(
                        .x = perfil_nota_tecnica[["jerarquia"]],
                        .y = names(perfil_nota_tecnica[["jerarquia"]]),
                        .f = function(x, y) {
                          print(y)
                          if (!is.null(x)) {
                            column(
                              width = width_row,
                              tags$h3(toupper(y)),
                              tags$ol(
                                purrr::map(
                                  .x = intersect(
                                    x = x,
                                    y = pull(nt_opciones$datos, agrupador)),
                                  tags$li))
                            ) %>%
                              return()
                          }
                        })
                    )
                  )
                
              }
            }
          )
          
        }
        
      })

      output$plot_agrupadores <- renderPlotly({
        if (!is.null(nt_opciones$datos)) {
          pie_chart(
            paquetes = nt_opciones$datos,
            columna = "agrupador",
            valor_costo = "valor_mes")
        }
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
      
      # Comparacion ------------------
      
      comparar <- reactiveValues(
        datos = list(),
        agrupadores_items = NULL
      )
      
      observeEvent(opciones$colnames, {
        if (input$comparar_episodios) {
          updateSelectizeInput(
            session = session,
            inputId = "comparar_col_valor",
            choices = opciones$colnames
          )
        }
        updateSelectizeInput(
          session = session,
          inputId = "comparar_agrupador",
          choices = c("Ninguno", opciones$colnames)
        )
      })
      
      observeEvent(input$comparar_episodios, {
        if (input$comparar_episodios) {
          output$comparar_col_valor_out <- renderUI({
            selectizeInput(
              inputId = ns("comparar_col_valor"),
              label = "Sumar valor por:",
              selected = "nro_identificacion",
              choices = opciones$colnames,
              multiple = FALSE)
          })
        } else {
          output$comparar_col_valor_out <- renderUI({})
        }
      })
      
      cambio_columnas <- reactive({
        list(input$comparar_agrupador, input$comparar_episodios)
      })
      
      observeEvent(input$comparar_episodios, {
        perfil_nt <-
          opciones$notas_tecnicas_lista[[input$board_select]][["perfil"]]
        if (input$comparar_episodios && perfil_nt %in%
              names(opciones$perfil_lista)) {
          if (!opciones$perfil_enable) {
            confirmSweetAlert(
              session = session,
              inputId = ns("comparar_cambiar_perfil"),
              title = "Utilizar perfil",
              text = "Esta nota técnica tiene un perfil asignado.
                      ¿Desea utilizarlo?",
              btn_labels = c("Cancelar", "Utilizar")
            )
          } else if (opciones$perfil_enable && 
                     opciones$perfil_selected != perfil_nt) {
            confirmSweetAlert(
              session = session,
              inputId = ns("comparar_cambiar_perfil"),
              title = "Utilizar perfil",
              text = "Esta nota técnica tiene un perfil asignado diferente al
                      seleccionado.
                      ¿Desea cambiarlo?",
              btn_labels = c("Cancelar", "Carmbiar")
            )
          }
        }
      })
      
      observeEvent(input$comparar_cambiar_perfil, {
        opciones$perfil_selected <- NULL
        opciones$perfil_selected <-
          opciones$notas_tecnicas_lista[[input$board_select]][["perfil"]]
      })
      
      observeEvent(cambio_columnas(), {
        if (!is.null(opciones$colnames) && 
            input$comparar_agrupador %notin% c("", "Ninguno")) {
          tryCatch(
            expr = {
              if (input$comparar_episodios) {
                comparar$agrupadores_items <- opciones$tabla %>%
                  select(!!as.name(input$comparar_agrupador)) %>%
                  distinct() %>%
                  pull(!!as.name(input$comparar_agrupador))
                if (length(comparar$agrupadores_items) <= 60) {
                  output$comparar_jerarquia <- renderUI({
                    if (opciones$perfil_enable) {
                      tagList(
                        perfil_jerarquia(
                          perfiles = opciones$perfil_lista,
                          perfil_select = opciones$perfil_selected,
                          items = comparar$agrupadores_items,
                          funcion_jerarquia = descriptiva_jerarquia,
                          ns = ns
                        ),
                        tags$hr()
                      )
                    } else {
                      tagList(
                        descriptiva_jerarquia(
                          ns = ns,
                          items_nivel_4 = comparar$agrupadores_items
                        ),
                        tags$hr()
                      )
                    }
                  })
                } else {
                  comparar$agrupadores_items <- NULL
                  output$comparar_jerarquia <- renderUI({
                    radioButtons(
                      inputId = ns("descriptiva_unidades"),
                      label = "Unidad de descriptiva",
                      selected = comparar$unidad_descriptiva,
                      choiceNames = c(
                        "Prestación",
                        "Paciente",
                        "Factura"
                      ),
                      choiceValues = c(
                        "prestacion",
                        "nro_identificacion",
                        "nro_factura"
                      )
                    )
                  })
                  comparar$unidad_descriptiva <- input$descriptiva_unidades
                }
              } else {
                comparar$agrupadores_items <- NULL
                output$comparar_jerarquia <- renderUI({
                  radioButtons(
                    inputId = ns("descriptiva_unidades"),
                    label = "Unidad de descriptiva",
                    selected = comparar$unidad_descriptiva,
                    choiceNames = c(
                      "Prestación",
                      "Paciente",
                      "Factura"
                    ),
                    choiceValues = c(
                      "prestacion",
                      "nro_identificacion",
                      "nro_factura"
                    )
                  )
                })
                comparar$unidad_descriptiva <- input$descriptiva_unidades
              }
            },
            error = function(e) {
              print(e)
              sendSweetAlert(
                session = session,
                title = "Error", 
                type = "error",
                text = "Por favor revisar los parametros de carga de datos,
                columnas, formato de fecha y los datos. Si este problema persiste
                ponerse en contacto con un administrador."
              )
            }
          )
        }
      })
      
      observeEvent(input$seleccionar_episodio, {
        output$comparar_jerarquia <- renderUI({
          tagList(
            descriptiva_jerarquia(
              ns = ns,
              items_nivel_1 = comparar$agrupadores_items),
            tags$hr()
          )
        })
      })
      
      observeEvent(input$seleccionar_factura, {
        output$comparar_jerarquia <- renderUI({
          tagList(
            descriptiva_jerarquia(
              ns = ns,
              items_nivel_2 = comparar$agrupadores_items),
            tags$hr()
          )
        })
      })
      
      observeEvent(input$seleccionar_paciente, {
        output$comparar_jerarquia <- renderUI({
          tagList(
            descriptiva_jerarquia(
              ns = ns,
              items_nivel_3 = comparar$agrupadores_items),
            tags$hr()
          )
        })
      })
      
      observeEvent(input$seleccionar_prestacion, {
        output$comparar_jerarquia <- renderUI({
          tagList(
            descriptiva_jerarquia(
              ns = ns,
              items_nivel_4 = comparar$agrupadores_items),
            tags$hr()
          )
        })
      })
      
      observeEvent(input$comparar_exe, {
        comparar_tipo <- input$comparar_tipo
        frecuencias_test <- frecuencias_jerarquia(
          data = opciones$tabla,
          columnas =      "tipo_ambito",
          columna_fecha = "fecha_prestacion",
          columna_sep =   NULL,
          columna_suma =  "nro_factura",
          nivel_1 = input$episodios_jerarquia_nivel_1_order,
          nivel_2 = input$episodios_jerarquia_nivel_2_order,
          nivel_3 = input$episodios_jerarquia_nivel_3_order,
          nivel_4 = input$episodios_jerarquia_nivel_4_order)[["descriptiva"]]
        
        print(frecuencias_test)
        
        nt_test <- nt_opciones$datos
        output$comparar_resultados <- renderUI({
          if (comparar_tipo == "Frecuencias") {
            
            comparar$datos <- comparacion_frecuencias(
              frecuencias = frecuencias_test,
              nota_tecnica = nt_test,
              agrupador = "tipo_ambito"
            )
            
            print(comparar$datos$comparacion)
            
            output$diferencias_frecuencias <- 
              DT::renderDataTable({comparar$datos$comparacion_frecs_dt})
            
            output$diferencias_frecuencias_x_cme <- 
              DT::renderDataTable({comparar$datos$comparacion_x_cme_dt})
            
            output$diferencias_frecuencias_porcentaje <- 
              DT::renderDataTable({comparar$datos$comparacion_porcentaje_dt})
            
            comparacion_frecuencias_ui(ns)
            
          } else {
            tags$h3("chao")
          }
        })
        

        
      })
  
  })
  
}

 # Funciones ---------------------------------------

comparacion_frecuencias_ui <- function(ns) {
  
  tagList(
    fluidRow(
      column(
        width = 12, 
        tags$h4("Diferencias de frecuencia"),
        DT::dataTableOutput(ns("diferencias_frecuencias")),
        tags$br(),
        tags$h4("Diferencias de frecuencia con costos medios"),
        DT::dataTableOutput(ns("diferencias_frecuencias_x_cme")),
        tags$br(),
        tags$h4("Diferencias de frecuencia en porcentaje"),
        DT::dataTableOutput(ns("diferencias_frecuencias_porcentaje"))
      )
    )
  )
  
}

comparacion_frecuencias <- function(frecuencias, nota_tecnica, agrupador) {
  
  style_interval <-ifelse(
    test = (Sys.getenv("NT_MODO_IPS") != "") %>% rep(2),
    yes = c("rgb(145, 255, 145)", "rgb(255, 145, 145)"),
    no = c("rgb(255, 145, 145)", "rgb(145, 255, 145)"))

  comparacion_frecs <- comparar_nt_frecuencias(
    frecuencias = frecuencias,
    nota_tecnica = nota_tecnica,
    agrupador = agrupador
  )
  
  comparacion_frecs_dt <- datatable(
    data = comparacion_frecs,
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Diferencia valor total" = "total_valor",
      "Diferencia de frecuencia total" = "total",
      "Costo medio" = "cm",
      "Frecuencia a mes" = "frec_mes",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    extensions = c('FixedColumns'),
    selection = 'none',
    options = list(
      pageLength = nrow(comparacion_frecs),
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 4),
      language = list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))) %>%
    formatStyle(columns = 1:ncol(comparacion_frecs), backgroundColor = 'white') %>%
    formatCurrency(
      table = .,
      columns = c("Valor a mes", "Diferencia valor total", "Costo medio"),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatRound(2, dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = 5:ncol(comparacion_frecs),
      backgroundColor = styleInterval(
        cuts = 0,
        values = style_interval
      ))
  
  comparacion_x_cme <- comparar_nt_frecuencias(
    frecuencias = frecuencias,
    nota_tecnica = nota_tecnica,
    agrupador = agrupador,
    indicador = "diff_cm")
  
  comparacion_x_cme_dt <- datatable(
    data = comparacion_x_cme,
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Diferencia valor total" = "total",
      "Costo medio" = "cm",
      "Frecuencia a mes" = "frec_mes",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    extensions = c('FixedColumns'),
    selection = 'none',
    options = list(
      pageLength = nrow(comparacion_x_cme),
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 4),
      language = list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))) %>%
    formatStyle(columns = 1:ncol(comparacion_x_cme), backgroundColor = 'white') %>%
    formatCurrency(
      table = .,
      columns = 3:ncol(comparacion_x_cme),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatRound(2, dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = 5:ncol(comparacion_x_cme),
      backgroundColor = styleInterval(
        cuts = 0,
        values = style_interval
      ))
  
  comparacion_porcentaje <- comparar_nt_frecuencias(
    frecuencias = frecuencias,
    nota_tecnica = nota_tecnica,
    agrupador = agrupador,
    indicador = "perc")
  
  comparacion_porcentaje_dt <- datatable(
    data = comparacion_porcentaje,
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Costo medio" = "cm",
      "Frecuencia a mes" = "frec_mes",
      "Porcentaje de ejecución medio" = "media",
      "Ejecución media a mes" = "media_valor",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    extensions = c('FixedColumns'),
    selection = 'none',
    options = list(
      pageLength = nrow(comparacion_porcentaje),
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 4),
      language = list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))) %>%
    formatStyle(columns = 1:ncol(comparacion_porcentaje),
                backgroundColor = 'white') %>%
    formatPercentage(
      table = .,
      columns = c(5:(ncol(comparacion_porcentaje) - 1)),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatCurrency(
      table = .,
      columns = c(3, 4, ncol(comparacion_porcentaje)),
      dec.mark = ",", mark = ".", digits = 0)  %>%
    formatRound(2, dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = 5:(ncol(comparacion_porcentaje) - 1),
      backgroundColor = styleInterval(
        cuts = 1,
        values = style_interval
      ))
  
  return(list(comparacion_frecs = comparacion_frecs,
              comparacion_frecs_dt = comparacion_frecs_dt,
              comparacion_x_cme = comparacion_x_cme,
              comparacion_x_cme_dt = comparacion_x_cme_dt,
              comparacion_porcentaje = comparacion_porcentaje,
              comparacion_porcentaje_dt = comparacion_porcentaje_dt))
  
}
