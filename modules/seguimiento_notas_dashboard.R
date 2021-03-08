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
              tags$br(),
              tabsetPanel(
                tabPanel(
                  title = "Frecuencias",
                  tags$br(),
                  fluidRow(
                    column(width = 4, uiOutput(ns("frecuencias_resumen"))),
                    column(
                      width = 8,
                      plotlyOutput(ns("frecuencias_plot"), height = "450px") %>%
                        withSpinner())
                  ),
                  tags$hr(),
                  tags$br(),
                  tags$h4("Ejecución:"),
                  DT::dataTableOutput(ns("frecuencias_total")) %>% withSpinner(),
                  tags$hr(),
                  tags$br(),
                  tags$h4("Diferencias de frecuencia:"),
                  DT::dataTableOutput(ns("diferencias_frecuencias")) %>%
                    withSpinner(),
                  tags$hr(),
                  tags$br(),
                  tags$h4("Diferencias de frecuencia con costos medios:"),
                  DT::dataTableOutput(ns("diferencias_frecuencias_x_cme")) %>% 
                    withSpinner(),
                  tags$hr(),
                  tags$br(),
                  tags$h4("Diferencias de frecuencia en porcentaje:"),
                  DT::dataTableOutput(ns("diferencias_frecuencias_porcentaje")) %>%
                    withSpinner(),
                  tags$hr()
                ),
                tabPanel(
                  title = "Valor facturado",
                  tags$br(),
                  fluidRow(
                    column(width = 4, uiOutput(ns("valor_fac_resumen"))),
                    column(
                      width = 8,
                      plotlyOutput(ns("valor_fac_plot"), height = "450px") %>%
                        withSpinner())
                  ),
                  tags$hr(),
                  tags$br(),
                  tags$h4("Ejecución:"),
                  DT::dataTableOutput(ns("valor_fac_total")) %>% withSpinner(),
                  tags$hr(),
                  tags$br(),
                  tags$h4("Diferencias de valor:"),
                  DT::dataTableOutput(ns("diferencias_valor_fac")) %>%
                    withSpinner(),
                  tags$hr(),
                  tags$br(),
                  tags$h4("Diferencias de valor en porcentaje:"),
                  DT::dataTableOutput(ns("diferencias_valor_fac_porcentaje")) %>%
                    withSpinner(),
                  tags$hr()
                ),
                tabPanel(
                  title = "Costos medios"
                )
              )
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
        agrupadores_items = NULL,
        frecs = list()
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
        if (input$comparar_cambiar_perfil) {
          opciones$perfil_selected <- NULL
          opciones$perfil_selected <-
            opciones$notas_tecnicas_lista[[input$board_select]][["perfil"]]
        }
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
        
        comparar$nt_name <- input$board_select
        
        if (!is.null(opciones$colnames) && !is.null(input$comparar_agrupador) &&
            input$comparar_agrupador %notin% c("", "Ninguno")) {
          comparar$frecs <- list()
          comparar$val_fact <- list()
          agrupador <- input$comparar_agrupador
          comparar_col_valor <- input$comparar_col_valor
          tryCatch(
            expr = {
              if (!is.null(comparar$agrupadores_items)) {
                comparar$datos$frecuencias_tabla <- frecuencias_jerarquia(
                  data = opciones$tabla,
                  columnas =      agrupador,
                  columna_fecha = "fecha_prestacion",
                  columna_sep =   NULL,
                  columna_suma = comparar_col_valor,
                  nivel_1 = input$episodios_jerarquia_nivel_1_order,
                  nivel_2 = input$episodios_jerarquia_nivel_2_order,
                  nivel_3 = input$episodios_jerarquia_nivel_3_order,
                  nivel_4 = input$episodios_jerarquia_nivel_4_order)[["descriptiva"]]
                
                comparar$datos$valor_factura_tabla <- opciones$tabla %>%
                  group_by(!!!rlang::syms(comparar_col_valor)) %>%
                  mutate(
                    ais_mes  = month(max(fecha_prestacion, na.rm = TRUE)),
                    ais_anio =  year(max(fecha_prestacion, na.rm = TRUE))) %>%
                  ungroup() %>%
                  episodios_jerarquia(
                    data = .,
                    columnas = agrupador,
                    columna_sep = c("ais_anio", "ais_mes"),
                    columna_valor = opciones$valor_costo,
                    columna_suma = comparar_col_valor,
                    nivel_1 = input$episodios_jerarquia_nivel_1_order,
                    nivel_2 = input$episodios_jerarquia_nivel_2_order,
                    nivel_3 = input$episodios_jerarquia_nivel_3_order,
                    nivel_4 = input$episodios_jerarquia_nivel_4_order
                  )
                
              } else {
                comparar$datos$frecuencias_tabla <- frecuencias(
                  columna_fecha = "fecha_prestacion",
                  data = opciones$tabla,
                  agrupador = agrupador,
                  columna_suma = comparar_col_valor,
                  prestaciones = (input$descriptiva_unidades == "prestacion")
                )
                
                comparar$datos$valor_factura_tabla <- opciones$tabla %>%
                  mutate(ais_mes  = month(fecha_prestacion),
                         ais_anio =  year(fecha_prestacion)) %>%
                  descriptiva(
                    columnas = c(agrupador, "ais_anio", "ais_mes"),
                    columna_suma = comparar_col_valor,
                    columna_valor = opciones$valor_costo,
                    prestaciones = (input$descriptiva_unidades == "prestacion")
                  )
              }
            
              nt_test <- nt_opciones$datos
              
              comparar$frecs <- comparacion_frecuencias(
                frecuencias_tabla = comparar$datos$frecuencias_tabla,
                nota_tecnica = nt_test,
                agrupador = agrupador
              )

              comparar$valor_fac <- comparacion_valor_facturado(
                descriptiva_tabla = 
                  comparar$datos$valor_factura_tabla[["descriptiva"]] %>%
                    ungroup(),
                nota_tecnica = nt_test,
                agrupador = agrupador,
                col_mes = "ais_mes", col_anio = "ais_anio"
              )
              
            }
          )
        }
        
      })
      
      output$frecuencias_resumen <- renderUI({comparar$frecs$totales})
      
      output$frecuencias_plot <- renderPlotly({
        comparar$frecs$plot_valor_acumulado
      })
      
      output$frecuencias_total <-
        DT::renderDataTable({comparar$frecs$frecuencias_original_dt})
      
      output$diferencias_frecuencias <- 
        DT::renderDataTable({comparar$frecs$comparacion_frecs_dt})
      
      output$diferencias_frecuencias_x_cme <- 
        DT::renderDataTable({comparar$frecs$comparacion_x_cme_dt})
      
      output$diferencias_frecuencias_porcentaje <- 
        DT::renderDataTable({comparar$frecs$comparacion_porcentaje_dt})

      output$valor_fac_resumen <- renderUI({comparar$valor_fac$totales})
      
      output$valor_fac_plot <- renderPlotly({
        comparar$valor_fac$plot_valor_acumulado
      })

      output$valor_fac_total <-
        DT::renderDataTable({comparar$valor_fac$comparacion_suma_dt})

      output$diferencias_valor_fac <-
        DT::renderDataTable({comparar$valor_fac$comparacion_diff_dt})

      output$diferencias_valor_fac_porcentaje <-
        DT::renderDataTable({comparar$valor_fac$comparacion_porcentaje_dt})
  
      output$comparar_descargar_xlsx <- downloadHandler(
        filename = function() {
          paste0("Seguimiento de ", comparar$nt_name, ".xlsx")
        },
        content = function(file) {
          write_xlsx(
            x = list(
              "Nota tecnica" = nt_opciones$datos,
              "Frecuencias" = comparar$frecs$frecuencias_original,
              "Diferencias frecuencias" = comparar$frecs$comparacion_frecs,
              "Diferencias valor por CM" = comparar$frecs$comparacion_x_cme,
              "Diferencias frencuencias %" = comparar$frecs$comparacion_porcentaje,
              "Valor facturado" = comparar$valor_fac$comparacion_suma,
              "Diferencias valor facturado" = comparar$valor_fac$comparacion_diff,
              "Diferencias valor facturado %" =  comparar$valor_fac$comparacion_porcentaje
            ),
            path = file
          )
        },
        contentType = "xlsx"
      )
      
  })
  
}

 # Funciones ---------------------------------------

comparacion_frecuencias <- function(frecuencias_tabla, nota_tecnica, agrupador) {
  
  style_interval <-ifelse(
    test = (Sys.getenv("NT_MODO_IPS") != "") %>% rep(2),
    yes = c("rgb(145, 255, 145)", "rgb(255, 145, 145)"),
    no = c("rgb(255, 145, 145)", "rgb(145, 255, 145)"))

  comparacion_frecs <- comparar_nt_frecuencias(
    frecuencias = frecuencias_tabla,
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
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(comparacion_frecs),
      scrollX = TRUE,
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
    frecuencias = frecuencias_tabla,
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
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(comparacion_x_cme),
      scrollX = TRUE,
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
    frecuencias = frecuencias_tabla,
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
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(comparacion_porcentaje),
      scrollX = TRUE,
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
  
  frecuencias_original <- frecuencias_tabla %>%
    rename(agrupador = !!as.name(agrupador)) %>%
    inner_join(nota_tecnica %>%
                 select(agrupador, frec_mes, cm)) %>%
    group_by(agrupador, frec_mes, cm) %>%
    mutate(valor_mes = frec_mes * cm) %>%
    group_by(agrupador, frec_mes, cm, valor_mes) %>%
    mutate(across(.fns = replace_na, replace = 0)) %>%
    mutate(., total = rowSums(across(), na.rm = TRUE)) %>%
    mutate(., total_valor = total * cm) %>%
    relocate(agrupador, frec_mes, cm, valor_mes)
  
  frecuencias_original_dt <- datatable(
    data = frecuencias_original,
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Valor total" = "total_valor",
      "Frecuencia total" = "total",
      "Costo medio" = "cm",
      "Frecuencia a mes" = "frec_mes",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(frecuencias_original),
      scrollX = TRUE,
      language = list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))) %>%
    formatStyle(columns = 1:ncol(frecuencias_original), backgroundColor = 'white') %>%
    formatCurrency(
      table = .,
      columns = c("Valor a mes", "Valor total", "Costo medio"),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatRound(c(2, ncol(frecuencias_original) - 1),
                dec.mark = ",", mark = ".", digits = 0)
  
  numero_meses <- ncol(frecuencias_tabla) - 1
  valor_a_ejecutar <- sum(nota_tecnica$valor_mes) * numero_meses
  valor_ejecutado <- sum(frecuencias_original$total_valor)
  
  totales <- list(
    "Valor a ejecutar:" = formatAsCurrency(valor_a_ejecutar),
    "Valor ejecutado con costo medio:" = formatAsCurrency(valor_ejecutado),
    "Diferencia de valor total:" = formatAsCurrency(valor_a_ejecutar - 
                                                      valor_ejecutado),
    "Porcentaje del valor ejecutado:" = formatAsPerc(
      100 * valor_ejecutado / na_if(valor_a_ejecutar, 0)))
  
  totales_ui <- purrr::map2(
    .x = totales, .y = names(totales),
    .f = function(x, y) {
      tagList(
        tags$b(y),
        tags$p(x)
      )
    }) %>%
    tagList()
  
  meses <- frecuencias_tabla %>%
    select(-c(rlang::sym(agrupador))) %>%
    colnames() %>%
    mes_spanish_inv() %>%
    as.numeric()
  
  minimo_mes <- min(meses)
  maximo_mes <- max(meses)
  
  meses_completos <- tibble("ais_mes_anio" = seq(minimo_mes, maximo_mes)) %>%
    mutate(ais_anio = substr(ais_mes_anio, 1, 4) %>% as.numeric(),
           ais_mes  = substr(ais_mes_anio, 5, 6) %>% as.numeric()) %>%
    filter(ais_mes >= 1 & ais_mes <= 12) %>%
    rename(mes_anio_num = ais_mes_anio)
  
  valor_acumulado <- meses_completos %>%
    left_join(
      comparar_nt_frecuencias(
        frecuencias = frecuencias_tabla,
        nota_tecnica = nota_tecnica,
        agrupador = agrupador,
        indicador = "cm") %>%
        ungroup() %>%
        select(-c(cm, frec_mes, valor_mes)) %>%
        pivot_longer(
          cols = -c(agrupador),
          names_to = "mes_anio",
          values_to = "valor") %>%
        group_by(mes_anio) %>%
        summarise(suma = sum(valor, na.rm = TRUE)) %>%
        mutate(mes_anio_num = mes_spanish_inv(mes_anio))
    ) %>%
    mutate(numero_meses = 1:nrow(.), suma = replace_na(suma, 0),
           mes_anio = mes_spanish_juntos(mes_anio_num),
           mes_anio_num = do.call(purrr::map(
           .x = as.Date(paste(ais_anio, ais_mes, "01", sep = "-")),
           .f = function(x) last(seq(x, length = 2, by = "months") - 1)),
           what = "c")) %>%
    arrange(mes_anio_num) %>%
    ungroup() %>%
    mutate(valor_acumulado = cumsum(suma), 
           valor_mes_esperado = as.double(sum(nota_tecnica$valor_mes)),
           numero_meses = 1:nrow(.),
           valor_a_ejecutar = valor_mes_esperado * numero_meses)
  
  plot_valor_acumulado <- valor_acumulado %>%
    plot_ly(
      x = ~mes_anio_num,
      y = ~valor_acumulado, 
      name = "Valor ejecutado",
      type = "scatter", mode = "lines+markers"
    ) %>%
    add_trace(
      y = ~valor_a_ejecutar,
      name = "Valor a ejecutar",
      mode = "lines",
      line = list(color = 'rgb(205, 12, 24)', dash = 'dash'),
      fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)'
    ) %>%
    layout(
      legend = list(x = 0.1, y = 0.9),
      xaxis = list(title = "Mes"),
      yaxis = list(title = "Suma",
                   tickformat = ",.2f")
    ) %>%
    config(locale = "es")

  
  return(list(comparacion_frecs = comparacion_frecs,
              comparacion_frecs_dt = comparacion_frecs_dt,
              comparacion_x_cme = comparacion_x_cme,
              comparacion_x_cme_dt = comparacion_x_cme_dt,
              comparacion_porcentaje = comparacion_porcentaje,
              comparacion_porcentaje_dt = comparacion_porcentaje_dt,
              frecuencias_original = frecuencias_original,
              frecuencias_original_dt = frecuencias_original_dt,
              totales = totales_ui,
              valor_acumulado = valor_acumulado,
              plot_valor_acumulado = plot_valor_acumulado
              ))
  
}

comparacion_valor_facturado <- function(
  descriptiva_tabla, nota_tecnica, agrupador, col_mes = "ais_mes",
  col_anio = "ais_anio") {
  
  descriptiva_tabla <- descriptiva_tabla %>%
    mutate(ais_mes_anio = !!rlang::sym(col_anio) * 100 + !!rlang::sym(col_mes))
  
  style_interval <-ifelse(
    test = (Sys.getenv("NT_MODO_IPS") != "") %>% rep(2),
    yes = c("rgb(145, 255, 145)", "rgb(255, 145, 145)"),
    no = c("rgb(255, 145, 145)", "rgb(145, 255, 145)"))
  
  comparaciones <- purrr::map(
    .x = list("suma" = "suma", "diff" = "diff", "perc" = "perc"),
    .f = comparar_nt_valor_factura,
    descriptiva_tabla = descriptiva_tabla,
    nota_tecnica = nota_tecnica,
    agrupador = agrupador,
    col_anio = col_anio,
    col_mes = col_mes
  )
  
  comparacion_suma_dt <- datatable(
    data = comparaciones[["suma"]],
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Suma valor total" = "total",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(comparaciones[["suma"]]),
      scrollX = TRUE,
      language = list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))) %>%
    formatCurrency(
      columns = 2:ncol(comparaciones[["suma"]]),
      dec.mark = ",", mark = ".", digits = 0)
  
  comparacion_diff_dt <- datatable(
    data = comparaciones[["diff"]],
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Diferencia de valor total" = "total",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(comparaciones[["diff"]]),
      scrollX = TRUE,
      language = list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))) %>%
    formatCurrency(
      columns = 2:ncol(comparaciones[["diff"]]),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = c(3:ncol(comparaciones[["diff"]]),
                  ncol(comparaciones[["diff"]])),
      backgroundColor = styleInterval(
        cuts = 0,
        values = style_interval
      ))
  
  comparacion_perc_dt <- datatable(
    data = comparaciones[["perc"]],
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Porcentaje de ejecución medio" = "media",
      "Ejecución media a mes" = "media_valor",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(comparaciones[["perc"]]),
      scrollX = TRUE,
      language = list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))) %>%
    formatPercentage(
      columns = 3:(ncol(comparaciones[["perc"]]) - 1),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = 3:(ncol(comparaciones[["perc"]]) - 1),
      backgroundColor = styleInterval(
        cuts = 1,
        values = style_interval
      )) %>%
    formatCurrency(
      columns = c(2, ncol(comparaciones[["perc"]])),
      dec.mark = ",", mark = ".", digits = 0)
  
  minimo_mes <- min(pull(descriptiva_tabla, ais_mes_anio))
  maximo_mes <- max(pull(descriptiva_tabla, ais_mes_anio))
  valor_mes <- sum(nota_tecnica$valor_mes)
   
  meses_completos <- tibble("ais_mes_anio" = seq(minimo_mes, maximo_mes)) %>%
    mutate(ais_anio = substr(ais_mes_anio, 1, 4) %>% as.numeric(),
           ais_mes  = substr(ais_mes_anio, 5, 6) %>% as.numeric()) %>%
    filter(ais_mes >= 1 & ais_mes <= 12) %>%
    select(-ais_mes_anio)
  
  valor_acumulado <- meses_completos %>%
    left_join(
      descriptiva_tabla %>%
        rename(agrupador = !!as.name(agrupador)) %>%
        left_join(nota_tecnica %>%
                     select(agrupador)) %>%
        arrange(!!rlang::sym(col_anio), !!rlang::sym(col_mes)) %>%
        group_by(!!rlang::sym(col_anio), !!rlang::sym(col_mes)) %>%
        summarise(Suma = sum(Suma, na.rm = TRUE)) %>%
        select(!!!rlang::syms(c(col_anio, col_mes)), Suma),
      by = c("ais_mes" = col_mes, "ais_anio" = col_anio)) %>%
    mutate(numero_meses = 1:nrow(.),
           mes_anio_num = ais_anio * 100 + ais_mes,
           mes_anio = mes_spanish_juntos(mes_anio_num),
           mes_anio_num = do.call(purrr::map(
             .x = as.Date(paste(ais_anio, ais_mes, "01", sep = "-")),
             .f = function(x) last(seq(x, length = 2, by = "months") - 1)),
             what = "c")) %>%
    mutate(Suma = replace_na(Suma, 0),
           valor_acumulado = cumsum(Suma), valor_mes_esperado = valor_mes,
           valor_a_ejecutar = valor_mes * as.double(numero_meses))
  
  plot_valor_acumulado <- valor_acumulado %>%
    plot_ly(
      x = ~mes_anio_num,
      y = ~valor_acumulado, 
      name = "Valor ejecutado",
      type = "scatter", mode = "lines+markers"
    ) %>%
    add_trace(
      y = ~valor_a_ejecutar,
      name = "Valor a ejecutar",
      mode = "lines",
      line = list(color = 'rgb(205, 12, 24)', dash = 'dash'),
      fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)'
    ) %>%
    config(locale = "es") %>%
    layout(
      legend = list(x = 0.1, y = 0.9),
      xaxis = list(title = "Mes"),
      yaxis = list(title = "Suma",
                   tickformat = ",.2f")
    ) 
  
  numero_meses <- nrow(valor_acumulado)
  valor_a_ejecutar <- numero_meses * as.double(valor_mes)
  valor_ejecutado <- sum(valor_acumulado$Suma, na.rm = TRUE)
  
  totales <- list(
    "Valor a ejecutar:" = formatAsCurrency(valor_a_ejecutar),
    "Valor facturado:" = formatAsCurrency(valor_ejecutado),
    "Diferencia de valor total:" = formatAsCurrency(valor_a_ejecutar - 
                                                      valor_ejecutado),
    "Porcentaje del valor ejecutado:" = formatAsPerc(
      100 * valor_ejecutado / na_if(valor_a_ejecutar, 0)))
  
  totales_ui <- purrr::map2(
    .x = totales, .y = names(totales),
    .f = function(x, y) {
      tagList(
        tags$b(y),
        tags$p(x)
      )
    }) %>%
    tagList()
  
  return(list(comparacion_suma = comparaciones[["suma"]],
              comparacion_suma_dt = comparacion_suma_dt,
              comparacion_diff = comparaciones[["diff"]],
              comparacion_diff_dt = comparacion_diff_dt,
              comparacion_porcentaje = comparaciones[["perc"]],
              comparacion_porcentaje_dt = comparacion_perc_dt,
              valor_acumulado = valor_acumulado,
              plot_valor_acumulado = plot_valor_acumulado,
              totales = totales_ui
  ))
  
  
}
