#
# Analítica Integrada Salud
#
# Derechos de autor 2021 por MD&CO Consulting Group (NIT 901.119.781-5)
# Copyright (C) 2021 by MD&CO Consulting Group
#
# Este programa es software libre: puede redistribuirlo o modificarlo bajo
# los términos de la licencia Affero General Public License tal cual
# publicada por la Free Software Foundation, sea la versión 3 de la licencia
# o cualquier versión posterior. Este programa se distribuye SIN GARANTÍA
# EXPERSA O IMPLÍCITA, INCLUIDAS LAS DE NO INFRACCIÓN, COMERCIABILIDAD O
# APTITUD PARA UN PROPÓSITO PARTICULAR. Referir a la
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) para más detalles.
#
  
seguimiento_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        width = 4,
        tabsetPanel(
          tabPanel(
            title = "Generales",
            tags$br(),
            selectizeInput(
              inputId =  ns("nota_tecnica"),
              label = "Comparar con nota técnica:",
              choices = NULL,
              multiple = FALSE),
            agrupadores_widget(
              id = id,
              separadores = FALSE
            ),
            tags$br(),
            actionButton(ns("exe"), "Generar"),
            tags$br()
          ),
          tabPanel(
            title = "Descargas",
            tags$br(),
            downloadButton(
              outputId = ns("descargar_informe"),
              label = "Informe completo",
              width = "100%"
            )
          )
        )
      ),
      box(
        width = 8,
        fluidRow(
          column(width = 4, uiOutput(ns("frecs_resumen"))),
          column(
            width = 8,
            plotlyOutput(ns("frecs_plot"), height = "450px") %>%
              withSpinner())
        ),
        tags$hr(),
        tags$br(),
        tags$h4("Nota técnica"),
        DT::dataTableOutput(ns("nota_tecnica")),
        tags$hr(),
        tags$br(),
        conditionalPanel(
          condition = '$("#limites_exist").attr("class") == "limites_exist"',
          tags$h4("Frecuencia a mes contratada ajustada"),
          DT::dataTableOutput(ns("frecs_efectiva")) %>%
            withSpinner(),
          tags$hr(),
          tags$br(),
          tags$h4("Total pagador"),
          DT::dataTableOutput(ns("frecs_pagador")) %>%
            withSpinner(),
          tags$hr(),
          tags$br(),
          tags$h4("Ajuste al valor del contrato"),
          DT::dataTableOutput(ns("frecs_ajuste")) %>%
            withSpinner(),
          tags$hr(),
          tags$br()
        ),
        uiOutput(ns("limit_check")),
        tabsetPanel(
          tabPanel(
            title = "Resultados frecuencias",
            tags$hr(),
            tags$br(),
            tags$h4("Ejecución de frecuencias por mes"),
            DT::dataTableOutput(ns("frecs_base")) %>% withSpinner(),
            tags$hr(),
            tags$br(),
            tags$h4("Ejecución de frecuencias por costo medio"),
            DT::dataTableOutput(ns("frecs_x_cm")) %>%
              withSpinner(),
            tags$hr(),
            tags$br(),
            tags$h4("Diferencia ejecución"),
            DT::dataTableOutput(ns("frecs_diff")) %>%
              withSpinner()
          ),
          tabPanel(
            title = "Resultados de valor facturado",
            tags$hr(),
            tags$br(),
            tags$h4("Valor facturado por mes"),
            DT::dataTableOutput(ns("valor_base")) %>% withSpinner(),
            tags$hr(),
            tags$br(),
            tags$h4("Diferencia ejecución"),
            DT::dataTableOutput(ns("valor_diff")) %>%
              withSpinner()
          ),
          tabPanel(
            title = "Costo medio efectivo",
            tags$hr(),
            tags$br(),
            conditionalPanel(
              condition = 
                '$("#limites_exist").attr("class") == "limites_exist"',
              selectizeInput(
                inputId = ns("nt_costo_medio"),
                label = "Agrupador",
                choices = NULL
              ),
              plotlyOutput(
                outputId = ns("costos_medios"),
                height = "370px",
                width = "100%"
              )
            )
          )
        )
      )
    )
  )
}

seguimiento_server <- function(id, opciones, cache) {
  moduleServer(id = id, module = function(input, output, session) {

    ns <- NS(id)

    style_interval <- ifelse(
      test = (Sys.getenv("NT_MODO_IPS") != "") %>% rep(2),
      yes = c("rgb(145, 255, 145)", "rgb(255, 190, 100)"),
      no = c("rgb(255, 190, 100)", "rgb(145, 255, 145)"))

    episodios <- reactiveValues(
      nt_current = data.frame(),
      frecuencias = data.frame(),
      nt_selected = "Ninguno",
      descriptiva = data.frame(),
      comparar_nt = data.frame(),
      frecs_base = data.frame(),
      frecs_x_cm = data.frame(),
      frecs_efectiva = data.frame(),
      frecs_pagador = data.frame(),
      frecs_diff = data.frame(),
      frecs_ajuste = data.frame(),
      valor_base = data.frame(),
      valor_diff = data.frame(),
      limites = FALSE)


    observe({
      updateSelectizeInput(
        inputId = "nota_tecnica",
        choices = c("Ninguno", opciones$notas_tecnicas$nt),
        selected = episodios$nt_selected
        )
    }) %>%
      bindEvent(opciones$notas_tecnicas)
    
    observe({
      updateSelectizeInput(
        inputId = "nt_costo_medio",
        choices = opciones$notas_tecnicas[
          which(nt == episodios$nt_selected & 
                  (!is.na(frec_mes_max) | !is.na(frec_mes_min)))
          ]$agrupador
      )
      
    }) %>%
    bindEvent(episodios$nt_selected)

    observe({
      episodios$nt_selected <- input$nota_tecnica
    })

    # Se observa que el usuario haga click en los titulos de las unidades
    # de conteo en el widget de jerarquia.
    # De esta manera se pueden mover los diferentes agrupadores de manera
    # sencilla entre unidades.

    episodios_jerarquia_server(
      id = id,
      episodios = episodios,
      cache = cache,
      opciones = opciones)

    # Generar descriptiva
    observeEvent(input$exe, {
      withProgress(message = "Calculando descriptiva", {
        if (opciones$datos_cargados &&
          input$agrupador %notin% c("", "Ninguno")) {
          agrupador <- input$agrupador
          if (input$episodios) episodios_col_rel <- input$episodios_col_rel
          separadores <- c("ais_mes", "ais_anio")
          episodios$agrupador <- agrupador
          episodios$separadores <- separadores
          # Si se va a generar por episodios
          if (input$episodios) {
            # Se genera un ID para el cache y se busca si ya ha sido
            # generado en el pasado
            episodios$descriptiva <- cache_call(
              fn = episodios_jerarquia,
              cache = cache,
              cache_params = list(
                columnas =      agrupador,
                columna_valor = opciones$valor_costo,
                columna_sep =   separadores,
                columna_suma =  episodios_col_rel,
                nivel_1 = input$episodios_jerarquia_nivel_1_order$text,
                nivel_2 = input$episodios_jerarquia_nivel_2_order$text,
                nivel_3 = input$episodios_jerarquia_nivel_3_order$text,
                nivel_4 = input$episodios_jerarquia_nivel_4_order$text,
                frec_cantidad = opciones$cantidad),
              non_cache_params = list(
                data = opciones$tabla %>%
                  mutate(
                    ais_mes = month(fecha_prestacion),
                    ais_anio = year(fecha_prestacion),
                    ais_mes_anio = ais_anio * 100 + ais_mes)),
              prefix = "desc_ep_seg",
              cache_depends = opciones$tabla_query
            )[["descriptiva"]]
          }
          if (!input$episodios) {
            # Si se va a generar de manera tradicional
            # Se checkea un ID para el cache y se busca si ha sido
            # generada en el pasado
            episodios$descriptiva <- cache_call(
              fn = descriptiva,
              cache = cache,
              cache_params = list(
                columnas = c(agrupador, separadores),
                columna_valor = opciones$valor_costo,
                columna_suma = input$unidades,
                prestaciones = (input$unidades == "prestacion"),
                frec_cantidad = opciones$cantidad),
              non_cache_params = list(
                data = opciones$tabla %>%
                  mutate(
                    ais_mes = month(fecha_prestacion),
                    ais_anio = year(fecha_prestacion),
                    ais_mes_anio = ais_anio * 100 + ais_mes)),
              prefix = "desc_seg",
              cache_depends = opciones$tabla_query
            )[["descriptiva"]]
          }
          
          tryCatch(
            expr = {
              episodios$descriptiva <- episodios$descriptiva %>% 
                descriptiva_timeseries(agrupador = c(agrupador))  
            },
            error = function(e) {
              purrr::map(e, message)
              showNotification(
                "Error: El rango de fecha seleccionado es invalido."
              )
            }
          )
        }
      })
    })

    observe({
      if (input$nota_tecnica %notin% c("", "Ninguno")) {
        nt <- opciones$notas_tecnicas %>%
          filter(nt == input$nota_tecnica) %>%
          rename(cod_nt = nt)
        episodios$nt_current <- nt
      } else {
        episodios$nt_current <- data.frame()
      }
    }) %>%
      bindEvent(input$exe)

    # Se espera a que se generen las descripciones por frecuencia o valor
    # para empezar a hacer la comparación con la nota técnica
    observe({
      tryCatch(
        expr = {
          if (nrow(episodios$nt_current) > 0) {
            if (nrow(episodios$descriptiva) > 0) {
              episodios$limites <- episodios$nt_current %>%
                ungroup() %>%
                summarise(
                  limit_check = !(all(is.na(frec_mes_min)) &&
                    all(is.na(frec_mes_max)))
                ) %>%
                pull(limit_check)
              episodios$comparar_nt <- comparar_nt(
                timeseries = episodios$descriptiva,
                nota_tecnica = episodios$nt_current,
                agrupador = episodios$agrupador)
            }
          } else {
            showNotification("Seleccionar una nota técnica")
          }
        },
        error = function(e) {
          purrr::map(e, message)
          showNotification(
            "Error: validar que la nota técnica corresponda a los datos"
          )
        }
      )
    }) %>%
    bindEvent(
      episodios$descriptiva,
      episodios$nt_current)

    observe({
      if (input$nota_tecnica %notin% c("", "Ninguno")) {
        perfil_nt <-
          opciones$notas_tecnicas_lista[[input$nota_tecnica]][["perfil"]]
        if (is.null(perfil_nt)) perfil_nt <- "Ninguno"
        if (input$episodios &&
            perfil_nt %in% names(opciones$perfil_lista)) {
          if (!opciones$perfil_enable) {
            confirmSweetAlert(
              session = session,
              inputId = ns("cambiar_perfil"),
              title = "Utilizar perfil",
              text = "Esta nota técnica tiene un perfil asignado.
                      ¿Desea utilizarlo?",
              btn_labels = c("Cancelar", "Utilizar")
            )
          } else if (opciones$perfil_enable &&
                     opciones$perfil_selected != perfil_nt) {
            confirmSweetAlert(
              session = session,
              inputId = ns("cambiar_perfil"),
              title = "Utilizar perfil",
              text = "Esta nota técnica tiene un perfil asignado diferente al
                      seleccionado.
                      ¿Desea cambiarlo?",
              btn_labels = c("Cancelar", "Carmbiar")
            )
          }
        }
      }
    }) %>%
    bindEvent(input$episodios, input$nota_tecnica)

    observe({
      if (input$cambiar_perfil) {
        opciones$perfil_selected <- NULL
        opciones$perfil_selected <-
          opciones$notas_tecnicas_lista[[input$nota_tecnica]][["perfil"]]
      }
    }) %>%
    bindEvent(input$cambiar_perfil)

    output$limit_check <- renderUI({
      tags$div(
        id = "limites_exist",
        class = ifelse(
          test = episodios$limites,
          yes = "limites_exist",
          no = "limites_non"
        )
      )
    })

    output$nota_tecnica <- DT::renderDataTable({
      if (nrow(episodios$nt_current) > 0) {
        datatable(
          episodios$nt_current,
          colnames = c(
            "Nota técnica" = "cod_nt",
            "Agrupador" = "agrupador",
            "Frecuencia a mes" = "frec_mes",
            "Costo Medio" = "cm",
            "Frecuencia per capita" = "frecuencia_pc",
            "Valor mes" = "valor_mes",
            "Límite inferior" = "frec_mes_min",
            "Límite superior" = "frec_mes_max"),
        rownames = FALSE,
        selection = "none",
        extensions = c("FixedColumns"),
        options = list(
          dom = "t",
          scrollCollapse = TRUE,
          fixedColumns = list(leftColumns = 2),
          scrollY = "300px",
          pageLength = 99999,
          scrollX = TRUE,
          language = list(
            url = dt_spanish))
        ) %>%
        formatStyle(columns = TRUE, backgroundColor = "white") %>%
        formatRound(
          columns = -c(1, 2),
          dec.mark = ",", mark = ".", digits = 3
        )
      }
    })

    output$frecs_resumen <- renderUI({
      if (nrow(episodios$comparar_nt) > 0) {
        valor_contratado <- sum(episodios$comparar_nt$valor_contratado)
        valor_contratado_base <-
          sum(episodios$comparar_nt$valor_contratado_base)
        valor_ejecutado_cm <-
          sum(episodios$comparar_nt$valor_ejecutado_cm)
        valor_ejecutado_fac <-
          sum(episodios$comparar_nt$Suma, na.rm = TRUE)

        totales <- list(
          "Valor base de nota técnica" =
            formatAsCurrency(valor_contratado_base),
          "Valor de nota técnica ajustado:" =
            formatAsCurrency(valor_contratado),
          "Valor ejecutado con frecuencia:" =
            formatAsCurrency(valor_ejecutado_cm),
          "Diferencia de valor con frecuencia:" =
            formatAsCurrency(valor_ejecutado_cm - valor_contratado),
          "Porcentaje del valor ejecutado con frecuencia:" =
            formatAsPerc(100 * valor_ejecutado_cm /
              na_if(valor_contratado, 0)),
          "Valor ejecutado con facturacion" =
            formatAsCurrency(valor_ejecutado_fac),
          "Diferencia de valor con facturación" =
            formatAsCurrency(valor_ejecutado_fac - valor_contratado),
           "Porcentaje del valor ejecutado con facturación:" =
            formatAsPerc(100 * valor_ejecutado_fac /
              na_if(valor_contratado, 0)))

        totales_ui <- purrr::map2(
          .x = totales, .y = names(totales),
          .f = function(x, y) {
            tagList(
              tags$b(y),
              tags$p(x)
            )
          }) %>%
          tagList()
      }
    })

    observe({
      if(nrow(episodios$comparar_nt)>0){
      a <- max(c(0,episodios$comparar_nt$frec_mes_min),na.rm = TRUE)
      b <- unique(episodios$comparar_nt$frec_mes)
      c <- max(c(0,episodios$comparar_nt$frec_mes_max),na.rm = TRUE)
      cm <- unique(episodios$comparar_nt$cm)
      
      episodios$costo_medio_datos <- 
        data.frame("x" = 1:(c+100),
                   "a" = a, 
                   "b" = b,
                   "c" = c,
                   "cm" = cm) %>% 
        mutate("y" = case_when(
          x >= 1 & x <= a ~ (((b*cm)-(cm*(a-x)))/x),
          x >= a & x <= c ~ ((b*cm)/x),
          x >= c          ~ (((b*cm)+(cm*(x-c)))/x)
          )
        ) %>% select(x,y)
      }
    })
    
    output$costos_medios <- renderPlotly({
      if (nrow(episodios$comparar_nt) > 0) {
        episodios$costo_medio_datos %>% 
          plot_ly(
          x = ~x,
          y = ~y,
          name = "Costo medio",
          type = "scatter",
          mode = "lines"
        )  %>% 
          layout(
            legend = list(x = 0.1, y = 0.9),
            xaxis = list(title = "Frecuencia"),
            yaxis = list(title = "Costo",
                         tickformat = ",.2f")
          ) %>%
          config(locale = "es")
      }
    })
    
    output$frecs_plot <- renderPlotly({
      if (nrow(episodios$comparar_nt) > 0) {
        episodios$comparar_nt %>%
          group_by(mes_anio_num) %>%
          summarise(
            valor_contratado = sum(valor_contratado, na.rm = TRUE),
            valor_ejecutado_cm = sum(valor_ejecutado_cm, na.rm = TRUE),
            valor_ejecutado_fac = sum(Suma, na.rm = TRUE)
          ) %>%
          ungroup() %>%
          arrange(mes_anio_num) %>%
          mutate(
            valor_acumulado_cm = cumsum(valor_ejecutado_cm),
            valor_acumulado_fac = cumsum(valor_ejecutado_fac),
            valor_contratado = cumsum(valor_contratado),
            mes_anio_num = ym(mes_anio_num)
          ) %>%
          plot_ly(
            x = ~mes_anio_num,
            y = ~valor_acumulado_cm,
            name = "Valor ejecutado con frecuencias",
            type = "scatter",
            mode = "lines+markers"
          ) %>%
          add_trace(
            y = ~valor_acumulado_fac,
            name = "Valor ejecutado con facturación",
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "green"),
            marker = list(color = "green")
          ) %>%
          add_trace(
            y = ~valor_contratado,
            mode = "lines",
            name = "Valor contratado",
            line = list(color = "rgb(205, 12, 24)", dash = "dash")
          ) %>%
          layout(
            legend = list(x = 0.1, y = 0.9),
            xaxis = list(title = "Mes"),
            yaxis = list(title = "Suma",
                         tickformat = ",.2f")
          ) %>%
          config(locale = "es")
      }
    })

    observe({
      if (nrow(episodios$comparar_nt) > 0) {
        episodios$frecs_base <- episodios$comparar_nt %>%
          group_by(agrupador) %>%
          mutate(
            frec_media = round(mean(Frecuencia, na.rm = TRUE), digits = 2)
          ) %>%
          arrange(mes_anio_num) %>%
          pivot_wider(
            id_cols = c(unidad_conteo, agrupador, frec_mes, frec_media),
            values_from = Frecuencia,
            names_from = mes_anio
          )
      }
    })

    output$frecs_base <- DT::renderDataTable({
      if (nrow(episodios$frecs_base) > 0) {
        episodios$frecs_base %>%
          datatable(
            colnames = c(
              "Unidad de conteo" = "unidad_conteo",
              "Agrupador" = "agrupador",
              "Frecuencia a mes" = "frec_mes",
              "Frecuencia media ejecutada" = "frec_media"),
            rownames = FALSE,
            selection = "none",
            extensions = c("FixedColumns"),
            options = list(
              dom = "t",
              scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 2),
              scrollY = "300px",
              pageLength = 99999,
              scrollX = TRUE,
              language = list(
                url = dt_spanish))) %>%
          formatStyle(columns = TRUE, backgroundColor = "white") %>%
          formatRound(
            columns = -c(1, 2),
            dec.mark = ",", mark = ".", digits = 3
          )
      }
    })

    observe({
      if (nrow(episodios$comparar_nt) > 0) {
        episodios$frecs_x_cm <- episodios$comparar_nt %>%
          group_by(agrupador) %>%
          mutate(
            valor_media = round(
              mean(valor_ejecutado_cm, na.rm = TRUE),
              digits = 2
            )
          ) %>%
          arrange(mes_anio_num) %>%
          pivot_wider(
            id_cols = c(unidad_conteo, agrupador, valor_mes, valor_media),
            values_from = valor_ejecutado_cm,
            names_from = mes_anio
          )
      }
    })

    output$frecs_x_cm <- DT::renderDataTable({
      if (nrow(episodios$frecs_x_cm) > 0) {
        episodios$frecs_x_cm %>%
          datatable(
            colnames = c(
              "Unidad de conteo" = "unidad_conteo",
              "Agrupador" = "agrupador",
              "Valor a mes contratado" = "valor_mes",
              "Valor medio ejecutado" = "valor_media"),
            rownames = FALSE,
            selection = "none",
            extensions = c("FixedColumns"),
            options = list(
              dom = "t",
              scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 2),
              scrollY = "300px",
              pageLength = 99999,
              scrollX = TRUE,
              language = list(
                url = dt_spanish))) %>%
          formatStyle(columns = TRUE, backgroundColor = "white") %>%
          formatCurrency(
            columns = -c(1, 2),
            dec.mark = ",", mark = ".", digits = 0
          )
      }
    })

    observe({
      if (nrow(episodios$comparar_nt) > 0) {
        episodios$frecs_efectiva <- episodios$comparar_nt %>%
          group_by(agrupador) %>%
          arrange(mes_anio_num) %>%
          pivot_wider(
            id_cols = c(unidad_conteo, agrupador, frec_mes,
              frec_mes_min, frec_mes_max),
            values_from = frec_efectiva,
            names_from = mes_anio
          )
      }
    })

    output$frecs_efectiva <- DT::renderDataTable({
      if (nrow(episodios$frecs_efectiva) > 0) {
        episodios$frecs_efectiva %>%
          datatable(
            colnames = c(
              "Unidad de conteo" = "unidad_conteo",
              "Agrupador" = "agrupador",
              "Frecuencia a mes contratada base" = "frec_mes",
              "Límite inferior" = "frec_mes_min",
              "Límite superior" = "frec_mes_max"),
            rownames = FALSE,
            selection = "none",
            extensions = c("FixedColumns"),
            options = list(
              dom = "t",
              scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 2),
              scrollY = "300px",
              pageLength = 99999,
              scrollX = TRUE,
              language = list(
                url = dt_spanish))) %>%
          formatStyle(columns = TRUE, backgroundColor = "white") %>%
          formatRound(
            columns = -c(1, 2),
            dec.mark = ",", mark = ".", digits = 2
          )
      }
    })

    observe({
      if (nrow(episodios$comparar_nt) > 0) {
        episodios$frecs_pagador <- episodios$comparar_nt %>%
          group_by(agrupador) %>%
          arrange(mes_anio_num) %>%
          pivot_wider(
            id_cols = c(unidad_conteo, agrupador, valor_mes),
            values_from = valor_contratado,
            names_from = mes_anio
          )
      }
    })

    output$frecs_pagador <- DT::renderDataTable({
      if (nrow(episodios$frecs_pagador) > 0) {
        episodios$frecs_pagador %>%
          datatable(
            colnames = c(
              "Unidad de conteo" = "unidad_conteo",
              "Agrupador" = "agrupador",
              "Valor a mes base" = "valor_mes"),
            rownames = FALSE,
            selection = "none",
            extensions = c("FixedColumns"),
            options = list(
              dom = "t",
              scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 2),
              scrollY = "300px",
              pageLength = 99999,
              scrollX = TRUE,
              language = list(
                url = dt_spanish))) %>%
          formatStyle(columns = TRUE, backgroundColor = "white") %>%
          formatCurrency(
            columns = -c(1, 2),
            dec.mark = ",", mark = ".", digits = 2
          )
      }
    })

    observe({
      if (nrow(episodios$comparar_nt) > 0) {
        episodios$frecs_ajuste <- episodios$comparar_nt %>%
          group_by(agrupador) %>%
          arrange(mes_anio_num) %>%
          pivot_wider(
            id_cols = c(unidad_conteo, agrupador),
            values_from = diff_lim_x_cm,
            names_from = mes_anio
          )
      }
    })

    output$frecs_ajuste <- DT::renderDataTable({
      if (nrow(episodios$frecs_ajuste) > 0) {
        episodios$frecs_ajuste %>%
          datatable(
            colnames = c(
              "Unidad de conteo" = "unidad_conteo",
              "Agrupador" = "agrupador"),
            rownames = FALSE,
            selection = "none",
            extensions = c("FixedColumns"),
            options = list(
              dom = "t",
              scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 2),
              scrollY = "300px",
              pageLength = 99999,
              scrollX = TRUE,
              language = list(
                url = dt_spanish))) %>%
          formatStyle(columns = TRUE, backgroundColor = "white") %>%
          formatCurrency(
            columns = -c(1, 2),
            dec.mark = ",", mark = ".", digits = 2
          )
      }
    })


    observe({
      if (nrow(episodios$comparar_nt) > 0) {
        episodios$frecs_diff <- episodios$comparar_nt %>%
          group_by(agrupador) %>%
          mutate(
            diff_media = mean(diff_efectiva_x_cm, na.rm = TRUE),
            diff_total = sum(diff_efectiva_x_cm, na.rm = TRUE)
          ) %>%
          arrange(mes_anio_num) %>%
          pivot_wider(
            id_cols = c(unidad_conteo, agrupador, diff_media, diff_total),
            values_from = diff_efectiva_x_cm,
            names_from = mes_anio
          )
      }
    })

    output$frecs_diff <- DT::renderDataTable({
      if (nrow(episodios$frecs_diff) > 0) {
        episodios$frecs_diff %>%
          datatable(
            colnames = c(
              "Unidad de conteo" = "unidad_conteo",
              "Agrupador" = "agrupador",
              "Diferencia media" = "diff_media",
              "Diferencia total" = "diff_total"),
            rownames = FALSE,
            selection = "none",
            extensions = c("FixedColumns"),
            options = list(
              dom = "t",
              scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 2),
              scrollY = "300px",
              pageLength = 99999,
              scrollX = TRUE,
              language = list(
                url = dt_spanish))) %>%
          formatStyle(columns = TRUE, backgroundColor = "white") %>%
          formatCurrency(
            columns = -c(1, 2),
            dec.mark = ",", mark = ".", digits = 2
          ) %>%
          formatStyle(
            columns = -c(1, 2),
            backgroundColor = styleInterval(
              cuts = 0,
              values = style_interval
            )
          )
      }
    })

    observe({
      if (nrow(episodios$comparar_nt) > 0) {
        episodios$valor_base <- episodios$comparar_nt %>%
          group_by(agrupador) %>%
          mutate(
            valor_medio = mean(Suma, na.rm = TRUE),
          ) %>%
          arrange(mes_anio_num) %>%
          pivot_wider(
            id_cols = c(unidad_conteo, agrupador, valor_medio),
            values_from = Suma,
            names_from = mes_anio
          )
      }
    })

    output$valor_base <- DT::renderDataTable({
      if (nrow(episodios$valor_base) > 0) {
        episodios$valor_base %>%
          datatable(
            colnames = c(
              "Unidad de conteo" = "unidad_conteo",
              "Agrupador" = "agrupador",
              "Valor medio ejecutado" = "valor_medio"),
            rownames = FALSE,
            selection = "none",
            extensions = c("FixedColumns"),
            options = list(
              dom = "t",
              scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 2),
              scrollY = "300px",
              pageLength = 99999,
              scrollX = TRUE,
              language = list(
                url = dt_spanish))) %>%
          formatStyle(columns = TRUE, backgroundColor = "white") %>%
          formatCurrency(
            columns = -c(1, 2),
            dec.mark = ",", mark = ".", digits = 2
          )
      }
    })

    observe({
      if (nrow(episodios$comparar_nt) > 0) {
        episodios$valor_diff <- episodios$comparar_nt %>%
          group_by(agrupador) %>%
          mutate(
            diff_media = mean(diff_facturado, na.rm = TRUE),
            diff_total = sum(diff_facturado, na.rm = TRUE)
          ) %>%
          arrange(mes_anio_num) %>%
          pivot_wider(
            id_cols = c(unidad_conteo, agrupador, diff_media, diff_total),
            values_from = diff_facturado,
            names_from = mes_anio
          )
      }
    })

    output$valor_diff <- DT::renderDataTable({
      if (nrow(episodios$valor_diff) > 0) {
        episodios$valor_diff %>%
          datatable(
            colnames = c(
              "Unidad de conteo" = "unidad_conteo",
              "Agrupador" = "agrupador",
              "Diferencia media" = "diff_media",
              "Diferencia total" = "diff_total"),
            rownames = FALSE,
            selection = "none",
            extensions = c("FixedColumns"),
            options = list(
              dom = "t",
              scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 2),
              scrollY = "300px",
              pageLength = 99999,
              scrollX = TRUE,
              language = list(
                url = dt_spanish))) %>%
          formatStyle(columns = TRUE, backgroundColor = "white") %>%
          formatCurrency(
            columns = -c(1, 2),
            dec.mark = ",", mark = ".", digits = 2
          ) %>%
          formatStyle(
            columns = -c(1, 2),
            backgroundColor = styleInterval(
              cuts = 0,
              values = style_interval
            )
          )
      }
    })

    # Output para descargar el informe
    # En este output se ve si existe un informe por valor facturado
    # y por frecuencias e incluye las respectivas tablas en la descarga
    output$descargar_informe <- downloadHandler(
      filename = paste0("Seguimiento de: ", episodios$nt_selected, ".xlsx"),
      content = function(file) {
        writexl::write_xlsx(
          x = list(
            "Nota técnica" = episodios$nt_current,
            "Tabla maestra" = episodios$comparar_nt,
            "Frecuencia contratada" = episodios$frecs_efectiva,
            "Total pagador" = episodios$frecs_pagador,
            "Ajuste al valor" = episodios$frecs_ajuste,
            "Ejecución de frecuencia" = episodios$frecs_base,
            "Ejecución frec x costo medio" = episodios$frecs_x_cm,
            "Dif frec con contrato" = episodios$frecs_diff,
            "Ejecución valor facturado" = episodios$valor_base,
            "Dif facturado con contrato" = episodios$valor_diff
            ),
          path = file
        )
      },
      contentType = "xlsx"
    )

   }
  )
}
