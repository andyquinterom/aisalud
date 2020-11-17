shinyServer(function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  opciones <- reactiveValues(
    "valor_costo" = "VALOR"
  )
  
 # Modulo prepara ---------------------------------------------
  
  datos_modulos <- callModule(
    module = prepara_server,
    id = "prepara_modulo",
    nombre_id = "prepara_modulo",
    opciones = opciones
  )
  
  # Modulo filtros --------------------------------------------
  
  callModule(
    module = filtros_server,
    id = "filtros_sideBar",
    datos = datos_modulos
  )
  
  # Modulo descriptiva clasica ----------------------------------------
  
  callModule(
    module = descriptiva_server,
    id = "descriptiva_modulo",
    datos = datos_modulos,
    opciones = opciones
  )
  
  # Descriptiva ---------------------------------------------------------------
  
  observeEvent(input$descriptiva_exe, {
    if(!is.null(input$file)) {
      if(!is.null(input$descriptiva_cols) && input$descriptiva_cols != "NA") {
        opciones$descriptiva_cols <- input$descriptiva_cols
        withProgress(message = "Calculando descriptiva", {
          datos$descriptiva <- descriptiva(
            data = datos$original, 
            columnas = opciones$descriptiva_cols, 
            columna_valor = opciones$valor_costo, 
            prestaciones = opciones$analisis_prestacion)
          
          output$descriptiva_tabla <- DT::renderDataTable({
            DT::datatable(
              datos$descriptiva,
              options = list(
                language = list(
                  url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 50,
                autoWidth = FALSE,
                ordering=T, 
                scrollX = TRUE,
                scrollY = "60vh"),
              rownames= FALSE) %>%
              formatCurrency(
                c('P50','P75','P90','Media','Media truncada 10%',
                  'Media truncada 5%','Desv.tipica'),
                mark = ".", 
                dec.mark = ",") %>%
              formatCurrency(c('Suma','Min.','Max.','Rango'),
                             digits = 0, 
                             mark = ".",
                             dec.mark = ",")
            })   
        })
      }
    }
  })
  
  output$descriptiva_sumas_registros <- renderText({
    if (!is.null(input$file)) {
      paste("Total registros:", 
            formatC(
              length(datos$original$NRO_IDENTIFICACION),
              big.mark = ".", 
              decimal.mark = ",", 
              format = "f", 
              digits = 0
            ),
            sep = " "
      )
    }
  })
  
  output$descriptiva_sumas_pacientes <- renderText({
    if (!is.null(input$file)) {
      paste("Total pacientes:", 
            formatC(
              length(valores_unicos[["NRO_IDENTIFICACION"]]),
              big.mark = ".", 
              decimal.mark = ",", 
              format = "f", 
              digits = 0
            ),
            sep = " "
      )
    }
  })
  
  output$descriptiva_sumas_valor <- renderText({
    if (!is.null(input$file)) {
      if(nrow(datos$descriptiva) >= 1) {
        paste("Total",
              paste0(tolower(opciones$valor_costo), ":"), 
              formatC(
                sum(
                  datos$descriptiva$Suma,
                  na.rm = TRUE), 
                big.mark = ".", 
                decimal.mark = ",", 
                format = "f", 
                digits = 0),
              sep = " "
        )
      }
    }
  })
  
  output$descriptiva_descargar_csv = downloadHandler(
    filename = function() {
      paste("Descriptiva por ",
            input$valor_costo,
            ".csv", sep="")
    },
    content = function(file) {
      write.csv(
        x = datos$descriptiva,
        file = file, 
        row.names = FALSE,
        na="")
    }, 
    contentType = "text/csv"
  )
  
  output$descriptiva_descargar_xlsx = downloadHandler(
    filename = function() {
      paste("Descriptiva por ",
            input$valor_costo,
            ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(
        x = datos$descriptiva,
        path = file)
    }, 
    contentType = "xlsx"
  )
  
  # Modulo descriptiva y episodios --------------------------------------------
  
  callModule(
    module = episodios_server, 
    id = "episodios_modulo",
    opciones = opciones,
    nombre_id = "episodios_modulo",
    datos = datos_modulos
  )
  
  # Episodios ----------------------------------------------------------------
  
  observeEvent(input$episodios_exe, {
    if(!is.null(input$file)) {
      if(!is.null(input$episodios_col_valor) && input$episodios_cols != "NA") {
        opciones$episodios_cols <- input$episodios_cols
        opciones$episodios_col_valor <- input$episodios_col_valor
        opciones$episodios_cols_sep <- input$episodios_cols_sep
        withProgress(message = "Calculando descriptiva por episodio",{
          datos$episodios <- episodios(
            data = datos$original,
            columnas =      opciones$episodios_cols, 
            columna_valor = opciones$valor_costo, 
            columna_sep =   opciones$episodios_cols_sep,
            columna_suma =  opciones$episodios_col_valor)
          
          output$episodios_tabla <- DT::renderDataTable({
            DT::datatable(
              datos$episodios,
              options = list(
                language = list(
                  url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 50,
                autoWidth = FALSE,
                ordering=T, 
                scrollX = TRUE,
                scrollY = "60vh"),
              rownames= FALSE) %>%
              formatCurrency(
                c('P50','P75','P90','Media','Media truncada 10%',
                  'Media truncada 5%','Desv.tipica'),
                mark = ".",
                dec.mark = ",") %>%
              formatCurrency(c('Suma','Min.','Max.','Rango'),
                             digits=0,
                             mark = ".",
                             dec.mark = ",")
          })   
        })
      }
    }
  })
  
  output$episodios_descargar_csv = downloadHandler(
    filename = function() {
      paste("Episodios por ",
            input$valor_costo,
            ".csv", sep="")
    },
    content = function(file) {
      write.csv(
        x = datos$episodios,
        file = file, 
        row.names = FALSE,
        na="")
    }, 
    contentType = "text/csv"
  )
  
  output$episodios_descargar_xlsx = downloadHandler(
    filename = function() {
      paste("Episodios por ",
            input$valor_costo,
            ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(
        x = datos$episodios,
        path = file)
    }, 
    contentType = "xlsx"
  )
  
  # Modulo outliers -----------------------------------------------------------
  
  callModule(
    module = outliers_server,
    id = "outliers_modulo",
    datos = datos_modulos,
    opciones = opciones,
    nombre_id = "outliers_modulo"
  )
  
  # Outliers -----------------------------------------------------------------
  
  observeEvent(input$outliers_percentil_exe, {
    if(!is.null(input$file)) {
      if(!is.null(input$outliers_cols) && input$outliers_cols != "NA") {
        opciones$outliers_cols <- input$outliers_cols
        datos$outliers <- outliers_percentil(
          data =          datos$original,
          columna =       opciones$outliers_cols,
          columna_valor = opciones$valor_costo,
          percentil =     input$outliers_percentil,
          frecuencia =    input$outliers_frecuencia)
        
        output$outliers_tabla <- DT::renderDataTable({
          DT::datatable(
            datos$outliers,
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15, 
              autoWidth = FALSE,
              ordering=T, 
              scrollX = TRUE),
            rownames= FALSE) %>%
            formatCurrency(c('VALOR'), mark = ".", dec.mark = ",")
        })   
      }
    }
  })
  
  observeEvent(input$outliers_iqr_exe, {
    if(!is.null(input$file)) {
      if(!is.null(input$outliers_cols) && input$outliers_cols != "NA") {
        opciones$outliers_cols <- input$outliers_cols
        datos$outliers <- outliers_iqr(
          data =           datos$original,
          columna =        opciones$outliers_cols,
          columna_valor =  opciones$valor_costo,
          multiplicativo = input$outliers_iqr,
          frecuencia =     input$outliers_frecuencia)
        
        output$outliers_tabla <- DT::renderDataTable({
          DT::datatable(
            datos$outliers,
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15, 
              autoWidth = FALSE, 
              ordering=T,
              scrollX = TRUE),
            rownames= FALSE) %>%
            formatCurrency(c('VALOR'), mark = ".", dec.mark = ",")
        })   
      }
    }
  })
  
  output$outliers_descargar_csv = downloadHandler(
    filename = function() {
      paste("Outliers por ",
            opciones$outliers_cols,
            ".csv", sep="")
    },
    content = function(file) {
      write.csv(
        x = datos$outliers,
        file = file, 
        row.names = FALSE,
        na="")
    }, 
    contentType = "text/csv"
  )
  
  output$outliers_descargar_csv = downloadHandler(
    filename = function() {
      paste("Outliers por ",
            opciones$outliers_cols,
            ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(
        x = datos$outliers,
        path = file)
    }, 
    contentType = "xlsx"
  )
  
  # Modulo generar nota técnica -----------------------------------------------
  
  callModule(
    module = nota_tecnica_server,
    id = "nota_tecnica_modulo",
    nombre_id = "nota_tecnica_modulo",
    datos = datos_modulos,
    opciones = opciones
  )
  
  # Generar nota técnica ------------------------------------------------------
  
  observeEvent(input$crear_nt_exe, {
    if(!is.null(input$file)) {
      if (nrow(datos$descriptiva >= 1)) {
        opciones$nt_meses <- input$crear_nt_meses
        datos$notatecnica <- crear_notatecnica(
          x = datos$descriptiva, 
          columnas = opciones$descriptiva_cols, 
          meses = input$crear_nt_meses,
          poblacion = input$crear_nt_poblacion)
        datos$notatecnica[, "Frecuencia a mes" := numerize(`Frecuencia a mes`)]
        output$crear_nt_tabla <- DT::renderDataTable(
          DT::datatable(
            datos$notatecnica, 
            class = 'cell-border stripe', 
            rownames = FALSE, 
            extensions = 'ColReorder',
            options = list(
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              pageLength = 15, 
              autoWidth = FALSE, 
              ordering=T, 
              scrollX = TRUE, 
              colReorder = TRUE)) %>%
            formatCurrency(c('Primer escenario P75',
                             'Segundo escenario media',
                             'Tercer escenario media truncada 10%',
                             'Cuarto escenario media truncada 5%',
                             'Escenario combinado mayor',
                             'Escenario por variabilidad y frecuencia',
                             paste(c('Primer escenario P75',
                                     'Segundo escenario media',
                                     'Tercer escenario media truncada 10%',
                                     'Cuarto escenario media truncada 5%',
                                     'Escenario combinado mayor',
                                     'Escenario por variabilidad y frecuencia'),
                                   'a mes',
                                   sep = " "),
                             'Valor episodio',
                             'CME'),
                           mark = ".", 
                           dec.mark = ",")
        )
      }
    }
  })
  
  observeEvent(input$crear_nt_remove, {
    if(!is.null(input$file)) {
      if(!is.null(datos$notatecnica)) {
        datos$notatecnica <- datos$notatecnica[
          -c(input$crear_nt_tabla_rows_selected),]
      }
    }
  })
  
  # Escenarios y sumas 
  
  ### Escenario 1
  output$sumasNtEsc1 = renderText({
    paste("   Suma:",
          paste0("$",
                 formatC(
                   sum(datos$notatecnica$'Primer escenario P75',
                       na.rm = TRUE),
                   big.mark = ".",
                   decimal.mark = ",",
                   format = "f",
                   digits = 0
                 )
          ),
          sep = " "
    )
  })
  
  output$sumasNtEsc1Mes = renderText({
    paste("   Suma a mes:",
          paste0("$",
                 formatC(
                   sum(datos$notatecnica$'Primer escenario P75',
                       na.rm = TRUE)/opciones$nt_meses,
                   big.mark = ".",
                   decimal.mark = ",",
                   format = "f",
                   digits = 0
                 )
          ),
          sep = " "
    )
  })
  
  output$porcNtEsc1 = renderText({
    paste("   Porcentaje del valor:",
          paste0(
            round0(
              sum(
                datos$notatecnica$'Primer escenario P75',
                na.rm = TRUE)/sum(datos$descriptiva$Suma, na.rm = TRUE)*100
            ),
            "%"
          ),
          sep = " "
    )
  })
  
  ### Escenario 2
  output$sumasNtEsc2 = renderText({
    paste("   Suma:",
          paste0("$",
                 formatC(
                   sum(datos$notatecnica$'Segundo escenario media', na.rm = TRUE),
                   big.mark = ".",
                   decimal.mark = ",",
                   format = "f",
                   digits = 0
                 )
          ),
          sep = " "
    )
  })
  
  output$sumasNtEsc2Mes = renderText({
    paste("   Suma a mes:",
          paste0("$",
                 formatC(
                   sum(
                     datos$notatecnica$'Segundo escenario media',
                     na.rm = TRUE)/opciones$nt_meses,
                   big.mark = ".",
                   decimal.mark = ",",
                   format = "f",
                   digits = 0
                 )
          ),
          sep = " "
    )
  })
  
  output$porcNtEsc2 = renderText({
    paste("   Porcentaje del valor:",
          paste0(
            round0(
              sum(
                datos$notatecnica$'Segundo escenario media',
                na.rm = TRUE)/sum(datos$descriptiva$Suma, na.rm = TRUE)*100
            ),
            "%"
          ),
          sep = " "
    )
  })
  
  ### Escenario 3
  output$sumasNtEsc3 = renderText({
    paste("   Suma:",
          paste0("$",
                 formatC(
                   sum(
                     datos$notatecnica$'Tercer escenario media truncada 10%',
                     na.rm = TRUE),
                   big.mark = ".",
                   decimal.mark = ",",
                   format = "f",
                   digits = 0
                 )
          ),
          sep = " "
    )
  })
  
  output$sumasNtEsc3Mes = renderText({
    paste("   Suma a mes:",
          paste0("$",
                 formatC(
                   sum(
                     datos$notatecnica$'Tercer escenario media truncada 10%',
                     na.rm = TRUE)/opciones$nt_meses,
                   big.mark = ".",
                   decimal.mark = ",",
                   format = "f",
                   digits = 0
                 )
          ),
          sep = " "
    )
  })
  
  output$porcNtEsc3 = renderText({
    paste("   Porcentaje del valor:",
          paste0(
            round0(
              sum(
                datos$notatecnica$'Tercer escenario media truncada 10%',
                na.rm = TRUE)/sum(datos$descriptiva$Suma, na.rm = TRUE)*100
            ),
            "%"
          ),
          sep = " "
    )
  })
  
  ### Escenario 4
  output$sumasNtEsc4 = renderText({
    paste("   Suma:",
          paste0("$",
                 formatC(
                   sum(
                     datos$notatecnica$'Cuarto escenario media truncada 5%',
                     na.rm = TRUE),
                   big.mark = ".",
                   decimal.mark = ",",
                   format = "f",
                   digits = 0
                 )
          ),
          sep = " "
    )
  })
  
  output$sumasNtEsc4Mes = renderText({
    paste("   Suma a mes:",
          paste0("$",
                 formatC(
                   sum(
                     datos$notatecnica$'Cuarto escenario media truncada 5%',
                     na.rm = TRUE)/opciones$nt_meses,
                   big.mark = ".",
                   decimal.mark = ",",
                   format = "f",
                   digits = 0
                 )
          ),
          sep = " "
    )
  })
  
  output$porcNtEsc4 = renderText({
    paste("   Porcentaje del valor:",
          paste0(
            round0(
              sum(
                datos$notatecnica$'Cuarto escenario media truncada 5%', 
                na.rm = TRUE)/sum(datos$descriptiva$Suma, na.rm = TRUE)*100
            ),
            "%"
          ),
          sep = " "
    )
  })
  
  ### Escenario 5 Combinado
  output$sumasNtEsc5 = renderText({
    paste("   Suma:",
          paste0("$",
                 formatC(
                   sum(datos$notatecnica$'Escenario combinado mayor',
                       na.rm = TRUE),
                   big.mark = ".",
                   decimal.mark = ",",
                   format = "f",
                   digits = 0
                 )
          ),
          sep = " "
    )
  })
  
  output$sumasNtEsc5Mes = renderText({
    paste("   Suma a mes:",
          paste0("$",
                 formatC(
                   sum(
                     datos$notatecnica$'Escenario combinado mayor',
                     na.rm = TRUE)/opciones$nt_meses,
                   big.mark = ".",
                   decimal.mark = ",",
                   format = "f",
                   digits = 0
                 )
          ),
          sep = " "
    )
  })
  
  output$porcNtEsc5 = renderText({
    paste("   Porcentaje del valor:",
          paste0(
            round0(
              sum(
                datos$notatecnica$'Escenario combinado mayor', 
                na.rm = TRUE)/sum(datos$descriptiva$Suma, na.rm = TRUE)*100
            ),
            "%"
          ),
          sep = " "
    )
  })
  
  ### Escenario 6 Combinado
  output$sumasNtEsc6 = renderText({
    paste("   Suma:",
          paste0("$",
                 formatC(
                   sum(
                     datos$notatecnica$'Escenario por variabilidad y frecuencia', 
                     na.rm = TRUE),
                   big.mark = ".",
                   decimal.mark = ",",
                   format = "f",
                   digits = 0
                 )
          ),
          sep = " "
    )
  })
  
  output$sumasNtEsc6Mes = renderText({
    paste("   Suma a mes:",
          paste0("$",
                 formatC(
                   sum(
                     datos$notatecnica$'Escenario por variabilidad y frecuencia', 
                     na.rm = TRUE)/opciones$nt_meses,
                   big.mark = ".",
                   decimal.mark = ",",
                   format = "f",
                   digits = 0
                 )
          ),
          sep = " "
    )
  })
  
  output$porcNtEsc6 = renderText({
    paste("   Porcentaje del valor:",
          paste0(
            round0(
              sum(
                datos$notatecnica$'Escenario por variabilidad y frecuencia',
                na.rm = TRUE)/sum(datos$descriptiva$Suma, na.rm = TRUE)*100
            ),
            "%"
          ),
          sep = " "
    )
  })
  
  output$nt_descargar_csv = downloadHandler(
    filename = function() {
      paste("notatecnica.csv", sep="")
    },
    content = function(file) {
      write.csv(
        x = datos$notatecnica,
        file = file, 
        row.names = FALSE,
        na="")
    }, 
    contentType = "text/csv"
  )
  
  output$nt_descargar_xlsx = downloadHandler(
    filename = function() {
      paste("notatecnica.xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(
        x = datos$notatecnica,
        path = file)
    }, 
    contentType = "xlsx"
  )
  
  # Histograma ----------------------------------------------------------------
  
  observeEvent(input$histograma_exe, {
    if(!is.null(input$file)) {
      if(!is.null(input$histograma_col) && input$histograma_col != "NA") {
        opciones$histograma_col <- input$histograma_col
        opciones$histograma_x <- c(input$histograma_x_min, input$histograma_x_max)
        opciones$histograma_width <- input$histograma_width
        opciones$histograma_bins <- input$histograma_bins
        opciones$histograma_fill <- input$histograma_fill
        if (input$histograma_fill == "NA" ||
            input$histograma_fill == opciones$histograma_col) {
          opciones$histograma_fill <- NULL
        }

        if (!opciones$analisis_prestacion) {
          if (opciones$histograma_col == opciones$valor_costo) {
            histograma_datos <- agregar(
              data = datos$original,
              columna_suma = "NRO_IDENTIFICACION",
              columna_valor = opciones$valor_costo,
              columnas = c(opciones$histograma_fill,
                           "NRO_IDENTIFICACION"),
              prestaciones = TRUE)
          } else {
            histograma_datos <- agregar(
              data = datos$original, 
              columna_suma = "NRO_IDENTIFICACION",
              columna_valor = opciones$valor_costo,
              columnas = c(opciones$histograma_fill,
                           opciones$histograma_col,
                           "NRO_IDENTIFICACION"),
              prestaciones = FALSE)
          }
        } else {
          histograma_datos <- datos$original
        }
        
        if(opciones$histograma_col %in% datos$colnames_num) {
          histograma_datos <- histograma_datos[
            get(opciones$histograma_col) >= opciones$histograma_x[1] & 
            get(opciones$histograma_col) <= opciones$histograma_x[2]]
        }
        
        histograma <- ggplot(
          data = histograma_datos,
          mapping = aes(x = get(opciones$histograma_col)))
        
        output$histograma_render = renderPlotly({
          if(is.null(opciones$histograma_fill) || 
             opciones$histograma_fill %in% c(opciones$histograma_col,
                                             "VALOR",
                                             "COSTO")) {
            if(opciones$histograma_col %in% datos$colnames_num) {
              variable <- as.data.table(histograma_datos)[ 
                , get(opciones$histograma_col)]
              densidad <- density(variable)
              
              plot_ly() %>% 
                add_histogram(variable, name = "Histograma",
                              nbinsx = opciones$histograma_bins) %>% 
                add_trace(x = densidad$x, y = densidad$y, type = "scatter",
                          mode = "lines", fill = "#8a2be2", yaxis = "y2",
                          name = "Densidad") %>% 
                layout(yaxis2 = list(overlaying = "y", side = "right",
                                     zeroline = FALSE, showticklabels = FALSE,
                                     showgrid = FALSE),
                       xaxis = list(title = opciones$histograma_col),
                       yaxis = list(title = "Conteo"),
                       showlegend = FALSE)
            } else {
              ggplotly(
                tooltip = NULL,
                p = histograma +
                  stat_count(width = opciones$histograma_width,
                             color="black") +
                  xlab(opciones$histograma_col) +
                  ylab("Frecuencia") +
                  scale_y_continuous(labels = scales::comma) +
                  theme(
                    axis.text.x = element_text(
                      angle = 90,
                      hjust = 1,
                      size = 12),
                    legend.title = element_blank())
              ) %>% 
                config(locale = "es") %>%
                layout(legend = list(x= 1, y = 0.5))
            }
          } else {
            if(opciones$histograma_col %in% datos$colnames_num) {
              
              variable <- as.data.table(histograma_datos)[ 
                ,
                get(opciones$histograma_col)]
              densidad <- density(variable)
              agrupador <- as.data.table(histograma_datos)[ 
                ,
                get(opciones$histograma_fill)]
              
  
              plot_ly() %>% 
                add_histogram(variable, nbinsx = opciones$histograma_bins,
                              color = agrupador) %>% 
                add_trace(x = densidad$x, y = densidad$y, type = "scatter",
                          mode = "lines", fill = "#8a2be2", yaxis = "y2",
                          name = "Densidad") %>% 
                layout(yaxis2 = list(overlaying = "y", side = "right",
                                     zeroline = FALSE, showticklabels = FALSE,
                                     showgrid = FALSE),
                       xaxis = list(title = opciones$histograma_col),
                       yaxis = list(title = "Conteo"),
                       showlegend = TRUE,
                       barmode = "stack")
              
            } else {
              ggplotly(
                tooltip = NULL,
                p = histograma +
                  stat_count(width = opciones$histograma_width,
                             color="black",
                             aes(fill = get(opciones$histograma_fill))) +
                  xlab(opciones$histograma_col) +
                  ylab("Frecuencia") +
                  scale_y_continuous(labels = scales::comma) +
                  theme(
                    axis.text.x = element_text(
                      angle = 90,
                      hjust = 1,
                      size = 12),
                    legend.title = element_blank())
              ) %>% 
                config(locale = "es") %>%
                layout(legend = list(x= 1, y = 0.5))
              
            }
          }
        })
      }
    }
  })
  
  # Bigotes -------------------------------------------------------------------
  
  observeEvent(input$bigotes_col, {
    if(!is.null(input$file)) {
      if(!is.null(input$bigotes_col) && input$bigotes_col != "NA") {
        choices <- valores_unicos[[input$bigotes_col]]
        updatePickerInput(
          session = session,
          inputId = "bigotes_seleccionar",
          choices = choices,
          choicesOpt = list(
            content = stringr::str_replace_all(
              str_wrap(choices, width = 30), "\\n", "<br>")))
      }
    }
  })
  
  observeEvent(input$bigotes_exe, {
    if(!is.null(input$file)) {
      if(!is.null(input$bigotes_col) && 
         input$bigotes_col != "NA" && 
         !is.null(input$bigotes_seleccionar)) {
        
        withProgress(message = "Graficando", {
          opciones$bigotes_col <- input$bigotes_col
          opciones$bigotesFiltroY1 <- input$bigotes_y1
          opciones$bigotesFiltroY2 <- input$bigotes_y2
          if (!opciones$analisis_prestacion) {
            datos_bigotes <- agregar(
              data = datos$original[
                get(input$bigotes_col) %in% input$bigotes_seleccionar],
              columna_suma = "NRO_IDENTIFICACION",
              columna_valor = opciones$valor_costo,
              columnas = c('NRO_IDENTIFICACION', opciones$bigotes_col), 
              prestaciones = TRUE)
            bigotes_plot = ggplot(
              data = datos_bigotes, 
              aes(x=get(opciones$bigotes_col), 
                  y=get(opciones$valor_costo), 
                  group = get(opciones$bigotes_col))) +
              geom_boxplot() +
              scale_y_continuous(
                labels = scales::comma,
                name = opciones$valor_costo) +
              coord_cartesian(
                ylim = c(opciones$bigotes_y1,
                         opciones$bigotes_y2)) +
              xlab(opciones$bigotes_col) +
              ylab(opciones$valor_costo) +
              theme(
                axis.text.x = element_text(angle = 90, hjust = 1, size = 12))
            datos_bigotes <- NULL
          }
          else {
            datos_bigotes <- NULL
            bigotes_plot <- ggplot(
             data = datos$original[
               get(input$bigotes_col) %in% input$bigotes_seleccionar], 
             aes(x=get(opciones$bigotes_col), 
                 y=get(opciones$valor_costo), 
                 group = get(opciones$bigotes_col) )) +
              geom_boxplot() +
              scale_y_continuous(
                labels = scales::comma, name = opciones$valor_costo) +
              coord_cartesian(
                ylim = c(opciones$bigotes_y1, opciones$bigotes_y2)) +
              xlab(opciones$bigotes_col) +
              ylab(opciones$valor_costo) +
              theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12))
            
          }
          
          output$bigotes_render <- plotly::renderPlotly({
            ggplotly(
              p = bigotes_plot) %>%
              config(locale = "es")
          })
        })
      }
    }
  })
  
  # Modulo paquete -------------------------------------------------
  
  callModule(
    module = paquetes_dashboard_server,
    id = "paquetes_modulo_dashboard",
    paquetes = paquetes,
    paquetes_ref_cups = paquetes_ref_cups,
    paquetes_ref = paquetes_ref,
    paquetes_paquetes = paquetes_paquetes,
    paquetes_cups = paquetes_cups
  )

  callModule(
    module = paquetes_indice_server,
    id = "paquetes_modulo_indice",
    paquetes = paquetes,
    paquete_path = paquete_path,
    nombre_id = "paquetes_modulo_indice"
  )
  
  # Paquetes ----------------------------------------------------------------
  
  output$paquetes_indice_tabla <- DT::renderDataTable(
    datatable(
      unique(paquetes[, list(`CODIGO PAQUETE`,
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
          unique(paquetes[, list(`CODIGO PAQUETE`,
                                 ESPECIALIDAD, SERVICIO,
                                 DESCRIPCION, INCLUSIONES,
                                 EXCLUSIONES)]))
                )) %>%
      DT::formatStyle(1:6, backgroundColor = 'white')
  )
  
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
        `CODIGO PAQUETE` == input$paquetes_select]
      paquetes_valores$paquete_cups <- paquetes_cups[
        `CODIGO PAQUETE` == input$paquetes_select]
      
      paquetes_valores$servicio <-
        paquetes_valores$paquete_datos$SERVICIO
      paquetes_valores$especialidad <-
        paquetes_valores$paquete_datos$ESPECIALIDAD
      paquetes_valores$descripcion <-
        paquetes_valores$paquete_datos$`DESCRIPCION`
      paquetes_valores$inclusiones <- 
        paquetes_valores$paquete_datos$`INCLUSIONES`
      paquetes_valores$exclusiones <- 
        paquetes_valores$paquete_datos$`EXCLUSIONES`
    }
  })
  
  output$paquetes_tabla <- DT::renderDataTable({
    if(!is.null(paquetes)) {
      datatable(
        paquetes_valores$paquete_datos[,
            c("CUMS/CUPS",
              "PRESTACION",
              "COMPONENTE", 
              "TIPO DE COSTO",
              "VALOR",
              "COSTO")],
        rownames = F,
        selection = 'single',
        options = list(
          dom='ft',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = nrow(paquetes_valores$paquete_datos),
          ordering = FALSE,
          scrollX = TRUE,
          scrollY = "60vh")) %>%
        DT::formatCurrency(
          columns = c("VALOR", "COSTO"),
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
        value = formatAsCurrency(paquetes_valores$paquete_datos$VALOR),
        subtitle = "Valor del paquete", 
        icon = icon("tags", lib = "font-awesome"), 
        color = "green"
      )
    }
  })
  
  output$paquetes_costototal <- renderValueBox({
    if(!is.null(paquetes)) {
      valueBox(
        value = formatAsCurrency(paquetes_valores$paquete_datos$COSTO),
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
        referente = paquetes_ref[
          `CODIGO PAQUETE` == paquetes_valores$paquete],
        cups = paquetes_valores$paquete_cups[
          input$paquetes_tabla_rows_selected,
          "CUMS/CUPS"],
        valor_costo = input$paquetes_valor_costo)
  })
  
  output$paquetes_resumen_plot <- renderggiraph({
    if(!is.null(paquetes)) 
      pie_chart(
        paquetes = paquetes_valores$paquete_cups,
        columna = input$paquetes_resumen_select,
        valor_costo = input$paquetes_valor_costo)
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
  
  output$paquetes_componenete_plot <- renderggiraph({
    if(!is.null(input$paquetes_componenete_select))
      pie_chart(
        paquetes = paquetes_valores$paquete_cups[
          COMPONENTE %in% input$paquetes_componenete_select],
        columna = input$paquetes_componenete_datos,
        valor_costo = input$paquetes_valor_costo)
  })
  
  output$paquetes_componenete_tabla <- DT::renderDataTable({
    if(!is.null(input$paquetes_componenete_select)) {
      datatable(
        resumenComp(
          tabla = paquetes_valores$paquete_cups[
            COMPONENTE %in% input$paquetes_componenete_select],
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
  
  output$paquetes_tipo_costo_plot <- renderggiraph({
    if(!is.null(input$paquetes_tipo_costo_select))
      pie_chart(
        paquetes = paquetes_valores$paquete_cups[
          `TIPO DE COSTO` %in% input$paquetes_tipo_costo_select],
        columna = input$paquetes_tipo_costo_datos,
        valor_costo = input$paquetes_valor_costo)
  })
  
  output$paquetes_tipo_costo_tabla <- DT::renderDataTable({
    if(!is.null(input$paquetes_tipo_costo_select)) {
      datatable(
        resumenComp(
          tabla = paquetes_valores$paquete_cups[
            `TIPO DE COSTO` %in% input$paquetes_tipo_costo_select],
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
  
  # Pricing ------------------------------------------------------------------
  
  observeEvent(input$pricing_actualizar, {
    confirmSweetAlert(
      session,
      inputId = "pricing_actualizar_conf", 
      title = "Confirmar", 
      text = "¿Seguro que quieres actualizar el pricing?
              Al final, deberá reiniciar la aplicación.",
      showCloseButton = TRUE,
      btn_labels = c("Cancelar", "Confirmar"))
  })
  
  observeEvent(input$pricing_actualizar_conf, {
    if (input$pricing_actualizar_conf) {
      withProgress(
        value = 0,
        message = "Actualizando pricing...",
        min = 0,
        max = 1.1, {
          unlink("datos/pricing/", recursive = TRUE)
          dir.create("datos/pricing")
          pricingList = drive_ls(path = as_id(pricing_path))
          incProgress(0.1, message = "¡Archivos leidos!")
          for (i in 1:length(pricingList$id)) {
            drive_download(
              file = as_id(pricingList$id[i]),
              path = paste0("datos/pricing/", 
                            pricingList$name[i]),
              overwrite = T)
            incProgress(1/length(pricingList$id))
            }
      })
      
      sendSweetAlert(
        session,
        title = "¡Pricing actualizados efectivamente!",
        text = "Para ver los datos y gráficos actualizados,
                por favor recargar la página.",
        type = "success")
      
      stopApp()
    }
  })
  
  pricing <- reactiveValues()
  
  observeEvent(input$pricing_select, {
    if (PRICING_INCLUIDO) {
      pricing$datos <- fread(
        input = paste0("datos/pricing/", input$pricing_select, ".csv"),
        na.strings = c("-", "#N/A", "#DIV/0!", "NA"),
        data.table = TRUE)
      
      pricing$datos[, "CODIGO_CUPS" := as.character(CODIGO_CUPS)]
      pricing$datos[, "VALOR_UNITARIO" := numerize(VALOR_UNITARIO)]
      pricing$datos[, "MAXIMO" := numerize(MAXIMO)]
      pricing$datos[, "MINIMO" := numerize(MINIMO)]
      pricing$datos[, "MEDIA" := numerize(MEDIA)]
      pricing$datos[, "RELATIVO_MEDIA" := numerize(RELATIVO_MEDIA)]
      pricing$datos[, "RELATIVO_MINIMO" := numerize(RELATIVO_MINIMO)]
    
    pricing$sd_media <- round(
      abs(sd(pricing$datos$RELATIVO_MEDIA, na.rm = TRUE)),
      digits = 3)
    pricing$sd_min <- round(
      abs(median(pricing$datos$RELATIVO_MINIMO, na.rm = TRUE)),
      digits = 3)
    pricing$descriptiva <- data.table(
      "x" = c("Valor Unitario Mínimo",
              "Valor Unitario Máximo",
              "Valor Unitario Promedio",
              "Mínimo",
              "Máximo", 
              "Media"),
      "y" = c(0,0,0,0,0,0))
    }
  })
  
  output$pricing_ui_prestacion <- renderUI({
    pickerInput(
      inputId = "pricing_filtro_prestaciones",
      label = "Nombre de Prestación",
      choices = unique(pricing$datos$NOMBRE_PRESTACION),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "Deseleccionar todos",
        `select-all-text` = "Seleccionar todos",
        `live-search` = TRUE))
  })
  
  output$pricing_ui_entidad <- renderUI({
    pickerInput(
      inputId = "pricing_filtro_entidad",
      label = "Nombre Entidad",
      choices = unique(pricing$datos$NOMBRE_ENTIDAD),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "Deseleccionar todos",
        `select-all-text` = "Seleccionar todos",
        `live-search` = TRUE))
  })
  
  output$pricing_ui_observacion <- renderUI({
    pickerInput(
      inputId = "pricing_filtro_observacion",
      label = "Observación",
      choices = unique(pricing$datos$OBSERVACIÓN),
      multiple = TRUE, 
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "Deseleccionar todos",
        `select-all-text` = "Seleccionar todos", 
        `live-search` = TRUE))
  })
  
  output$pricing_ui_rel_media <- renderUI({
    sliderInput(
      inputId = "pricing_rel_media",
      "Relativo a la media",
      min = -2*pricing$sd_media,
      max = 2*pricing$sd_media,
      value = c(-2*pricing$sd_media, 2*pricing$sd_media),
      step = 0.01)
  })
  
  output$pricing_ui_rel_min <- renderUI({
    sliderInput(
      inputId = "pricing_rel_min",
      "Relativo al mínimo",
      min = -2*pricing$sd_min,
      max = 2*pricing$sd_min,
      value = c(-2*pricing$sd_min, 2*pricing$sd_min),
      step = 0.01)
  })
  
  observeEvent(input$pricing_exe, {
    pricing$filtrado <- pricing$datos
    pricing$filtrado <- pricing$filtrado[
      NOMBRE_PRESTACION %in% input$pricing_filtro_prestaciones &
      NOMBRE_ENTIDAD %in% input$pricing_filtro_entidad &
      OBSERVACIÓN %in% input$pricing_filtro_observacion]

    if (input$pricing_rel_media[1] != -2*pricing$sd_media) {
      pricing$filtrado <- pricing$filtrado[
        RELATIVO_MEDIA >= input$pricing_rel_media[1]]
    }
    if (input$pricing_rel_media[2] != 2*pricing$sd_media) {
      pricing$filtrado <- pricing$filtrado[
        RELATIVO_MEDIA <= input$pricing_rel_media[2]]
    }

    if (input$pricing_rel_min[1] != -2*pricing$sd_min) {
      pricing$filtrado <- pricing$filtrado[
        RELATIVO_MINIMO >= input$pricing_rel_min[1]]
    }
    if (input$pricing_rel_min[2] != 2*pricing$sd_min) {
      pricing$filtrado <- pricing$filtrado[
        RELATIVO_MINIMO >= input$pricing_rel_min[2]]
    }

    pricing_descriptiva_y <- c(
      min(pricing$filtrado$VALOR_UNITARIO, na.rm = T),
      max(pricing$filtrado$VALOR_UNITARIO, na.rm = T),
      mean(pricing$filtrado$VALOR_UNITARIO, na.rm = TRUE),
      min(pricing$filtrado$MINIMO, na.rm = T),
      max(pricing$filtrado$MAXIMO, na.rm = T),
      max(pricing$filtrado$MEDIA, na.rm = TRUE))
    
    pricing$descriptiva[, "y" := pricing_descriptiva_y]
    
  })
  
  output$pricing_cups <- DT::renderDataTable({
    if (!is.null(pricing$filtrado)) {
      datatable(pricing$filtrado[, list(
        "MIN" = min(VALOR_UNITARIO, na.rm = TRUE),
        "MEDIA" = mean(VALOR_UNITARIO, na.rm = TRUE), 
        "MAX" = max(VALOR_UNITARIO, na.rm = TRUE), 
        "MEDIA MERCADO" = min(MEDIA, na.rm = TRUE)), 
        by= c("NOMBRE_ENTIDAD", "CODIGO_CUPS")],
        rownames = FALSE, 
        options = list(
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          ordering=T)) %>%
        formatCurrency(c("MIN", "MEDIA", "MAX", "MEDIA MERCADO"),
                       mark = ".",
                       dec.mark = ",")
    }
  })
  
  output$pricing_entidades <- renderggiraph({
    if (!is.null(pricing$filtrado)) {
      girafe(
        ggobj = ggplot(
          data = pricing$filtrado, 
          aes(NOMBRE_ENTIDAD,
              fill = OBSERVACIÓN, 
              data_id = OBSERVACIÓN,
              tooltip = OBSERVACIÓN)) +
          geom_bar_interactive() +
          theme_minimal() +
          scale_y_continuous(
            labels = scales::comma, 
            name = "# de prestaciones") +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="left") +
          labs(y= "Conteo", x = "Nombre Entidad"),
        options = list(
          opts_selection(type = "none", only_shiny = FALSE)),
        width_svg = 18,
        height_svg = 9
      )
    }
  })

  output$pricing_prestaciones <- renderggiraph({
    if (!is.null(pricing$descriptiva)) {
      girafe(
        ggobj = ggplot(
          data = pricing$descriptiva,
          aes(x = x,
              y = as.numeric(as.character(y)),
              fill = x,
              data_id = x,
              tooltip = formatAsCurrency(numerize(y)))) +
          geom_bar_interactive(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="left") +
          scale_y_continuous(labels = scales::comma, name = "Valor") +
          labs(y= "Valor", x = "Comparación"),
        options = list(opts_selection(type = "none", only_shiny = FALSE)),
        width_svg = 18, height_svg = 9
      )
    }
  })
  
  # Modulos seguimiento NT ---------------------------------------------
  
  if (NT_INCLUIDO) {
    
    callModule(
      module = seguimiento_notas_indice_server,
      id = "seguimiento_notas_indice",
      indice = dash_nt_indice,
      mapa = dash_nt_mapa
    )
    
    callModule(
      module = seguimiento_notas_dashboard_server,
      id = "seguimiento_notas_dash",
      indice = dash_nt_indice,
      nota_tecnica = dash_nt_datos,
      inclusiones = dash_nt_inclusiones
    )
    
    callModule(
      module = seguimiento_notas_comparar_server,
      id = "seguimiento_notas_comparar",
      datos = datos_modulos,
      indice = dash_nt_indice,
      nota_tecnica = dash_nt_datos,
      nombre_id = "seguimiento_notas_comparar",
      opciones = opciones
    )
    
  }
  
  # Actualizar seguimiento notas tecnicas
  
  observeEvent(input$dash_nt_actualizar, {
    confirmSweetAlert(
      session, inputId = "dash_nt_actualizar_conf", 
      title = "Confirmar", 
      text = "¿Seguro que quieres actualizar las notas técnicas?
              Al final, deberá reiniciar la aplicación.",
      showCloseButton = TRUE,
      btn_labels = c("Cancelar", "Confirmar"))
  })
  
  observeEvent(input$dash_nt_actualizar_conf, {
    if (input$dash_nt_actualizar_conf) {
      unlink("datos/nts/", recursive = TRUE)
      dir.create("datos/nts")
      withProgress(value = 0, message = "Actualizando notas técnicas...", {
        write_feather(
          sheets_read(
            nts_path, 
            sheet = "NTs",
            col_types = "ccddd"),
          "datos/nts/notas_tecnicas.feather")
        
        incProgress(0.3)
        
        write_feather(
          sheets_read(
            nts_path, 
            sheet = "INDICE",
            col_types = "ccdcccd"),
          "datos/nts/indice.feather")
        
        incProgress(0.3)
        
        write_feather(
          sheets_read(
            nts_path,
            sheet = "INCLUSIONES",
            col_types = "ccdc"),
          "datos/nts/inclusiones.feather")
        
        incProgress(0.3)
        
        saveRDS(
          mapaValoresNT(
            as.data.table(
              sheets_read(
                nts_path,
                sheet = "INDICE",
                col_types = "ccdcccd"))) %>% 
            layout(autosize = TRUE),
          "datos/nts/nt_mapa.rds")
      })
      
      sendSweetAlert(
        session, 
        title = "¡Notas técnicas actualizados efectivamente!",
        text = "Para ver los datos y gráficos actualizados,
                por favor recargar la página.", 
        type = "success")
      
      stopApp()
    }
  })
  
})