shinyServer(function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Sección cargar datos -----------------------------------------------------
  
  opciones <- reactiveValues(descriptiva_cols = NULL, valor_costo = "VALOR")
  datos <- reactiveValues(descriptiva = data.table(),
                          original = data.table(),
                          descriptivaEp = data.table())
  valores_unicos <- reactiveValues()
  minimos_maximos <- reactiveValues()
  filtro <- reactiveValues("col_1" = "NA", 
                           "col_2" = "NA", 
                           "col_3" = "NA", 
                           "col_4" = "NA",
                           "num_1" = "NA",
                           "num_2" = "NA")

  output$preview <- DT::renderDataTable({
    if(is.null(input$file)) {
      data.table()
    } else {
      tryCatch(
        expr = { 
          Preview <- DT::datatable(
            head(
              read_feather(path = input$file$datapath, 
                           columns = c("NRO_IDENTIFICACION",
                                       "FECHA_PRESTACION",
                                       input$valor_costo)),
              n = 5),
            rownames = FALSE,
            options = list(
              dom = 't',
              pageLength = 5,
              ordering = FALSE,
              columnDefs = list(list(className = 'dt-center', targets = 0:2)), 
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
              )) %>%
            DT::formatStyle(
              columns = 1:3, 
              valueColumns = 1,
              backgroundColor = "white")
          opciones$valor_costo <- input$valor_costo
          showNotification(
            ui = paste0("Análisis cambiado a ", tolower(opciones$valor_costo)),
            duration = 5,
            type = "message",
            session = session)
          Preview
      },
      error = function(e) {
        print(e[1])
        updateRadioButtons(session = session,
                           inputId = "valor_costo",
                           selected = opciones$valor_costo)

        sendSweetAlert(
          session = session,
          title = "Error",
          text = e[1],
          type = "error"
        )
      }
      )
    }
  })
  
  observeEvent(input$analisis_prestacion, {
    if (input$analisis_prestacion == "PACIENTE") {
      opciones$analisis_prestacion <- FALSE
      showNotification(
        ui = paste("Análisis cambiado a paciente"),
        duration = 5,
        type = "message",
        session = session)
    }
    else {
      opciones$analisis_prestacion <- TRUE
      showNotification(
        ui = paste("Análisis cambiado a prestación"),
        duration = 5,
        type = "message",
        session = session)
    }
  })
  
  
  observeEvent(input$ejecutar_opciones, {
    if(!is.null(input$file)) {
      withProgress(
        message = "Cargando base de datos", {
          print("Inicio lectura de archivo")
          datos$original <- as.data.table(
            x = feather::read_feather(path = input$file$datapath))
          print("Fin lectura de archivo")
          incProgress(0.5)
          opciones$fecha_min <- as.Date(x = input$fecha_min, "%d/%m/%Y")
          opciones$fecha_max <- as.Date(x = input$fecha_max, "%d/%m/%Y")
          opciones$formato_fecha <- input$formato_fecha
          incProgress(0.1)
          datos$original[, "FECHA_PRESTACION" := as.Date(
            x = FECHA_PRESTACION,format = opciones$formato_fecha)]
          datos$original <- datos$original[
            FECHA_PRESTACION >= opciones$fecha_min &
            FECHA_PRESTACION <= opciones$fecha_max]
          datos$original[, "NRO_IDENTIFICACION" := as.character(
            NRO_IDENTIFICACION)]
          columnas_num <- unlist(lapply(datos$original[1,], is.numeric))
          datos$colnames <- colnames(datos$original)
          incProgress(0.1)
          updatePickerInput(session,
                            inputId = "descriptiva_cols",
                            choices = datos$colnames, 
                            selected = opciones$descriptiva_cols)
          updatePickerInput(session,
                            inputId = "episodios_cols",
                            choices = datos$colnames,
                            selected = opciones$episodios_cols)
          updatePickerInput(session,
                            inputId = "episodios_col_valor",
                            choices = datos$colnames,
                            selected = opciones$episodios_col_valor)
          updatePickerInput(session, 
                            inputId = "episodios_cols_sep",
                            choices = datos$colnames,
                            selected = opciones$episodios_cols_sep)
          updatePickerInput(session,
                            inputId = "outliers_cols",
                            choices = datos$colnames,
                            selected = opciones$outliers_cols)
          updatePickerInput(session,
                            inputId = "bigotes_col",
                            choices = c("NA", datos$colnames[!columnas_num]),
                            selected = opciones$bigotes_col)
          updatePickerInput(session,
                            inputId = "histograma_fill",
                            choices = c("NA", datos$colnames),
                            selected = opciones$histograma_fill)
          updatePickerInput(session, 
                            inputId = "histograma_col",
                            choices = datos$colnames,
                            selected = opciones$histograma_col)
          updatePickerInput(session, 
                            inputId = "comparar_nt_col", 
                            choices = datos$colnames,
                            selected = opciones$comparar_nt_col)
          for (i in c(1:5)) {
            updatePickerInput(
              session,
              inputId = paste("filtro_", i, sep = ""),
              choices = c("NA", datos$colnames[!columnas_num]))
          }
          for (i in c(1:2)) {
            updatePickerInput(
              session, 
              inputId = paste("filtro_largo_", i, sep = ""), 
              choices = c("NA", datos$colnames[!columnas_num]))
          }
          for (i in c(1:2)) {
            updatePickerInput(
              session, 
              inputId = paste("filtro_num_1", i, sep = ""),
              choices = c("NA", datos$Colnames[columnas_num]))
          }
          for (i in datos$colnames[!columnas_num]) {
            valores_unicos[[i]] = unique(datos$original[[i]])
          }
          for (i in datos$colnames[columnas_num]) {
            minimos_maximos[[i]] = c(min(datos$original[[i]], na.rm = TRUE),
                                     max(datos$original[[i]], na.rm = TRUE))
          }
          sendSweetAlert(
            session = session,
            title = "Éxito",
            text = "Las opciones han sido ejecutadas",
            type = "success"
          )
      })} 
  })
  
  # Inicio cargar filtros -----------------------------------------------------
  
  observeEvent(input$filtro_1, {
    filtro$col_1 <- input$filtro_1
    if (filtro$col_1 != "NA" && !is.null(input$filtro_1)) {
      updatePickerInput(
        session, 
        inputId = "filtro_1_val", 
        choices = valores_unicos[[filtro$col_1]],
        choicesOpt = list(
          content = stringr::str_replace_all(
            str_wrap(valores_unicos[[filtro$col_1]], width = 50),
            "\\n", "<br>")))
    } else if (filtro$col_1 == "NA") {
      updatePickerInput(session, 
                        inputId = "filtro_1_val", 
                        choices = "NA", 
                        selected = "NA")
    }
  })
  
  observeEvent(input$filtro_2, {
    filtro$col_2 <- input$filtro_2
    if (filtro$col_2 != "NA" && !is.null(input$filtro_2)) {
      updatePickerInput(
        session, 
        inputId = "filtro_2_val", 
        choices = valores_unicos[[filtro$col_2]],
        choicesOpt = list(
          content = stringr::str_replace_all(
            str_wrap(valores_unicos[[filtro$col_2]], width = 50),
            "\\n", "<br>")))
    } else if (filtro$col_2 == "NA") {
      updatePickerInput(session, 
                        inputId = "filtro_2_val", 
                        choices = "NA", 
                        selected = "NA")
    }
  })
  
  observeEvent(input$filtro_3, {
    filtro$col_3 <- input$filtro_3
    if (filtro$col_3 != "NA" && !is.null(input$filtro_3)) {
      updatePickerInput(
        session, 
        inputId = "filtro_3_val", 
        choices = valores_unicos[[filtro$col_3]],
        choicesOpt = list(
          content = stringr::str_replace_all(
            str_wrap(valores_unicos[[filtro$col_3]], width = 50),
            "\\n", "<br>")))
    } else if (filtro$col_3 == "NA") {
      updatePickerInput(session, 
                        inputId = "filtro_3_val", 
                        choices = "NA", 
                        selected = "NA")
    }
  })
  
  observeEvent(input$filtro_4, {
    filtro$col_4 <- input$filtro_4
    if (filtro$col_4 != "NA" && !is.null(input$filtro_4)) {
      updatePickerInput(
        session, 
        inputId = "filtro_4_val", 
        choices = valores_unicos[[filtro$col_4]],
        choicesOpt = list(
          content = stringr::str_replace_all(
            str_wrap(valores_unicos[[filtro$col_4]], width = 50),
            "\\n", "<br>")))
    } else if (filtro$col_4 == "NA") {
      updatePickerInput(session, 
                        inputId = "filtro_4_val", 
                        choices = "NA", 
                        selected = "NA")
    }
  })
  
  observeEvent(input$filtro_5, {
    filtro$col_5 <- input$filtro_5
    if (filtro$col_5 != "NA" && !is.null(input$filtro_5)) {
      updatePickerInput(
        session, 
        inputId = "filtro_5_val", 
        choices = valores_unicos[[filtro$col_5]],
        choicesOpt = list(
          content = stringr::str_replace_all(
            str_wrap(valores_unicos[[filtro$col_5]], width = 50),
            "\\n", "<br>")))
    } else if (filtro$col_5 == "NA") {
      updatePickerInput(session, 
                        inputId = "filtro_5_val", 
                        choices = "NA", 
                        selected = "NA")
    }
  })
  
  # Numericos
  
  observeEvent(input$filtro_num_1, {
    filtro$num_1 <- input$filtro_num_1
    if (filtro$num_1 != "NA" && !is.null(input$filtro_num_1)) {
      updateNumericInput(
        session, 
        inputId = "filtro_num_1_min",
        value = minimos_maximos[[filtro$num_1]][1])
      updateNumericInput(
        session,
        inputId = "filtro_num_1_max",
        value = minimos_maximos[[filtro$num_1]][2])
    }
    else if (filtro$num_1 == "NA") {
      updateNumericInput(
        session, 
        inputId = "filtro_num_1_min",
        value = 0)
      updateNumericInput(
        session,
        inputId = "filtro_num_1_max", 
        value = 0)
    }
  })
  
  observeEvent(input$filtro_num_1_min, ignoreNULL = FALSE, {
    if (is.na(input$filtro_num_1_min)) {
      updateNumericInput(
        session,
        inputId = "filtro_num_1_min", 
        value = minimos_maximos[[filtro$num_1]][1])
    }
  })
  
  observeEvent(input$filtro_num_1_max, ignoreNULL = FALSE, {
    if (is.na(input$filtro_num_1_max)) {
      updateNumericInput(
        session,
        inputId = "filtro_num_1_max", 
        value = minimos_maximos[[filtro$num_1]][2])
    }
  })
  
  observeEvent(input$filtro_num_2, {
    filtro$num_2 <- input$filtro_num_2
    if (filtro$num_2 != "NA" && !is.null(input$filtro_num_2)) {
      updateNumericInput(
        session, 
        inputId = "filtro_num_2_min",
        value = minimos_maximos[[filtro$num_2]][1])
      updateNumericInput(
        session,
        inputId = "filtro_num_2_max",
        value = minimos_maximos[[filtro$num_2]][2])
    }
    else if (filtro$num_2 == "NA") {
      updateNumericInput(
        session, 
        inputId = "filtro_num_2_min",
        value = 0)
      updateNumericInput(
        session,
        inputId = "filtro_num_2_max", 
        value = 0)
    }
  })
  
  observeEvent(input$filtro_num_2_min, ignoreNULL = FALSE, {
    if (is.na(input$filtro_num_2_min)) {
      updateNumericInput(
        session,
        inputId = "filtro_num_2_min", 
        value = minimos_maximos[[filtro$num_2]][1])
    }
  })
  
  observeEvent(input$filtro_num_2_max, ignoreNULL = FALSE, {
    if (is.na(input$filtro_num_2_max)) {
      updateNumericInput(
        session,
        inputId = "filtro_num_2_max", 
        value = minimos_maximos[[filtro$num_2]][2])
    }
  })
  
  # Aplicar filtros ----------------------------------------------------------
  
  observeEvent(input$filtro_aplicar, {
    if(!is.null(input$file)) {
      withProgress(message = "Aplicando filtros",{
        datos$original <- as.data.table(
          feather::read_feather(path = input$file$datapath))
        datos$original[, "FECHA_PRESTACION" := as.Date(
          x = FECHA_PRESTACION, format = opciones$formato_fecha)]
        datos$original <- datos$original[
          FECHA_PRESTACION >= opciones$fecha_min &
            FECHA_PRESTACION <= opciones$fecha_max]
        datos$original[, "NRO_IDENTIFICACION" := as.character(
          NRO_IDENTIFICACION)]
        incProgress(0.3)
        if (input$filtro_1 != "NA") {
          datos$original <- datos$original[
            get(input$filtro_1) %in% input$filtro_1_val]
        }
        if (input$filtro_2 != "NA") {
          datos$original <- datos$original[
            get(input$filtro_2) %in% input$filtro_2_val]
        }
        if (input$filtro_3 != "NA") {
          datos$original <- datos$original[
            get(input$filtro_3) %in% input$filtro_3_val]
        }
        if (input$filtro_4 != "NA") {
          datos$original <- datos$original[
            get(input$filtro_4) %in% input$filtro_4_val]
        }
        if (input$filtro_5 != "NA") {
          datos$original <- datos$original[
            get(input$filtro_5) %in% input$filtro_5_val]
        }
        
        if (input$filtro_largo_1 != "NA") {
          datos$original <- ifelse(
            test = input$filtro_largo_1_excluir,
            yes = datos$original[
              get(input$filtro_largo_1) %notin% str_split(
                input$filtro_largo_1_val, pattern = ";")[[1]]],
            no =  datos$original[
              get(input$filtro_largo_1) %in% str_split(
                input$filtro_largo_1_val, pattern = ";")[[1]]]
          )
        }
        
        if (input$filtro_largo_2 != "NA") {
          datos$original <- ifelse(
            test = input$filtro_largo_2_excluir,
            yes = datos$original[
              get(input$filtro_largo_2) %notin% str_split(
                input$filtro_largo_2_val, pattern = ";")[[1]]],
            no =  datos$original[
              get(input$filtro_largo_2) %in% str_split(
                input$filtro_largo_2_val, pattern = ";")[[1]]]
          )
        }
        
        if (input$filtro_num_1 != "NA") {
          datos$original <- datos$original[
            get(input$filtro_num_1) >= input$filtro_num_1_min &
            get(input$filtro_num_1) <= input$filtro_num_1_max]
        }
        if (input$filtro_num_2 != "NA") {
          datos$original <- datos$original[
            get(input$filtro_num_2) >= input$filtro_num_2_min &
              get(input$filtro_num_2) <= input$filtro_num_2_max]
        }

      })
      showNotification(
        ui = paste0("Filtros aplicados"),
        duration = 5, 
        type = "message", 
        session = session)
    }
  })
  
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
          
          output$outliers_tabla <- DT::renderDataTable({
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
  
  # Generar nota técnica ------------------------------------------------------
  
  observeEvent(input$crear_nt_exe, {
    if(!is.null(input$file)) {
      if (nrow(datos$descriptiva >= 1)) {
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
          -as.numeric(input$NTtabla_rows_selected),]
      }
    }
  })
  
  # Histograma ----------------------------------------------------------------
  
  observeEvent(input$exeOpcionesHistograma, {
    if(!is.null(input$file)) {
      if(!is.null(input$histogramaColumna) && input$histogramaColumna != "NA") {
        withProgress(message = "Graficando", min = 0,{
          totalcolumna = all(check.numeric(unlist(extractCol(datos$original, input$histogramaColumna))))
          opciones$histogramaColumna = input$histogramaColumna
          opciones$histogramaFiltroX = c(input$histogramaFiltroX1, input$histogramaFiltroX2)
          opciones$histogramabinWidth = input$histogramabinWidth
          opciones$histogramaNumBins = input$histogramaNumBins
          opciones$histogramaRelleno = input$histogramaRelleno
          colorRellenoNA = ""
          if (input$histogramaRelleno == "NA" || input$histogramaRelleno == opciones$histogramaColumna)
            opciones$histogramaRelleno = NULL
          incProgress(0.1)
          if (isFALSE(opciones$pacientesPrestaciones)) {
            bdPrestaciones = NULL
            if (opciones$histogramaColumna == opciones$valor_costo)
              bdPacientes = agregar(datos$original, columna_valor = opciones$valor_costo, columnas = c('NRO_IDENTIFICACION', opciones$histogramaRelleno), prestaciones = TRUE)
            else
              bdPacientes = agregar(datos$original, columna_valor = opciones$valor_costo, columnas = c('NRO_IDENTIFICACION', opciones$histogramaRelleno, opciones$histogramaColumna), prestaciones = TRUE)
            
            incProgress(0.3)
            if(totalcolumna)
              bdPacientes = bdPacientes[get(opciones$histogramaColumna) >= opciones$histogramaFiltroX[1] & get(opciones$histogramaColumna) <= opciones$histogramaFiltroX[2]]
            histograma = ggplot(bdPacientes, aes(x=get(opciones$histogramaColumna)))
            bdPacientes = NULL
          }
          else {
            incProgress(0.3)
            bdPacientes = NULL
            if(totalcolumna)
              bdPrestaciones = datos$original[get(opciones$histogramaColumna) >= opciones$histogramaFiltroX[1] & get(opciones$histogramaColumna) <= opciones$histogramaFiltroX[2]]
            else
              bdPrestaciones = datos$original
            histograma = ggplot(bdPrestaciones, aes(x=get(opciones$histogramaColumna)))
            bdPrestaciones = NULL
          }
          incProgress(0.3)
          output$histogramaFinal = renderPlotly({
            if(is.null(opciones$histogramaRelleno) || opciones$histogramaRelleno %in% c(opciones$histogramaColumna, "VALOR", "COSTO")) {
              if(totalcolumna) {
                ggplotly(tooltip = FALSE, p = 
                           histograma +
                           geom_histogram(bins = opciones$histogramaNumBins, color="black", aes(y = ..density..)) +
                           geom_density() +
                           xlab(opciones$histogramaColumna) +
                           ylab("Densidad") +
                           scale_y_continuous(labels = scales::comma) +
                           theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), legend.title = element_blank())
                ) %>% config(locale = "es") %>%
                  layout(legend = list(x= 1, y = 0.5))
              }
              else {
                ggplotly(tooltip = NULL, p =
                           histograma +
                           stat_count(width = opciones$histogramabinWidth, color="black") +
                           xlab(opciones$histogramaColumna) +
                           ylab("Frecuencia") +
                           scale_y_continuous(labels = scales::comma) +
                           theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), legend.title = element_blank())
                ) %>% config(locale = "es") %>%
                  layout(legend = list(x= 1, y = 0.5))
                
              }
            }
            else {
              if(totalcolumna) {
                ggplotly(tooltip = NULL, p =
                           histograma +
                           geom_histogram(aes(y = ..density.., fill = as.character((get(opciones$histogramaRelleno)))), bins = opciones$histogramaNumBins, color="black") +
                           geom_density() +
                           xlab(opciones$histogramaColumna) +
                           ylab("Densidad") +
                           scale_y_continuous(labels = scales::comma) +
                           guides(fill=guide_legend(title=opciones$histogramaRelleno)) +
                           theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), legend.title = element_blank())
                ) %>% config(locale = "es") %>%
                  layout(legend = list(x= 1, y = 0.5))
                
              }
              else {
                ggplotly(tooltip = NULL, p =
                           histograma +
                           stat_count(aes(fill = as.character(get(opciones$histogramaRelleno))), width = opciones$histogramabinWidth, color="black") +
                           xlab(opciones$histogramaColumna) +
                           ylab("Frecuencia") +
                           scale_y_continuous(labels = scales::comma) +
                           guides(fill=guide_legend(title=opciones$histogramaRelleno)) +
                           theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), legend.title = element_blank())
                ) %>% config(locale = "es") %>%
                  layout(legend = list(x= 1, y = 0.5))
                
              }
            }
          })
          incProgress(0.3)
        })
      }
    }
  })
  
  observeEvent(input$bigotesColumna, {
    if(!is.null(input$file)) {
      if(!is.null(input$bigotesColumna) && input$bigotesColumna != "NA") {
        choices = unique(datos$original[,get(input$bigotesColumna)])
        updatePickerInput(session = session,inputId = "bigotesSeleccionar", choices = c(choices)
                          , choicesOpt = list(content = stringr::str_replace_all(str_wrap(choices, width = 50), "\\n", "<br>")))
      }
    }
  })
  
  observeEvent(input$exeOpcionesBigotes, {
    if(!is.null(input$file)) {
      if(!is.null(input$bigotesColumna) && input$bigotesColumna != "NA" && !is.null(input$bigotesSeleccionar)) {
        withProgress(message = "Graficando", {
          opciones$bigotesColumna = input$bigotesColumna
          opciones$bigotesFiltroY1 = input$bigotesFiltroY1
          opciones$bigotesFiltroY2 = input$bigotesFiltroY2
          if (isFALSE(opciones$pacientesPrestaciones)) {
            bdPacientes = agregar(datos$original[get(input$bigotesColumna) %in% input$bigotesSeleccionar], columna_valor = opciones$valor_costo, columnas = c('NRO_IDENTIFICACION', opciones$bigotesColumna), prestaciones = TRUE)
            bigotes = ggplot(bdPacientes, 
                             aes(x=get(opciones$bigotesColumna), 
                                 y=get(opciones$valor_costo), 
                                 group = get(opciones$bigotesColumna)
                             )) +
              geom_boxplot() +
              scale_y_continuous(labels = scales::comma, name = opciones$valor_costo) +
              coord_cartesian(ylim = c(opciones$bigotesFiltroY1, opciones$bigotesFiltroY2)) +
              xlab(opciones$bigotesColumna) +
              ylab(opciones$valor_costo) +
              theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12))
            bdPacientes = NULL
          }
          else {
            bdPacientes = NULL
            bigotes = ggplot(datos$original[get(input$bigotesColumna) %in% input$bigotesSeleccionar], 
                             aes(x=get(opciones$bigotesColumna), 
                                 y=get(opciones$valor_costo), 
                                 group = get(opciones$bigotesColumna)
                             )) +
              geom_boxplot() +
              scale_y_continuous(labels = scales::comma, name = opciones$valor_costo) +
              coord_cartesian(ylim = c(opciones$bigotesFiltroY1, opciones$bigotesFiltroY2)) +
              xlab(opciones$bigotesColumna) +
              ylab(opciones$valor_costo) +
              theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12))
            
          }
          
          output$bigotesFinal = plotly::renderPlotly({
            ggplotly(bigotes) %>%
              config(locale = "es")
          })
        })
      }
    }
  })
  
  observeEvent(input$exeLineaDeTiempo, {
    if(dim(datos$original)[1] != 0) {
      bdLinea = agregar(datos$original, columna_valor = opciones$valor_costo, columnas = "FECHA_PRESTACION", prestaciones = opciones$pacientesPrestaciones)
      LineaDeTiempo = ggplot(bdLinea, aes(x = FECHA_PRESTACION, y = get(opciones$valor_costo))) +
        geom_line(linetype = "dashed")
      bdLinea = NULL
      output$lineadetiempoFinal = renderPlot({
        LineaDeTiempo +
          geom_point() +
          xlab("FECHA") +
          ylab(opciones$valor_costo) +
          scale_y_continuous(labels = scales::comma) 
      })
    }
  })
  
  
  
  output$descargaDescriptivaCSV = downloadHandler(
    filename = function() {
      paste("Descriptiva por ",input$valor_costo," ",input$fecha_min," - ",input$fecha_max,".csv", sep="")
    },
    content = function(file) {
      write.csv(datos$descriptiva, file, row.names = FALSE, na="")
    }, contentType = "text/csv"
  )
  
  output$descargaDescriptivaEXCEL = downloadHandler(
    filename = function() {
      paste("Descriptiva por ",input$valor_costo," ",input$fecha_min," - ",input$fecha_max,".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(datos$descriptiva, path = file)
    }, contentType = "xlsx"
  )
  
  output$descargaDescriptivaEpCSV = downloadHandler(
    filename = function() {
      paste("Episodios por ",input$valor_costo," ",input$fecha_min," - ",input$fecha_max,".csv", sep="")
    },
    content = function(file) {
      write.csv(datos$descriptivaEp, file, row.names = FALSE, na="")
    }, contentType = "text/csv"
  )
  
  output$descargaDescriptivaEpEXCEL = downloadHandler(
    filename = function() {
      paste("Episodios por ",input$valor_costo," ",input$fecha_min," - ",input$fecha_max,".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(datos$descriptivaEp, path = file)
    }, contentType = "xlsx"
  )
  
  output$descargaOutliersCSV = downloadHandler(
    filename = function() {
      paste("outliers por",input$valor_costo,"-",opciones$outliersColumna,".csv", sep="")
    },
    content = function(file) {
      write.csv(datos$outliers, file, row.names = FALSE, na="")
    }, contentType = "text/csv"
  )
  
  output$descargaOutliersEXCEL = downloadHandler(
    filename = function() {
      paste("outliers por",input$valor_costo,"-",opciones$outliersColumna,".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(datos$outliers, path = file)
    }, contentType = "xlsx"
  )
  
  output$descargaAgregacionesCSV = downloadHandler(
    filename = function() {
      paste("agregaciones",".xlsx", sep="")
    },
    content = function(file) {
      write.csv(datos$agregaciones, file, row.names = FALSE, na="")
    }, contentType = "text/csv"
  )
  
  output$descargaAgregacionesEXCEL = downloadHandler(
    filename = function() {
      paste("agregaciones",".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(datos$agregaciones, path = file)
    }, contentType = "xlsx"
  )
  
  output$descargaNTExcel = downloadHandler(
    filename = function() {
      paste("notatécnica",".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(datos$notatecnica, path = file)
    }, contentType = "xlsx"
  )
  
  output$sumasDescriptivaRegistros <- renderText({
    if (!is.null(input$file)) {
      paste("Total registros:", 
            formatC(
              length(datos$original$NRO_IDENTIFICACION), big.mark = ".", decimal.mark = ",", format = "f", digits = 0
            ),
            sep = " "
      )
    }
  })
  output$sumasDescriptivaPacientes <- renderText({
    if (!is.null(input$file)) {
      paste("Total pacientes:", 
            formatC(
              length(unique(datos$original$NRO_IDENTIFICACION)), big.mark = ".", decimal.mark = ",", format = "f", digits = 0
            ),
            sep = " "
      )
    }
  })
  output$sumasDescriptivavalor_costo <- renderText({
    if (!is.null(input$file)) {
      if(nrow(datos$descriptiva) >= 1) {
        paste("Total", paste0(tolower(opciones$valor_costo), ":"), 
              formatC(
                sum(datos$descriptiva$Suma, na.rm = TRUE), big.mark = ".", decimal.mark = ",", format = "f", digits = 0
              ),
              sep = " "
        )
      }
    }
  })
  output$sumasDescriptivaCUPS <- renderText({
    if (!is.null(input$file)) {
      if("CODIGO_CUPS" %in% names(datos$original)) {
        paste("Total prestaciones:", 
              formatC(
                length(unique(datos$original$CODIGO_CUPS)), big.mark = ".", decimal.mark = ",", format = "f", digits = 0
              ),
              sep = " "
        )
      }
    }
  })
  
  ### Sumas de la nota técnica
  
  ### Escenario 1
  output$sumasNtEsc1 = renderText({
    paste("   Suma:",
          paste0("$",
                 formatC(
                   sum(datos$notatecnica$'Primer escenario P75', na.rm = TRUE),
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
                   sum(datos$notatecnica$'Primer escenario P75', na.rm = TRUE)/opciones$meses,
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
              sum(datos$notatecnica$'Primer escenario P75', na.rm = TRUE)/sum(datos$descriptiva$Suma, na.rm = TRUE)*100
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
                   sum(datos$notatecnica$'Segundo escenario media', na.rm = TRUE)/opciones$meses,
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
              sum(datos$notatecnica$'Segundo escenario media', na.rm = TRUE)/sum(datos$descriptiva$Suma, na.rm = TRUE)*100
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
                   sum(datos$notatecnica$'Tercer escenario media truncada 10%', na.rm = TRUE),
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
                   sum(datos$notatecnica$'Tercer escenario media truncada 10%', na.rm = TRUE)/opciones$meses,
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
              sum(datos$notatecnica$'Tercer escenario media truncada 10%', na.rm = TRUE)/sum(datos$descriptiva$Suma, na.rm = TRUE)*100
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
                   sum(datos$notatecnica$'Cuarto escenario media truncada 5%', na.rm = TRUE),
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
                   sum(datos$notatecnica$'Cuarto escenario media truncada 5%', na.rm = TRUE)/opciones$meses,
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
              sum(datos$notatecnica$'Cuarto escenario media truncada 5%', na.rm = TRUE)/sum(datos$descriptiva$Suma, na.rm = TRUE)*100
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
                   sum(datos$notatecnica$'Escenario combinado mayor', na.rm = TRUE),
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
                   sum(datos$notatecnica$'Escenario combinado mayor', na.rm = TRUE)/opciones$meses,
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
              sum(datos$notatecnica$'Escenario combinado mayor', na.rm = TRUE)/sum(datos$descriptiva$Suma, na.rm = TRUE)*100
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
                   sum(datos$notatecnica$'Escenario por variabilidad y frecuencia', na.rm = TRUE),
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
                   sum(datos$notatecnica$'Escenario por variabilidad y frecuencia', na.rm = TRUE)/opciones$meses,
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
              sum(datos$notatecnica$'Escenario por variabilidad y frecuencia', na.rm = TRUE)/sum(datos$descriptiva$Suma, na.rm = TRUE)*100
            ),
            "%"
          ),
          sep = " "
    )
  })
  
  ### Sección dashboard
  
  # PAQUETES = as.data.table(readr::read_csv("datos/PAQUETES.csv"))
  # PAQUETE_PP = PAQUETES[`COMPONENTE` == "PAQUETE"]
  # PAQUETES_CC = PAQUETES[`COMPONENTE` != "PAQUETE"]
  # REF_PAQUETES = as.data.table(readr::read_csv("datos/REFERENTE-PAQUETES.csv"))
  # REF = as.data.table(readr::read_csv("datos/REFERENTE.csv"))
  
  output$paqueteIndexTable = DT::renderDataTable(
    datatable(unique(PAQUETES[, list(`CODIGO PAQUETE` ,ESPECIALIDAD, SERVICIO, DESCRIPCION, INCLUSIONES, EXCLUSIONES)])
              , rownames = F
              , options = 
                list(
                  dom = 'ft',
                  ordering = FALSE,
                  scrollX = TRUE,
                  scrollY = "80vh",
                  pageLength = nrow(unique(PAQUETES[, list(`CODIGO PAQUETE` ,ESPECIALIDAD, SERVICIO, DESCRIPCION, INCLUSIONES, EXCLUSIONES)]))
                )) %>%
      DT::formatStyle(1:6, backgroundColor = 'white')
  )
  
  observeEvent(input$DASHBOARD_actualizar, {
    confirmSweetAlert(session, inputId = "DASHBOARD_actualizar_conf", 
                      title = "Confirmar", 
                      text = "¿Seguro que quieres actualizar los paquetes? Al final, deberá reiniciar la aplicación."
                      , showCloseButton = TRUE
                      , btn_labels = c("Cancelar", "Confirmar"))
  })
  
  observeEvent(input$DASHBOARD_actualizar_conf, {
    if (isTRUE(input$DASHBOARD_actualizar_conf)) {
      unlink("datos/PAQUETES/", recursive = TRUE)
      dir.create("datos/PAQUETES")
      withProgress(value = 0, message = "Actualizando paquetes...", {
        write_feather(sheets_read(paquete_path, sheet = "PAQUETES", col_types = "cccdcccccccdd") 
                      ,"datos/PAQUETES/PAQUETES.feather")
        incProgress(0.3)
        write_feather(sheets_read(paquete_path, sheet = "REFERENTE-PAQUETES") 
                      ,"datos/PAQUETES/REFERENTE-PAQUETES.feather")
        incProgress(0.3)
        write_feather(sheets_read(paquete_path, sheet = "REFERENTE") 
                      ,"datos/PAQUETES/REFERENTE.feather")
        incProgress(0.3)
      })
      sendSweetAlert(session, title = "¡Paquete actualizados efectivamente!", text = "Para ver los datos y gráficos actualizados, por favor recargar la página.", type = "success")
      stopApp()
    }
  })
  
  selectedDash = reactiveValues(
    "paquete" = "",
    "servicio" = "",
    "descripcion" = "",
    "inclusiones" = "",
    "exclusiones" = "",
    "especialidad" = ""
  )
  
  observeEvent(input$paquete, {
    if(!is.null(PAQUETES)) {
      selectedDash$paquete = input$paquete
      
      selectedDash$PAQUETE_PP = PAQUETE_PP[`CODIGO PAQUETE` == input$paquete]
      selectedDash$PAQUETES_CC = PAQUETES_CC[`CODIGO PAQUETE` == selectedDash$paquete]
      
      selectedDash$servicio = selectedDash$PAQUETE_PP$SERVICIO
      selectedDash$especialidad = selectedDash$PAQUETE_PP$ESPECIALIDAD
      selectedDash$descripcion = selectedDash$PAQUETE_PP$`DESCRIPCION`
      selectedDash$inclusiones = selectedDash$PAQUETE_PP$`INCLUSIONES`
      selectedDash$exclusiones = selectedDash$PAQUETE_PP$`EXCLUSIONES`
    }
  })
  
  output$TABLA_paquete = DT::renderDataTable({
    if(!is.null(PAQUETES)) {
      datatable(
        selectedDash$PAQUETES_CC[, c("CUMS/CUPS", "PRESTACION", "COMPONENTE", "TIPO DE COSTO", "VALOR", "COSTO")]
        , rownames = F
        , selection = 'single'
        , options = list(
          dom='ft'
          , language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
          , pageLength = nrow(PAQUETES)
          , ordering = FALSE
          , scrollX = TRUE
          , scrollY = "60vh"
        )
      ) %>%
        DT::formatCurrency(
          columns = c("VALOR", "COSTO")
          , digits = 0
          , mark = ".", dec.mark = ","
        )
    }
  })
  
  output$BOX_especialidad = renderValueBox({
    if(!is.null(PAQUETES)) {
      valueBox(
        value = selectedDash$especialidad
        , subtitle = "Especialidad"
        , icon = icon("stethoscope", lib = "font-awesome")
        , color = "yellow"
      )
    }
  })
  
  output$BOX_valortotal = renderValueBox({
    if(!is.null(PAQUETES)) {
      valueBox(
        value = formatAsCurrency(selectedDash$PAQUETE_PP$VALOR)
        , subtitle = "Valor del paquete"
        , icon = icon("tags", lib = "font-awesome")
        , color = "green"
      )
    }
  })
  
  output$BOX_costototal = renderValueBox({
    if(!is.null(PAQUETES)) {
      valueBox(
        value = formatAsCurrency(selectedDash$PAQUETE_PP$COSTO)
        , subtitle = "Costo del paquete"
        , icon = icon("money-check", lib = "font-awesome")
        , color = "red"
      )
    }
  })
  
  output$BOX_codpaquete = renderValueBox({
    if(!is.null(PAQUETES)) {
      valueBox(
        value = selectedDash$paquete
        , subtitle = "Código del paquete"
        , icon = icon("qrcode", lib = "font-awesome")
        , color = "blue"
      )
    }
  })
  
  output$BOX_servicio = renderText({
    if(!is.null(PAQUETES))
      selectedDash$servicio
  })
  
  output$BOX_descripcion = renderValueBox({
    if(!is.null(PAQUETES)) {
      valueBox(
        value = str_wrap(selectedDash$descripcion, width = 30)
        , subtitle = "Descripción del paquete"
        , icon = icon("hashtag", lib = "font-awesome")
        , color = "teal"
      )
    }
  })
  
  output$BOX_inclusiones = renderText({
    if(!is.null(PAQUETES)) 
      selectedDash$inclusiones
  })
  
  output$BOX_exclusiones = renderText({
    if(!is.null(PAQUETES)) 
      selectedDash$exclusiones
  })
  
  output$PLOT_ref_paquete = renderggiraph({
    if(!is.null(PAQUETES)) 
      REFPgrafico(paquetes = selectedDash$PAQUETE_PP, referente = REF_PAQUETES, CUMSCUPS = selectedDash$paquete, valor_costo = input$DASHBOARD_valor_costo)
  })
  
  output$PLOT_ref_cumscups = renderggiraph({
    if(!is.null(input$TABLA_paquete_rows_selected))
      REFgrafico(paquetes = selectedDash$PAQUETES_CC, referente = REF[`CODIGO PAQUETE` == selectedDash$paquete], CUMSCUPS = selectedDash$PAQUETES_CC[input$TABLA_paquete_rows_selected, list(`CUMS/CUPS`)], valor_costo = input$DASHBOARD_valor_costo)
  })
  
  output$PLOT_pie_paquete = renderggiraph({
    if(!is.null(PAQUETES)) 
      PIEchart(paquetes = selectedDash$PAQUETES_CC, columna = input$COL_pie_paquete, valor_costo = input$DASHBOARD_valor_costo)
  })
  
  output$TABLA_pie_paquete_resumen = DT::renderDataTable({
    if(!is.null(PAQUETES)) {
      datatable(
        resumenComp(tabla = selectedDash$PAQUETES_CC, columna = input$COL_pie_paquete, colsum = input$DASHBOARD_valor_costo)
        , rownames = F
        , selection = 'none'
        , options = list(
          dom='ft'
          , language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
          , pageLength = nrow(PAQUETES)
          , ordering = FALSE
          , scrollX = TRUE
          , scrollY = "30vh"
        )
      ) %>%
        formatStyle(columns = c(input$COL_pie_paquete, "SUMA", "PARTICIPACIÓN (%)"), backgroundColor = "white")
    }
  })
  
  output$PLOT_pie_componente = renderggiraph({
    if(!is.null(input$COL_pie_componente))
      PIEchart(paquetes = selectedDash$PAQUETES_CC[COMPONENTE %in% input$COL_pie_componente], columna = input$COL_pie_componente_en, valor_costo = input$DASHBOARD_valor_costo)
  })
  
  output$TABLA_pie_componente = DT::renderDataTable({
    if(!is.null(input$COL_pie_componente)) {
      datatable(
        resumenComp(tabla = selectedDash$PAQUETES_CC[COMPONENTE %in% input$COL_pie_componente], columna = input$COL_pie_componente_en, colsum = input$DASHBOARD_valor_costo)
        , rownames = F
        , selection = 'none'
        , options = list(
          dom='ft'
          , language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
          , pageLength = nrow(PAQUETES)
          , ordering = FALSE
          , scrollX = TRUE
          , scrollY = "20vh"
        )
      ) %>%
        formatStyle(columns = c(input$COL_pie_componente_en, "SUMA", "PARTICIPACIÓN (%)"), backgroundColor = "white")
    }
  })
  
  output$PLOT_pie_tipocosto = renderggiraph({
    if(!is.null(input$COL_pie_tipocosto))
      PIEchart(paquetes = selectedDash$PAQUETES_CC[`TIPO DE COSTO` %in% input$COL_pie_tipocosto], columna = input$COL_pie_tipocosto_en, valor_costo = input$DASHBOARD_valor_costo)
  })
  
  output$TABLA_pie_tipocosto = DT::renderDataTable({
    if(!is.null(input$COL_pie_tipocosto)) {
      datatable(
        resumenComp(tabla = selectedDash$PAQUETES_CC[`TIPO DE COSTO` %in% input$COL_pie_tipocosto], columna = input$COL_pie_tipocosto_en, colsum = input$DASHBOARD_valor_costo)
        , rownames = F
        , selection = 'none'
        , options = list(
          dom='ft'
          , language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
          , pageLength = nrow(PAQUETES)
          , ordering = FALSE
          , scrollX = TRUE
          , scrollY = "20vh"
        )
      ) %>%
        formatStyle(columns = c(input$COL_pie_tipocosto_en, "SUMA", "PARTICIPACIÓN (%)"), backgroundColor = "white")
    }
  })
  
  ### Sección Pricing
  
  observeEvent(input$pricingActualizar, {
    confirmSweetAlert(session, inputId = "pricingActualizar_conf", 
                      title = "Confirmar", 
                      text = "¿Seguro que quieres actualizar el pricing? Al final, deberá reiniciar la aplicación."
                      , showCloseButton = TRUE
                      , btn_labels = c("Cancelar", "Confirmar"))
  })
  
  observeEvent(input$pricingActualizar_conf, {
    if (isTRUE(input$pricingActualizar_conf)) {
      withProgress(value = 0, message = "Actualizando pricing...", min = 0, max = 1.1, {
        unlink("datos/PRICING/", recursive = TRUE)
        dir.create("datos/PRICING")
        pricingList = drive_ls(path = as_id(pricing_path))
        incProgress(0.1, message = "¡Archivos leidos!")
        i = 1
        while (i <= length(pricingList$id)) {
          drive_download(file = as_id(pricingList$id[i]), path = paste0("datos/PRICING/", pricingList$name[i]), overwrite = T)
          i = i + 1
          incProgress(1/length(pricingList$id))
        }
        i = NULL
        
      })
      sendSweetAlert(session, title = "¡Pricing actualizados efectivamente!", text = "Para ver los datos y gráficos actualizados, por favor recargar la página.", type = "success")
      stopApp()
    }
  })
  
  pricing = reactiveValues()
  
  observeEvent(input$pricingSelect, {
    pricing$bd = as.data.table(readr::read_csv(paste0("datos/PRICING/", input$pricingSelect, ".csv"), na = c("-", "#N/A", "#DIV/0!", "NA")))
    
    pricing$datos$CODIGO_CUPS = as.character(pricing$datos$CODIGO_CUPS)
    pricing$datos$VALOR_UNITARIO = as.numeric(as.character(pricing$datos$VALOR_UNITARIO))
    pricing$datos$MAXIMO = as.numeric(as.character(pricing$datos$MAXIMO))
    pricing$datos$MINIMO = as.numeric(as.character(pricing$datos$MINIMO))
    pricing$bd$`MEDIA` = as.numeric(as.character(pricing$bd$"MEDIA"))
    pricing$datos$RELATIVO_MINIMO = as.numeric(as.character(pricing$datos$RELATIVO_MINIMO))
    pricing$datos$RELATIVO_MEDIA = as.numeric(as.character(pricing$datos$RELATIVO_MEDIA))
    
    pricing$bdsd.media = round(abs(sd(pricing$datos$RELATIVO_MEDIA, na.rm = TRUE)), digits = 3)
    pricing$bdsd.min = round(abs(median(pricing$datos$RELATIVO_MINIMO, na.rm = TRUE)), digits = 3)
    pricing$descriptiva = data.frame("x" = c("Valor Unitario Mínimo", "Valor Unitario Máximo", "Valor Unitario Promedio", "Mínimo", "Máximo", "Media"), "y" = c(0,0,0,0,0,0))
    
  })
  
  output$pricingOutPrestacion = renderUI({
    pickerInput("pricingFiltroPrestacion", label = "Nombre de Prestación",
                choices = unique(pricing$datos$NOMBRE_PRESTACION), multiple = TRUE,
                options = list(`actions-box` = TRUE, `deselect-all-text` = "Deseleccionar todos",
                               `select-all-text` = "Seleccionar todos", `live-search` = TRUE))
  })
  
  output$pricingOutEntidad = renderUI({
    pickerInput("pricingFiltroEntidad", label = "Nombre Entidad",
                choices = unique(pricing$datos$NOMBRE_ENTIDAD), multiple = TRUE,
                options = list(`actions-box` = TRUE, `deselect-all-text` = "Deseleccionar todos",
                               `select-all-text` = "Seleccionar todos", `live-search` = TRUE))
  })
  
  output$pricingOutObs = renderUI({
    pickerInput("pricingFiltroObservacion", label = "Observación",
                choices = unique(pricing$datos$OBSERVACIÓN),
                multiple = TRUE, 
                options = list(`actions-box` = TRUE, `deselect-all-text` = "Deseleccionar todos",
                               `select-all-text` = "Seleccionar todos", `live-search` = TRUE))
  })
  
  output$pricingOutRel_media = renderUI({
    sliderInput("pricingRel_media", "Relativo a la media", min = -2*pricing$bdsd.media, max = 2*pricing$bdsd.media, value = c(-2*pricing$bdsd.media, 2*pricing$bdsd.media), step = 0.01)
  })
  
  output$pricingOutRel_min = renderUI({
    sliderInput("pricingRel_min", "Relativo al mínimo", min = -2*pricing$bdsd.min, max = 2*pricing$bdsd.min, value = c(-2*pricing$bdsd.min, 2*pricing$bdsd.min), step = 0.01)
  })
  
  observeEvent(input$pricingEjecutar, {
    pricing$filtrado = pricing$bd
    pricing$filtrado = pricing$filtrado[NOMBRE_PRESTACION %in% input$pricingFiltroPrestacion & NOMBRE_ENTIDAD %in% input$pricingFiltroEntidad & OBSERVACIÓN %in% input$pricingFiltroObservacion]
    
    if (input$pricingRel_media[1] != -2*pricing$bdsd.media) {
      pricing$filtrado = pricing$filtrado[RELATIVO_MEDIA >= input$pricingRel_media[1]]
    }
    if (input$pricingRel_media[2] != 2*pricing$bdsd.media) {
      pricing$filtrado = pricing$filtrado[RELATIVO_MEDIA <= input$pricingRel_media[2]]
    }
    
    if (input$pricingRel_min[1] != -2*pricing$bdsd.min) {
      pricing$filtrado = pricing$filtrado[RELATIVO_MINIMO >= input$pricingRel_min[1]]
    }
    if (input$pricingRel_min[2] != 2*pricing$bdsd.min) {
      pricing$filtrado = pricing$filtrado[RELATIVO_MINIMO >= input$pricingRel_min[2]]
    }
    
    pricing$descriptiva$y =    c(min(pricing$filtrado$VALOR_UNITARIO, na.rm = T),
                                 max(pricing$filtrado$VALOR_UNITARIO, na.rm = T),
                                 mean(pricing$filtrado$VALOR_UNITARIO, na.rm = TRUE),
                                 min(pricing$filtrado$MINIMO, na.rm = T),
                                 max(pricing$filtrado$MAXIMO, na.rm = T),
                                 max(pricing$filtrado$"MEDIA", na.rm = TRUE))
    
  })
  
  observeEvent(input$pricingEjecutar, {
    output$pricingCUPSCUMS = DT::renderDataTable({
      datatable(pricing$filtrado[, 
                                 list(
                                   "MIN" = min(VALOR_UNITARIO, na.rm = TRUE),
                                   "MEDIA" = mean(VALOR_UNITARIO, na.rm = TRUE), 
                                   "MAX" = max(VALOR_UNITARIO, na.rm = TRUE), 
                                   "MEDIA MERCADO" = min(MEDIA, na.rm = TRUE)), 
                                 by= c("NOMBRE_ENTIDAD", "CODIGO_CUPS")]
                , rownames = FALSE
                , options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                 ordering=T)) %>%
        formatCurrency(c("MIN", "MEDIA", "MAX", "MEDIA MERCADO"), mark = ".", dec.mark = ",")
    })
  })
  
  observeEvent(input$pricingEjecutar, {
    output$pricingEntidades = renderggiraph({
      girafe(
        ggobj = ggplot(pricing$filtrado, aes(NOMBRE_ENTIDAD, fill = OBSERVACIÓN, data_id = OBSERVACIÓN, tooltip = OBSERVACIÓN)) +
          geom_bar_interactive() +
          theme_minimal() +
          scale_y_continuous(labels = scales::comma, name = "# de prestaciones") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="left") +
          labs(y= "Conteo", x = "Nombre Entidad")
        , options = list(opts_selection(type = "none", only_shiny = FALSE))
        , width_svg = 18, height_svg = 9
      )
    })
  })
  
  observeEvent(input$pricingEjecutar, {
    output$pricingPrestaciones = renderggiraph({
      girafe(
        ggobj =  ggplot(pricing$descriptiva, aes(x = x, y = as.numeric(as.character(y)), fill = x, data_id = x, tooltip = formatAsCurrency(as.numeric(as.character(y))))) +
          geom_bar_interactive(stat = "identity") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="left") +
          scale_y_continuous(labels = scales::comma, name = "Valor") +
          labs(y= "Valor", x = "Comparación")
        , options = list(opts_selection(type = "none", only_shiny = FALSE))
        , width_svg = 18, height_svg = 9
      )
    })
  })
  
  ###Dashboard NT
  
  observeEvent(input$dashNT_actualizar, {
    confirmSweetAlert(session, inputId = "dashNT_actualizar_conf", 
                      title = "Confirmar", 
                      text = "¿Seguro que quieres actualizar las notas técnicas? Al final, deberá reiniciar la aplicación."
                      , showCloseButton = TRUE
                      , btn_labels = c("Cancelar", "Confirmar"))
  })
  
  observeEvent(input$dashNT_actualizar_conf, {
    if (isTRUE(input$dashNT_actualizar_conf)) {
      unlink("datos/NTs/", recursive = TRUE)
      dir.create("datos/NTs")
      withProgress(value = 0, message = "Actualizando notas técnicas...", {
        write_feather(sheets_read(nts_path, sheet = "NTs", col_types = "ccddd") 
                      ,"datos/NTs/NTs.feather")
        incProgress(0.3)
        write_feather(sheets_read(nts_path, sheet = "INDICE", col_types = "ccdcccd") 
                      ,"datos/NTs/INDICE.feather")
        incProgress(0.3)
        write_feather(sheets_read(nts_path, sheet = "INCLUSIONES", col_types = "ccdc") 
                      ,"datos/NTs/INCLUSIONES.feather")
        incProgress(0.3)
        saveRDS(mapaValoresNT(as.data.table(sheets_read(nts_path, sheet = "INDICE", col_types = "ccdcccd"))) %>% layout(autosize = TRUE), "datos/NTs/NTmapa.rds")
      })
      sendSweetAlert(session, title = "¡Notas técnicas actualizados efectivamente!", text = "Para ver los datos y gráficos actualizados, por favor recargar la página.", type = "success")
      stopApp()
    }
  })
  
  NT_TEMP = reactiveValues("INDICE" = NTs_INDICE, "NT" = NTs_NT, "INCLUSIONES" = NTs_INCLUSIONES)
  
  observeEvent(input$dashNT_select, {
    NT_TEMP$NT = NTs_NT[COD_NT == input$dashNT_select]
    NT_TEMP$INDICE = NTs_INDICE[COD_NT == input$dashNT_select]
    NT_TEMP$INCLUSIONES = NTs_INCLUSIONES[COD_NT == input$dashNT_select]
  })
  
  output$dashNTNombreEntidad = renderValueBox({
    if(!is.null(NT_TEMP$INDICE)) {
      valueBox(
        value = NT_TEMP$INDICE$NOM_PRESTADOR
        , subtitle = "Prestador"
        , icon = icon("stethoscope", lib = "font-awesome")
        , color = "yellow"
      )
    }
  })
  
  output$dashNT_ValorMes = renderValueBox({
    if(!is.null(NT_TEMP$INDICE)) {
      valueBox(
        value = formatAsCurrency(NT_TEMP$INDICE$VALOR_MES)
        , subtitle = "Valor total a mes"
        , icon = icon("dollar-sign", lib = "font-awesome")
        , color = "green"
      )
    }
  })
  
  output$dashNT_Poblacion = renderValueBox({
    if(!is.null(NT_TEMP$INDICE)) {
      valueBox(
        value = format(NT_TEMP$INDICE$POBLACION, scientific = F, big.mark = ".", small.mark = ",")
        , subtitle = "Pobalción"
        , icon = icon("users", lib = "font-awesome")
        , color = "blue"
      )
    }
  })
  
  output$dashNT_Departamento = renderValueBox({
    if(!is.null(NT_TEMP$INDICE)) {
      valueBox(
        value = NT_TEMP$INDICE$DEPARTAMENTO
        , subtitle = NT_TEMP$INDICE$CIUDADES
        , icon = icon("city", lib = "font-awesome")
        , color = "aqua"
      )
    }
  })
  
  output$dashNT_Inclusiones = DT::renderDataTable({
    if(!is.null(NT_TEMP$INCLUSIONES)) {
      datatable(
        NT_TEMP$INCLUSIONES[INCLUIDO == 1, c("OBJETO", "NOTAS")]
        , rownames = F
        , selection = 'none'
        , colnames = c("Observación", "Notas")
        , options = list(
          dom='ft'
          , language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
          , pageLength = nrow(PAQUETES)
          , ordering = FALSE
          , scrollX = TRUE
          , scrollY = "60vh"
        )
      ) %>%
        DT::formatStyle(columns = 1:4, valueColumns = 1, backgroundColor = "white")
    }
  })
  
  output$dashNT_Exclusiones = DT::renderDataTable({
    if(!is.null(NT_TEMP$INCLUSIONES)) {
      datatable(
        NT_TEMP$INCLUSIONES[INCLUIDO == 0, c("OBJETO", "NOTAS")]
        , rownames = F
        , selection = 'none'
        , colnames = c("Observación", "Notas")
        , options = list(
          dom='ft'
          , language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
          , pageLength = nrow(PAQUETES)
          , ordering = FALSE
          , scrollX = TRUE
          , scrollY = "60vh"
        )
      ) %>%
        DT::formatStyle(columns = 1:4, valueColumns = 1, backgroundColor = "white")
    }
  })
  
  output$dashNT_PLOT_Agrupadores = renderGirafe({
    PIEchart(as.data.table(NT_TEMP$NT), "AGRUPADOR", valor_costo = "VALOR_MES")
  })
  
  output$dashNT_NT = DT::renderDataTable({
    if(!is.null(NT_TEMP$NT)) {
      datatable(
        NT_TEMP$NT[, c("AGRUPADOR", "FREC_MES", "CME", "VALOR_MES")]
        , rownames = F
        , selection = 'none'
        , colnames = c("Agrupador", "Frecuencia a mes", "CME", "Valor a mes")
        , options = list(
          dom='ft'
          , language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
          , pageLength = nrow(PAQUETES)
          , ordering = FALSE
          , scrollX = TRUE
          , scrollY = "60vh"
        )
      ) %>%
        DT::formatCurrency(
          columns = c("CME", "VALOR_MES")
          , digits = 0, mark = ".", dec.mark = ","
        ) %>%
        DT::formatStyle(columns = 1:4, valueColumns = 1, backgroundColor = "white")
    }
  })
  
  ###INDICE
  
  output$indiceNT = DT::renderDataTable({
    if(!is.null(NTs_INDICE)) {
      datatable(
        NTs_INDICE[, -c("COD_DEPARTAMENTO")]
        , rownames = F
        , selection = 'none'
        , colnames = c("Nombre NT", "Prestador", "Población", "Departamento", "Ciudades", "Valor a mes")
        , options = list(
          dom='ft'
          , language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
          , pageLength = nrow(PAQUETES)
          , ordering = FALSE
          , scrollX = TRUE
          , scrollY = "60vh"
        )
      ) %>%
        DT::formatCurrency(
          columns = c("VALOR_MES")
          , digits = 0, mark = ".", dec.mark = ","
        ) %>%
        DT::formatRound( columns = "POBLACION", mark = ".")
    }
  })
  
  output$indiceNT_Mapa = renderPlotly(
    MAPA_NT
  )
  
  ###Comprar NT con BD
  
  compararNT_TEMP = reactiveValues(
    NT = data.table()
  )
  
  observeEvent(input$compararNT_ejecutar, {
    if(!is.null(input$file)) {
      if(input$compararNTcolumna != "NA") {
        
        opciones$compararNTcolumna = input$compararNTcolumna
        
        tryCatch(
          expr = {
            compararNT_TEMP$NT = NTs_NT[COD_NT == input$compararNT_select]
            
            compararNT_TEMP$descBSumas = descriptivaBasicaTrans(data = descriptivaBasica(data = datos$original
                                                                                         , agrupador = opciones$compararNTcolumna
                                                                                         , columna_valor = opciones$valor_costo
                                                                                         , prestaciones = opciones$pacientesPrestaciones
                                                                                         , columna_fecha = "FECHA_PRESTACION")
                                                                , agrupador = opciones$compararNTcolumna
                                                                , frec = FALSE
            )
            compararNT_TEMP$descBFrecs = descriptivaBasicaTrans(data = descriptivaBasica(data = datos$original
                                                                                         , agrupador = opciones$compararNTcolumna
                                                                                         , columna_valor = opciones$valor_costo
                                                                                         , prestaciones = opciones$pacientesPrestaciones
                                                                                         , columna_fecha = "FECHA_PRESTACION")
                                                                , agrupador = opciones$compararNTcolumna
                                                                , suma = FALSE
            )
            
            compararNT_TEMP$difsValorRIPS = diferenciaValorRIPS(sumas = compararNT_TEMP$descBSumas
                                                                , NT = compararNT_TEMP$NT
                                                                , porcentaje = FALSE)
            
            compararNT_TEMP$difsValorRIPSperc = diferenciaValorRIPS(sumas = compararNT_TEMP$descBSumas
                                                                    , NT = compararNT_TEMP$NT
                                                                    , porcentaje = TRUE)
            
            compararNT_TEMP$difsValorCME = diferenciaValorCME(frecs = compararNT_TEMP$descBFrecs
                                                              , NT = compararNT_TEMP$NT
                                                              , porcentaje = FALSE)
            
            compararNT_TEMP$difsValorCMEperc = diferenciaValorCME(frecs = compararNT_TEMP$descBFrecs
                                                                  , NT = compararNT_TEMP$NT
                                                                  , porcentaje = TRUE)
            
            compararNT_TEMP$totales = diferenciasTotales(frecs = compararNT_TEMP$descBFrecs, sumas = compararNT_TEMP$descBSumas, NT = compararNT_TEMP$NT)
          },
          error = function(e) {
            sendSweetAlert(
              session = session,
              title = "Error",
              text = e[1],
              type = "error"
            )
          }
        )
        
        
      }
    }
  })
  
  output$compararNT_totales = DT::renderDataTable({
    if (!is.null(compararNT_TEMP$totales)) {
      DT::datatable(compararNT_TEMP$totales[["totales"]], 
                    class = 'cell-border stripe', 
                    rownames = FALSE, 
                    extensions = 'ColReorder',
                    selection = 'none',
                    options = list(
                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 15, autoWidth = FALSE, ordering=F, scrollX = TRUE, colReorder = TRUE,
                      dom = 't'
                    )) %>%
        DT::formatStyle(3, backgroundColor = styleInterval(
          cuts = c(0)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        DT::formatStyle(4, backgroundColor = styleInterval(
          cuts = c(1)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        formatCurrency(c(2,3), mark = ".", dec.mark = ",", digits = 0) %>%
        formatPercentage(4, digits = 0, mark = ".", dec.mark = ",")
    }
  })
  
  output$compararNT_totalMesRIPS = DT::renderDataTable({
    if (!is.null(compararNT_TEMP$totales)) {
      DT::datatable(compararNT_TEMP$totales[["totalMesRIPS"]], 
                    class = 'cell-border stripe', 
                    rownames = FALSE, 
                    extensions = 'ColReorder',
                    selection = 'none',
                    options = list(
                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 15, autoWidth = FALSE, ordering=F, scrollX = TRUE, colReorder = TRUE,
                      dom = 't', scrollY = "50vh", scrollCollapse = TRUE
                    )) %>%
        DT::formatStyle(3, backgroundColor = styleInterval(
          cuts = c(0)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        DT::formatStyle(4, backgroundColor = styleInterval(
          cuts = c(1)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        formatCurrency(c(2,3), mark = ".", dec.mark = ",", digits = 0) %>%
        formatPercentage(4, digits = 0, mark = ".", dec.mark = ",")
    }
  })
  
  output$compararNT_totalAgrupadorRIPS = DT::renderDataTable({
    if (!is.null(compararNT_TEMP$totales)) {
      DT::datatable(compararNT_TEMP$totales[["totalAgrupadorRIPS"]], 
                    class = 'cell-border stripe', 
                    rownames = FALSE, 
                    extensions = 'ColReorder',
                    selection = 'none',
                    options = list(
                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 15, autoWidth = FALSE, ordering=F, scrollX = TRUE, colReorder = TRUE,
                      dom = 't', scrollY = "50vh", scrollCollapse = TRUE
                    )) %>%
        DT::formatStyle(3, backgroundColor = styleInterval(
          cuts = c(0)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        DT::formatStyle(4, backgroundColor = styleInterval(
          cuts = c(1)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        formatCurrency(c(2,3), mark = ".", dec.mark = ",", digits = 0) %>%
        formatPercentage(4, digits = 0, mark = ".", dec.mark = ",")
    }
  })
  
  output$compararNT_totalMesCME = DT::renderDataTable({
    if (!is.null(compararNT_TEMP$totales)) {
      DT::datatable(compararNT_TEMP$totales[["totalMesCME"]], 
                    class = 'cell-border stripe', 
                    rownames = FALSE, 
                    selection = 'none',
                    extensions = 'ColReorder',
                    options = list(
                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 15, autoWidth = FALSE, ordering=F, scrollX = TRUE, colReorder = TRUE,
                      dom = 't', scrollY = "50vh", scrollCollapse = TRUE
                    )) %>%
        DT::formatStyle(3, backgroundColor = styleInterval(
          cuts = c(0)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        DT::formatStyle(4, backgroundColor = styleInterval(
          cuts = c(1)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        formatCurrency(c(2,3), mark = ".", dec.mark = ",", digits = 0) %>%
        formatPercentage(4, digits = 0, mark = ".", dec.mark = ",")
    }
  })
  
  output$compararNT_totalAgrupadorCME = DT::renderDataTable({
    if (!is.null(compararNT_TEMP$totales)) {
      DT::datatable(compararNT_TEMP$totales[["totalAgrupadorCME"]], 
                    class = 'cell-border stripe', 
                    rownames = FALSE, 
                    extensions = 'ColReorder',
                    selection = 'none',
                    options = list(
                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 15, autoWidth = FALSE, ordering=F, scrollX = TRUE, colReorder = TRUE,
                      dom = 't', scrollY = "50vh", scrollCollapse = TRUE
                    )) %>%
        DT::formatStyle(3, backgroundColor = styleInterval(
          cuts = c(0)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        DT::formatStyle(4, backgroundColor = styleInterval(
          cuts = c(1)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        formatCurrency(c(2,3), mark = ".", dec.mark = ",", digits = 0) %>%
        formatPercentage(4, digits = 0, mark = ".", dec.mark = ",")
    }
  })
  
  output$compararNT_descBSumas = DT::renderDataTable({
    if (!is.null(compararNT_TEMP$descBSumas)) {
      DT::datatable(compararNT_TEMP$descBSumas, 
                    class = 'cell-border stripe', 
                    rownames = FALSE, 
                    extensions = 'ColReorder',
                    selection = 'none',
                    options = list(
                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 15, autoWidth = FALSE, ordering=T, scrollX = TRUE, colReorder = TRUE,
                      scrollY = "50vh", scrollCollapse = TRUE)) %>%
        formatCurrency(-c(1), mark = ".", dec.mark = ",", digits = 0)
    }
  })
  
  output$compararNT_descBFrecs = DT::renderDataTable({
    if (!is.null(compararNT_TEMP$descBFrecs)) {
      DT::datatable(compararNT_TEMP$descBFrecs, 
                    class = 'cell-border stripe', 
                    rownames = FALSE, 
                    extensions = 'ColReorder',
                    options = list(
                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 15, autoWidth = FALSE, ordering=T, scrollX = TRUE, colReorder = TRUE,
                      scrollY = "50vh", scrollCollapse = TRUE)) %>%
        DT::formatRound(-c(1),mark = ".", dec.mark = ",", digits = 0)
    }
  })
  
  output$compararNT_difsValorRIPS = DT::renderDataTable({
    if (!is.null(compararNT_TEMP$difsValorRIPS)) {
      DT::datatable(compararNT_TEMP$difsValorRIPS, 
                    class = 'cell-border stripe', 
                    rownames = FALSE, 
                    extensions = 'ColReorder',
                    selection = 'none',
                    options = list(
                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 15, autoWidth = FALSE, ordering=T, scrollX = TRUE, colReorder = TRUE,
                      scrollY = "50vh", scrollCollapse = TRUE)) %>%
        DT::formatStyle(-c(1), backgroundColor = styleInterval(
          cuts = c(0)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        DT::formatCurrency(-c(1),mark = ".", dec.mark = ",", digits = 0)
    }
  })
  
  output$compararNT_difsValorRIPSperc = DT::renderDataTable({
    if (!is.null(compararNT_TEMP$difsValorRIPSperc)) {
      DT::datatable(compararNT_TEMP$difsValorRIPSperc, 
                    class = 'cell-border stripe', 
                    rownames = FALSE, 
                    extensions = 'ColReorder',
                    selection = 'none',
                    options = list(
                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 15, autoWidth = FALSE, ordering=T, scrollX = TRUE, colReorder = TRUE,
                      scrollY = "50vh", scrollCollapse = TRUE)) %>%
        DT::formatStyle(-c(1), backgroundColor = styleInterval(
          cuts = c(1)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        DT::formatPercentage(-c(1),mark = ".", dec.mark = ",", digits = 0)
    }
  })
  
  output$compararNT_difsValorCME = DT::renderDataTable({
    if (!is.null(compararNT_TEMP$difsValorCME)) {
      DT::datatable(compararNT_TEMP$difsValorCME, 
                    class = 'cell-border stripe', 
                    rownames = FALSE, 
                    extensions = 'ColReorder',
                    selection = 'none',
                    options = list(
                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 15, autoWidth = FALSE, ordering=T, scrollX = TRUE, colReorder = TRUE,
                      scrollY = "50vh", scrollCollapse = TRUE)) %>%
        DT::formatStyle(-c(1), backgroundColor = styleInterval(
          cuts = c(0)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        DT::formatCurrency(-c(1),mark = ".", dec.mark = ",", digits = 0)
    }
  })
  
  output$compararNT_difsValorCMEperc = DT::renderDataTable({
    if (!is.null(compararNT_TEMP$difsValorCMEperc)) {
      DT::datatable(compararNT_TEMP$difsValorCMEperc, 
                    class = 'cell-border stripe', 
                    rownames = FALSE, 
                    extensions = 'ColReorder',
                    selection = 'none',
                    options = list(
                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 15, autoWidth = FALSE, ordering=T, scrollX = TRUE, colReorder = TRUE,
                      scrollY = "50vh", scrollCollapse = TRUE)) %>%
        DT::formatStyle(-c(1), backgroundColor = styleInterval(
          cuts = c(1)
          , values = c(
            "rgb(255, 145, 145)",
            "rgb(145, 255, 145)"
          )
        )) %>%
        DT::formatPercentage(-c(1),mark = ".", dec.mark = ",", digits = 0)
    }
  })
  
})