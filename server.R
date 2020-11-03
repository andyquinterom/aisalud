shinyServer(function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
 # Modulo prepara ---------------------------------------------
  
  datos_modulos <- callModule(
    module = prepara_server,
    id = "prepara_test",
    nombre_id = "prepara_test"
  )
  
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
          datos$colnames_num <- datos$colnames[columnas_num]
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
                            inputId = "dash_nt_comparar_agrupador", 
                            choices = datos$colnames,
                            selected = opciones$dash_nt_comparar_agrupador)
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
  
  # Modulo descriptiva ---------------------------------------------------------
  
  callModule(
    module = descriptiva_server,
    id = "descriptiva_test",
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
  
  output$descriptiva_sumas_registros <- renderText({
    if (!is.null(input$file)) {
      paste("Total registros:", 
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
    id = "outliers_test",
    datos = datos_modulos,
    opciones = opciones
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
          aes(x=get(opciones$histograma_col)))
        
        output$histograma_render = renderPlotly({
          if(is.null(opciones$histograma_fill) || 
             opciones$histograma_fill %in% c(opciones$histograma_col,
                                             "VALOR",
                                             "COSTO")) {
            if(opciones$histograma_col %in% datos$colnames_num) {
              ggplotly(
                tooltip = FALSE,
                p = histograma +
                  geom_histogram(
                    bins = opciones$histograma_bins, 
                    color="black",
                    aes(y = ..density..)) +
                  geom_density() +
                  xlab(opciones$histograma_col) +
                  ylab("Densidad") +
                  scale_y_continuous(labels = scales::comma) +
                  theme(axis.text.x = element_text(angle = 90,
                                                   hjust = 1,
                                                   size = 12),
                        legend.title = element_blank())) %>% 
                config(locale = "es") %>%
                layout(legend = list(x= 1, y = 0.5))
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
              ggplotly(
                tooltip = FALSE,
                p = histograma +
                  geom_histogram(
                    bins = opciones$histograma_bins, 
                    color="black",
                    aes(y = ..density..,
                        fill = get(opciones$histograma_fill))) +
                  geom_density() +
                  xlab(opciones$histograma_col) +
                  ylab("Densidad") +
                  scale_y_continuous(labels = scales::comma) +
                  guides(
                    fill=guide_legend(title=opciones$histograma_fill)) +
                  theme(axis.text.x = element_text(angle = 90,
                                                   hjust = 1,
                                                   size = 12),
                        legend.title = element_blank())) %>% 
                config(locale = "es") %>%
                layout(legend = list(x= 1, y = 0.5))
              
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
  
  # Dashboard NT --------------------------------------------------------------
  
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
  
  dash_nt_valores <- reactiveValues(
    "indice" = dash_nt_indice,
    "datos" = dash_nt_datos, 
    "inclusiones" = dash_nt_inclusiones)
  
  observeEvent(input$dash_nt_board_select, {
    dash_nt_valores$datos <- dash_nt_datos[
      COD_NT == input$dash_nt_board_select]
    dash_nt_valores$indice <- dash_nt_indice[
      COD_NT == input$dash_nt_board_select]
    dash_nt_valores$inclusiones <- dash_nt_inclusiones[
      COD_NT == input$dash_nt_board_select]
  })
  
  output$dash_nt_entidad <- renderValueBox({
    if(!is.null(dash_nt_valores$indice)) {
      valueBox(
        value = dash_nt_valores$indice$NOM_PRESTADOR,
        subtitle = "Prestador", 
        icon = icon("stethoscope", lib = "font-awesome"), 
        color = "yellow"
      )
    }
  })
  
  output$dash_nt_valor_mes <- renderValueBox({
    if(!is.null(dash_nt_valores$indice)) {
      valueBox(
        value = formatAsCurrency(dash_nt_valores$indice$VALOR_MES),
        subtitle = "Valor total a mes", 
        icon = icon("dollar-sign", lib = "font-awesome"),
        color = "green"
      )
    }
  })
  
  output$dash_nt_poblacion <- renderValueBox({
    if(!is.null(dash_nt_valores$indice)) {
      valueBox(
        value = format(
          dash_nt_valores$indice$POBLACION,
          scientific = F,
          big.mark = ".", 
          decimal.mark = ","),
        subtitle = "Pobalción", 
        icon = icon("users", lib = "font-awesome"),
        color = "blue"
      )
    }
  })
  
  output$dash_nt_departamento <- renderValueBox({
    if(!is.null(dash_nt_valores$indice)) {
      valueBox(
        value = dash_nt_valores$indice$DEPARTAMENTO,
        subtitle = dash_nt_valores$indice$CIUDADES, 
        icon = icon("city", lib = "font-awesome"),
        color = "aqua"
      )
    }
  })
  
  output$dash_nt_inclusiones <- DT::renderDataTable({
    if(!is.null(dash_nt_valores$inclusiones)) {
      datatable(
        dash_nt_valores$inclusiones[INCLUIDO == 1, c("OBJETO", "NOTAS")],
        rownames = F,
        selection = 'none',
        colnames = c("Observación", "Notas"),
        options = list(
          dom='ft',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = nrow(paquetes),
          ordering = FALSE, 
          scrollX = TRUE,
          scrollY = "60vh")) %>%
        DT::formatStyle(
          columns = 1:4,
          valueColumns = 1, 
          backgroundColor = "white")
    }
  })
  
  output$dash_nt_exclusiones <- DT::renderDataTable({
    if(!is.null(dash_nt_valores$inclusiones)) {
      datatable(
        dash_nt_valores$inclusiones[INCLUIDO == 0, c("OBJETO", "NOTAS")],
        rownames = F,
        selection = 'none',
        colnames = c("Observación", "Notas"),
        options = list(
          dom='ft',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = nrow(paquetes),
          ordering = FALSE, 
          scrollX = TRUE,
          scrollY = "60vh")) %>%
        DT::formatStyle(
          columns = 1:4,
          valueColumns = 1, 
          backgroundColor = "white")
    }
  })
  
  output$dash_nt_plot_agrupadores <- renderGirafe({
    pie_chart(
      paquetes = dash_nt_valores$datos,
      columna = "AGRUPADOR",
      valor_costo = "VALOR_MES")
  })
  
  output$dash_nt_board_datos <- DT::renderDataTable({
    if(!is.null(dash_nt_valores$datos)) {
      datatable(
        dash_nt_valores$datos[, c("AGRUPADOR", 
                                  "FREC_MES", 
                                  "CME", 
                                  "VALOR_MES")],
        rownames = F, 
        selection = 'none',
        colnames = c("Agrupador", "Frecuencia a mes", "CME", "Valor a mes"),
        options = list(
          dom='ft', 
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = nrow(paquetes),
          ordering = FALSE,
          scrollX = TRUE,
          scrollY = "60vh")) %>%
        DT::formatCurrency(
          columns = c("CME", "VALOR_MES")
          , digits = 0, mark = ".", dec.mark = ","
        ) %>%
        DT::formatStyle(
          columns = 1:4,
          valueColumns = 1,
          backgroundColor = "white")
    }
  })
  
  # Indice NT ------------------------------------------------------------------
  
  output$dash_nt_indice_tabla <- DT::renderDataTable({
    if(!is.null(dash_nt_indice)) {
      datatable(
        dash_nt_indice[, -c("COD_DEPARTAMENTO")],
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
          pageLength = nrow(dash_nt_indice), 
          ordering = FALSE, 
          scrollX = TRUE,
          scrollY = "60vh")) %>%
        DT::formatCurrency(
          columns = c("VALOR_MES")
          , digits = 0, mark = ".", dec.mark = ",") %>%
        DT::formatRound(columns = "POBLACION", mark = ".")
    }
  })
  
  output$dash_nt_indice_mapa <- renderPlotly(
    dash_nt_mapa
  )
  
  # Comparar NT ---------------------------------------------------------------
  
  dash_nt_comparar <- reactiveValues(
    datos = data.table()
  )
  
  observeEvent(input$dash_nt_comparar_exe, {
    if(!is.null(input$file)) {
      if(input$dash_nt_comparar_select != "NA") {
        opciones$dash_nt_comparar_select <- input$dash_nt_comparar_select
        opciones$dash_nt_comparar_agrupador <- input$dash_nt_comparar_agrupador
        tryCatch(
          expr = {
            dash_nt_comparar$datos <- dash_nt_datos[
              COD_NT == input$dash_nt_comparar_select]
            
            dash_nt_comparar$desc_sumas <- descriptiva_basica_trans(
              data = descriptiva_basica(
                data = datos$original,
                agrupador = opciones$dash_nt_comparar_agrupador,
                columna_valor = opciones$valor_costo,
                prestaciones = opciones$analisis_prestacion, 
                columna_fecha = "FECHA_PRESTACION"),
              agrupador = opciones$dash_nt_comparar_agrupador, 
              frec = FALSE
            )
            dash_nt_comparar$desc_frecs <- descriptiva_basica_trans(
              data = descriptiva_basica(
                data = datos$original,
                agrupador = opciones$dash_nt_comparar_agrupador,
                columna_valor = opciones$valor_costo,
                prestaciones = opciones$analisis_prestacion, 
                columna_fecha = "FECHA_PRESTACION"), 
              agrupador = opciones$dash_nt_comparar_agrupador, 
              suma = FALSE
            )
            
            dash_nt_comparar$diferencias_rips_sumas <- diferencia_valor_rips(
              sumas = dash_nt_comparar$desc_sumas,
              nota_tecnica = dash_nt_comparar$datos,
              porcentaje = FALSE)
            
            dash_nt_comparar$diferencias_rips_percent <- diferencia_valor_rips(
              sumas = dash_nt_comparar$desc_sumas, 
              nota_tecnica = dash_nt_comparar$datos, 
              porcentaje = TRUE)
            
            dash_nt_comparar$diferencias_cme_sumas <- diferencia_valor_cme(
              frecs = dash_nt_comparar$desc_frecs,
              nota_tecnica = dash_nt_comparar$datos,
              porcentaje = FALSE)
            
            dash_nt_comparar$diferencias_cme_percent <- diferencia_valor_cme(
              frecs = dash_nt_comparar$desc_frecs, 
              nota_tecnica = dash_nt_comparar$datos,
              porcentaje = TRUE)
            
            dash_nt_comparar$totales <- diferencias_totales(
              frecs = dash_nt_comparar$desc_frecs, 
              sumas = dash_nt_comparar$desc_sumas, 
              nota_tecnica = dash_nt_comparar$datos)
          },
          error = function(e) {
            print(e[1])
            sendSweetAlert(
              session = session,
              title = "Error",
              text = e,
              type = "error"
            )
          }
        )
        
        
      }
    }
  })
  
  output$dash_nt_comparar_totales <- DT::renderDataTable({
    if (!is.null(dash_nt_comparar$totales)) {
      DT::datatable(
        dash_nt_comparar$totales[["totales"]], 
        class = 'cell-border stripe', 
        rownames = FALSE, 
        extensions = 'ColReorder',
        selection = 'none',
        options = list(
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 15, 
          autoWidth = FALSE,
          ordering=F,
          scrollX = TRUE,
          colReorder = TRUE,
          dom = 't')) %>%
        DT::formatStyle(
          columns = 3,
          backgroundColor = styleInterval(
            cuts = c(0),
            values = c("rgb(255, 145, 145)",
                       "rgb(145, 255, 145)"))) %>%
        DT::formatStyle(
          columns = 4,
          backgroundColor = styleInterval(
            cuts = c(1), 
            values = c("rgb(255, 145, 145)",
                       "rgb(145, 255, 145)"))) %>%
        formatCurrency(
          columns = c(2,3),
          mark = ".", 
          dec.mark = ",", 
          digits = 0) %>%
        formatPercentage(
          columns = 4, 
          digits = 0, 
          mark = ".", 
          dec.mark = ",")
    }
  })
  
  output$dash_nt_comparar_total_mes_rips <- DT::renderDataTable({
    if (!is.null(dash_nt_comparar$totales)) {
      DT::datatable(
        dash_nt_comparar$totales[["total_mes_rips"]], 
        class = 'cell-border stripe', 
        rownames = FALSE, 
        extensions = 'ColReorder',
        selection = 'none',
        options = list(
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 15,
          autoWidth = FALSE, 
          ordering=F, 
          scrollX = TRUE, 
          colReorder = TRUE,
          dom = 't',
          scrollY = "50vh", 
          scrollCollapse = TRUE)) %>%
        DT::formatStyle(
          columns = 3, 
          backgroundColor = styleInterval(
            cuts = c(0), 
            values = c("rgb(255, 145, 145)",
                       "rgb(145, 255, 145)"))) %>%
        DT::formatStyle(
          columns = 4, 
          backgroundColor = styleInterval(
          cuts = c(1), 
          values = c("rgb(255, 145, 145)",
                     "rgb(145, 255, 145)"))) %>%
        formatCurrency(
          columns = c(2,3), 
          mark = ".",
          dec.mark = ",",
          digits = 0) %>%
        formatPercentage(
          columns = 4, 
          digits = 0,
          mark = ".",
          dec.mark = ",")
    }
  })
  
  output$dash_nt_comparar_total_agrup_rips <- DT::renderDataTable({
    if (!is.null(dash_nt_comparar$totales)) {
      DT::datatable(
        dash_nt_comparar$totales[["total_agrupador_rips"]], 
        class = 'cell-border stripe', 
        rownames = FALSE, 
        extensions = 'ColReorder',
        selection = 'none',
        options = list(
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 15,
          autoWidth = FALSE,
          ordering=F,
          scrollX = TRUE,
          colReorder = TRUE,
          dom = 't', 
          scrollY = "50vh", 
          scrollCollapse = TRUE)) %>%
        DT::formatStyle(
          columns = 3, 
          backgroundColor = styleInterval(
          cuts = c(0),
          values = c("rgb(255, 145, 145)",
                     "rgb(145, 255, 145)"))) %>%
        DT::formatStyle(
          columns = 4, 
          backgroundColor = styleInterval(
            cuts = c(1), 
            values = c("rgb(255, 145, 145)",
                       "rgb(145, 255, 145)"))) %>%
        formatCurrency(
          columns = c(2,3),
          mark = ".", 
          dec.mark = ",",
          digits = 0) %>%
        formatPercentage(
          columns = 4,
          digits = 0, 
          mark = ".", 
          dec.mark = ",")
    }
  })
  
  output$dash_nt_comparar_total_mes_cme <- DT::renderDataTable({
    if (!is.null(dash_nt_comparar$totales)) {
      DT::datatable(
        dash_nt_comparar$totales[["total_mes_cme"]], 
        class = 'cell-border stripe', 
        rownames = FALSE, 
        selection = 'none',
        extensions = 'ColReorder',
        options = list(
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 15, 
          autoWidth = FALSE, 
          ordering=F,
          scrollX = TRUE, 
          colReorder = TRUE,
          dom = 't',
          scrollY = "50vh",
          scrollCollapse = TRUE)) %>%
        DT::formatStyle(
          columns = 3,
          backgroundColor = styleInterval(
            cuts = c(0), 
            values = c("rgb(255, 145, 145)",
                       "rgb(145, 255, 145)"))) %>%
        DT::formatStyle(
          columns = 4,
          backgroundColor = styleInterval(
            cuts = c(1), 
            values = c("rgb(255, 145, 145)",
                       "rgb(145, 255, 145)"))) %>%
        formatCurrency(
          columns = c(2,3),
          mark = ".", 
          dec.mark = ",",
          digits = 0) %>%
        formatPercentage(
          columns = 4, 
          digits = 0,
          mark = ".", 
          dec.mark = ",")
    }
  })
  
  output$dash_nt_comparar_total_agrup_cme <- DT::renderDataTable({
    if (!is.null(dash_nt_comparar$totales)) {
      DT::datatable(
        dash_nt_comparar$totales[["total_agrupador_cme"]], 
        class = 'cell-border stripe', 
        rownames = FALSE, 
        extensions = 'ColReorder',
        selection = 'none',
        options = list(
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 15,
          autoWidth = FALSE, 
          ordering=F, 
          scrollX = TRUE,
          colReorder = TRUE,
          dom = 't', 
          scrollY = "50vh",
          scrollCollapse = TRUE)) %>%
        DT::formatStyle(
          columns = 3, 
          backgroundColor = styleInterval(
            cuts = c(0), 
            values = c("rgb(255, 145, 145)",
                       "rgb(145, 255, 145)"))) %>%
        DT::formatStyle(
          columns = 4, 
          backgroundColor = styleInterval(
            cuts = c(1),
            values = c("rgb(255, 145, 145)",
                       "rgb(145, 255, 145)"))) %>%
        formatCurrency(
          columns = c(2,3),
          mark = ".", 
          dec.mark = ",",
          digits = 0) %>%
        formatPercentage(
          columns = 4, 
          digits = 0, 
          mark = ".", 
          dec.mark = ",")
    }
  })
  
  output$dash_nt_comparar_desc_sumas <- DT::renderDataTable({
    if (!is.null(dash_nt_comparar$desc_sumas)) {
      DT::datatable(
        dash_nt_comparar$desc_sumas, 
        class = 'cell-border stripe', 
        rownames = FALSE, 
        extensions = 'ColReorder',
        selection = 'none',
        options = list(
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 15, 
          autoWidth = FALSE,
          ordering=T, 
          scrollX = TRUE, 
          colReorder = TRUE,
          scrollY = "50vh", 
          scrollCollapse = TRUE)) %>%
        formatCurrency(
          columns = -c(1), 
          mark = ".",
          dec.mark = ",",
          digits = 0)
    }
  })
  
  output$dash_nt_comparar_desc_frecs <- DT::renderDataTable({
    if (!is.null(dash_nt_comparar$desc_frecs)) {
      DT::datatable(
        dash_nt_comparar$desc_frecs, 
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
          colReorder = TRUE,
          scrollY = "50vh", 
          scrollCollapse = TRUE)) %>%
        DT::formatRound(
          columns = -c(1),
          mark = ".",
          dec.mark = ",",
          digits = 0)
    }
  })
  
  output$dash_nt_comparar_diferencias_rips_sumas <- DT::renderDataTable({
    if (!is.null(dash_nt_comparar$diferencias_rips_sumas)) {
      DT::datatable(
        dash_nt_comparar$diferencias_rips_sumas, 
        class = 'cell-border stripe', 
        rownames = FALSE, 
        extensions = 'ColReorder',
        selection = 'none',
        options = list(
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 15,
          autoWidth = FALSE,
          ordering=T,
          scrollX = TRUE,
          colReorder = TRUE,
          scrollY = "50vh",
          scrollCollapse = TRUE)) %>%
        DT::formatStyle(
          columns = -c(1),
          backgroundColor = styleInterval(
            cuts = c(0),
            values = c("rgb(255, 145, 145)",
                       "rgb(145, 255, 145)"))) %>%
        DT::formatCurrency(
          columns = -c(1),
          mark = ".",
          dec.mark = ",",
          digits = 0)
    }
  })
  
  output$dash_nt_comparar_diferencias_rips_percent <- DT::renderDataTable({
    if (!is.null(dash_nt_comparar$diferencias_rips_percent)) {
      DT::datatable(
        dash_nt_comparar$diferencias_rips_percent, 
        class = 'cell-border stripe', 
        rownames = FALSE, 
        extensions = 'ColReorder',
        selection = 'none',
        options = list(
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 15,
          autoWidth = FALSE,
          ordering=T, 
          scrollX = TRUE, 
          colReorder = TRUE,
          scrollY = "50vh", 
          scrollCollapse = TRUE)) %>%
        DT::formatStyle(
          columns = -c(1),
          backgroundColor = styleInterval(
          cuts = c(1),
          values = c("rgb(255, 145, 145)",
                     "rgb(145, 255, 145)"))) %>%
        DT::formatPercentage(
          columns = -c(1),
          mark = ".",
          dec.mark = ",",
          digits = 0)
    }
  })
  
  output$dash_nt_comparar_diferencias_cme_sumas <- DT::renderDataTable({
    if (!is.null(dash_nt_comparar$diferencias_cme_sumas)) {
      DT::datatable(
        dash_nt_comparar$diferencias_cme_sumas, 
        class = 'cell-border stripe', 
        rownames = FALSE, 
        extensions = 'ColReorder',
        selection = 'none',
        options = list(
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 15,
          autoWidth = FALSE,
          ordering=T, 
          scrollX = TRUE, 
          colReorder = TRUE,
          scrollY = "50vh", 
          scrollCollapse = TRUE)) %>%
        DT::formatStyle(
          columns = -c(1),
          backgroundColor = styleInterval(
            cuts = c(0),
            values = c("rgb(255, 145, 145)",
                       "rgb(145, 255, 145)"))) %>%
        DT::formatCurrency(
          columns = -c(1),
          mark = ".",
          dec.mark = ",", 
          digits = 0)
    }
  })
  
  output$dash_nt_comparar_diferencias_cme_percent <- DT::renderDataTable({
    if (!is.null(dash_nt_comparar$diferencias_cme_percent)) {
      DT::datatable(
        dash_nt_comparar$diferencias_cme_percent, 
        class = 'cell-border stripe', 
        rownames = FALSE, 
        extensions = 'ColReorder',
        selection = 'none',
        options = list(
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 15,
          autoWidth = FALSE,
          ordering=T, 
          scrollX = TRUE, 
          colReorder = TRUE,
          scrollY = "50vh",
          scrollCollapse = TRUE)) %>%
        DT::formatStyle(
          columns = -c(1),
          backgroundColor = styleInterval(
            cuts = c(1), 
            values = c("rgb(255, 145, 145)",
                       "rgb(145, 255, 145)"))) %>%
        DT::formatPercentage(
          columns = -c(1),
          mark = ".", 
          dec.mark = ",",
          digits = 0)
    }
  })
  
})