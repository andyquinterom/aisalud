#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(feather)
library(data.table)
library(shinyWidgets)
library(writexl)
library(ggplot2)
library(varhandle)
library(Cairo)
library(lubridate)
library(shinydashboard)
library(ggiraph)
library(scales)
library(readr)
library(plotly)
library(googlesheets4)



source("source/descriptiva.R")
source("source/outliers.R")
source("source/dependencies.R")
source("source/agregaciones.R")
source("source/notatecnica.R")
source("source/formatAsCurrency.R")
source("source/REFgrafico.R")
source("source/REFPgrafico.R")
source("source/PIEchart.R")
source("source/resumenComp.R")



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    session$onSessionEnded(function() {
        stopApp()
    })
    
    ###Bienvenido

    
    ### Sección descriptiva
    
    opciones = reactiveValues(descriptivaColumna = NULL, valorCosto = "VALOR")
    bd = reactiveValues(descriptiva = data.frame(), original = data.frame())
    uniques = reactiveValues()
    minsmaxs = reactiveValues()
    filtro = reactiveValues(bd1 = "NA", bd2 = "NA", bd3 = "NA", bd4 = "NA")
    
    if ("PAQUETES.feather" %in% dir(path = "PAQUETES" ,all.files=TRUE) && "REFERENTE-PAQUETES.feather" %in% dir(path = "PAQUETES" ,all.files=TRUE) && "REFERENTE.feather" %in% dir(path = "PAQUETES" ,all.files=TRUE)) {
        PAQUETES = as.data.table(read_feather("PAQUETES/PAQUETES.feather"))
        REF_PAQUETES = as.data.table(read_feather("PAQUETES/REFERENTE-PAQUETES.feather"))
        REF = as.data.table(read_feather("PAQUETES/REFERENTE.feather"))
        PAQUETE_PP = PAQUETES[`COMPONENTE` == "PAQUETE"]
        PAQUETES_CC = PAQUETES[`COMPONENTE` != "PAQUETE"]
    }
    
    output$preview = DT::renderDataTable({
        if(is.null(input$file)) 
            data.table()
        else {
            tryCatch( expr = {    
                       Preview = DT::datatable(head({
                               read_feather(input$file$datapath, 
                                            columns = c("NRO_IDENTIFICACION", "FECHA_PRESTACION", input$valorCosto))
                       }
                                  , n = 5)
                                  , rownames = FALSE
                                  , options = list(
                                      dom = 't'
                                      , pageLength = 5
                                      , ordering = FALSE
                                      , columnDefs = list(list(className = 'dt-center', targets = 0:2))
                                      , language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                                  )) %>%
                        DT::formatStyle(columns = 1:3, valueColumns = 1, backgroundColor = "white")
                        opciones$valorCosto = input$valorCosto
                        showNotification(paste0("Análisis cambiado a ", tolower(opciones$valorCosto)), duration = 5, type = "message", session = session)
                        Preview
            },
            error = function(e) {
                print(e[1])
                if (e[1] == "Invalid: Not a feather file") {
                    updateRadioButtons(session = session
                                       , inputId = "fileType"
                                       , selected = "RDS")
                }
                else {
                    updateRadioButtons(session = session
                                       , inputId = "valorCosto"
                                       , selected = opciones$valorCosto)
                }
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
    
    # observeEvent(input$exeBoomark, {
    #     session$doBookmark()
    # })
    
    observeEvent(input$pacientesPrestaciones, {
        if (input$pacientesPrestaciones == "FALSE") {
            opciones$pacientesPrestaciones = FALSE
            showNotification(paste("Análisis cambiado a paciente"), duration = 5, type = "message", session = session)
            }
        else {
            opciones$pacientesPrestaciones = TRUE
            showNotification(paste("Análisis cambiado a prestación"), duration = 5, type = "message", session = session)
        }
    })
    
    
    observeEvent(input$exeOpciones, {
        if(!is.null(input$file)) {
            withProgress(message = "Cargando base de datos",{
                print("Inicio leer")
                    bd$original = as.data.table(feather::read_feather(input$file$datapath))
                print("Fin leer")
                    incProgress(0.5)
                opciones$fechamin = as.Date(input$fechamin, "%d/%m/%Y")
                opciones$fechamax = as.Date(input$fechamax, "%d/%m/%Y")
                opciones$meses = as.numeric(as.character(interval(opciones$fechamin, opciones$fechamax) %/% months(1)))
                opciones$formatoFecha = input$formatoFecha
                    incProgress(0.1)
                bd$original$FECHA_PRESTACION = as.Date(bd$original$FECHA_PRESTACION, opciones$formatoFecha)
                bd$original = bd$original[FECHA_PRESTACION >= opciones$fechamin & FECHA_PRESTACION <= opciones$fechamax]
                bd$original[["NRO_IDENTIFICACION"]] = as.character(bd$original[["NRO_IDENTIFICACION"]])
                COLNUM = unlist(lapply(bd$original[1,], is.numeric))
                bd$Colnames = colnames(bd$original)
                        incProgress(0.1)
                    updatePickerInput(session, inputId = "descriptivaColumna", choices = bd$Colnames, selected = opciones$descriptivaColumna)
                    updatePickerInput(session, inputId = "outliersColumna", choices = bd$Colnames, selected = opciones$outliersColumna)
                    updatePickerInput(session, inputId = "bigotesColumna", choices = c("NA", bd$Colnames[!COLNUM]), selected = opciones$bigotesColumna)
                    updatePickerInput(session, inputId = "histogramaRelleno", choices = c("NA",bd$Colnames), selected = opciones$histogramaRelleno)
                    updatePickerInput(session, inputId = "histogramaColumna", choices = bd$Colnames, selected = opciones$histogramaColumna)
                    updatePickerInput(session, inputId = "agregacionesColumnas", choices = bd$Colnames, selected = opciones$agregacionesColumnas)
                    for (i in c(1:5)) {
                        updatePickerInput(session, paste("bdFiltro", i, sep = ""), choices = c("NA", bd$Colnames[!COLNUM]))
                    }
                    for (i in c(1:2)) {
                        updatePickerInput(session, paste("bdFiltroLong", i, sep = ""), choices = c("NA", bd$Colnames[!COLNUM]))
                    }
                    for (i in c(1:2)) {
                        updatePickerInput(session, paste("bdFiltroNum", i, sep = ""), choices = c("NA", bd$Colnames[COLNUM]))
                    }
                    for (i in bd$Colnames[!COLNUM]) {
                        uniques[[i]] = unique(bd$original[[i]])
                    }
                    for (i in bd$Colnames[COLNUM]) {
                        minsmaxs[[i]] = c(min(bd$original[[i]], na.rm = TRUE), max(bd$original[[i]], na.rm = TRUE))
                    }
                    sendSweetAlert(
                        session = session,
                        title = "Exito",
                        text = "Las opciones han sido ejecutadas",
                        type = "success"
                    )
            })} 
    })
    
    ################### Inicio cargar filtros
    
    observeEvent(input$bdFiltro1, {
        filtro$bd1 = input$bdFiltro1
        if (filtro$bd1 != "NA" && !is.null(input$bdFiltro1)) {
            updatePickerInput(session, "bdFiltroVAL1", choices = uniques[[filtro$bd1]]
                              , choicesOpt = list(content = stringr::str_replace_all(str_wrap(uniques[[filtro$bd1]], width = 50), "\\n", "<br>")))
        }
        else if (filtro$bd1 == "NA") {
            updatePickerInput(session, "bdFiltroVAL1", choices = "NA", selected = "NA")
        }
    })
    
    observeEvent(input$bdFiltroVAL1, {
        filtro$bdVAL1 = input$bdFiltroVAL1
    }) 
    
    observeEvent(input$bdFiltro2, {
        filtro$bd2 = input$bdFiltro2
        if (filtro$bd2 != "NA" && !is.null(input$bdFiltro2)) {
            updatePickerInput(session, "bdFiltroVAL2", choices = uniques[[filtro$bd2]]
                              , choicesOpt = list(content = stringr::str_replace_all(str_wrap(uniques[[filtro$bd2]], width = 50), "\\n", "<br>")))
        }
        else if (filtro$bd2 == "NA") {
            updatePickerInput(session, "bdFiltroVAL2", choices = "NA", selected = "NA")
        }
    })
    
    observeEvent(input$bdFiltroVAL2, {
        filtro$bdVAL2 = input$bdFiltroVAL2
    }) 
    
    observeEvent(input$bdFiltro3, {
        filtro$bd3 = input$bdFiltro3
        if (filtro$bd3 != "NA" && !is.null(input$bdFiltro3)) {
            updatePickerInput(session, "bdFiltroVAL3", choices = uniques[[filtro$bd3]]
                              , choicesOpt = list(content = stringr::str_replace_all(str_wrap(uniques[[filtro$bd3]], width = 50), "\\n", "<br>")))
        }
        else if (filtro$bd3 == "NA") {
            updatePickerInput(session, "bdFiltroVAL3", choices = "NA", selected = "NA")
        }
    })
    
    observeEvent(input$bdFiltroVAL3, {
        filtro$bdVAL3 = input$bdFiltroVAL3
    }) 
    
    observeEvent(input$bdFiltro4, {
        filtro$bd4 = input$bdFiltro4
        if (filtro$bd4 != "NA" && !is.null(input$bdFiltro4)) {
            updatePickerInput(session, "bdFiltroVAL4", choices = uniques[[filtro$bd4]]
                              , choicesOpt = list(content = stringr::str_replace_all(str_wrap(uniques[[filtro$bd4]], width = 50), "\\n", "<br>")))
        }
        else if (filtro$bd4 == "NA") {
            updatePickerInput(session, "bdFiltroVAL4", choices = "NA", selected = "NA")
        }
    })
    
    observeEvent(input$bdFiltroVAL4, {
        filtro$bdVAL4 = input$bdFiltroVAL4
    }) 
    
    observeEvent(input$bdFiltro5, {
        filtro$bd5 = input$bdFiltro5
        if (filtro$bd5 != "NA" && !is.null(input$bdFiltro5)) {
            updatePickerInput(session, "bdFiltroVAL5", choices = uniques[[filtro$bd5]]
                              , choicesOpt = list(content = stringr::str_replace_all(str_wrap(uniques[[filtro$bd5]], width = 50), "\\n", "<br>")))
        }
        else if (filtro$bd5 == "NA") {
            updatePickerInput(session, "bdFiltroVAL5", choices = "NA", selected = "NA")
        }
    })
    
    observeEvent(input$bdFiltroVAL5, {
        filtro$bdVAL5 = input$bdFiltroVAL5
    })
    
    # Numericos
    
    observeEvent(input$bdFiltroNum1, {
        filtro$bdNum1 = input$bdFiltroNum1
        if (filtro$bdNum1 != "NA" && !is.null(filtro$bdNum1)) {
            updateNumericInput(session, "bdFiltroNum1VAL1", value = minsmaxs[[filtro$bdNum1]][1])
            updateNumericInput(session, "bdFiltroNum1VAL2", value = minsmaxs[[filtro$bdNum1]][2])
        }
        else if (filtro$bdNum1 == "NA") {
            updateNumericInput(session, "bdFiltroNum1VAL1", value = 0)
            updateNumericInput(session, "bdFiltroNum1VAL2", value = 0)
        }
    })
    
    observeEvent(input$bdFiltroNum1VAL1, {
        if (is.na(input$bdFiltroNum1VAL1)) {
            updateNumericInput(session, "bdFiltroNum1VAL1", value = minsmaxs[[filtro$bdNum1]][1])
        }
    }, ignoreNULL = FALSE)
    
    observeEvent(input$bdFiltroNum1VAL2, {
        if (is.na(input$bdFiltroNum1VAL2)) {
            updateNumericInput(session, "bdFiltroNum1VAL2", value = minsmaxs[[filtro$bdNum1]][2])
        }
    })
    
    observeEvent(input$bdFiltroNum2, {
        filtro$bdNum2 = input$bdFiltroNum2
        if (filtro$bdNum2 != "NA" && !is.null(filtro$bdNum2)) {
            updateNumericInput(session, "bdFiltroNum2VAL1", value = minsmaxs[[filtro$bdNum2]][1])
            updateNumericInput(session, "bdFiltroNum2VAL2", value = minsmaxs[[filtro$bdNum2]][2])
        }
        else if (filtro$bdNum2 == "NA") {
            updateNumericInput(session, "bdFiltroNum2VAL1", value = 0)
            updateNumericInput(session, "bdFiltroNum2VAL2", value = 0)
        }
    })
    
    observeEvent(input$bdFiltroNum2VAL1, {
        if (is.na(input$bdFiltroNum2VAL1)) {
            updateNumericInput(session, "bdFiltroNum2VAL1", value = minsmaxs[[filtro$bdNum2]][1])
        }
    }, ignoreNULL = FALSE)
    
    observeEvent(input$bdFiltroNum2VAL2, {
        if (is.na(input$bdFiltroNum2VAL2)) {
            updateNumericInput(session, "bdFiltroNum2VAL2", value = minsmaxs[[filtro$bdNum2]][2])
        }
    })
    
    ################### Fin cargar filtros
    
    observeEvent(input$aplicarFiltros, {
        if(!is.null(input$file)) {
            withProgress(message = "Aplicando filtros",{
                bd$original = as.data.table(feather::read_feather(input$file$datapath))
                bd$original$FECHA_PRESTACION = as.Date(bd$original$FECHA_PRESTACION, opciones$formatoFecha)
                bd$original = bd$original[FECHA_PRESTACION >= opciones$fechamin & FECHA_PRESTACION <= opciones$fechamax]
                bd$original[["NRO_IDENTIFICACION"]] = as.character(bd$original[["NRO_IDENTIFICACION"]])
                incProgress(0.3)
                if (input$bdFiltro1 != "NA")
                    bd$original = bd$original[get(input$bdFiltro1) %in% input$bdFiltroVAL1]
                if (input$bdFiltro2 != "NA")
                    bd$original = bd$original[get(input$bdFiltro2) %in% input$bdFiltroVAL2]
                if (input$bdFiltro3 != "NA")
                    bd$original = bd$original[get(input$bdFiltro3) %in% input$bdFiltroVAL3]
                if (input$bdFiltro4 != "NA")
                    bd$original = bd$original[get(input$bdFiltro4) %in% input$bdFiltroVAL4]
                if (input$bdFiltro5 != "NA")
                    bd$original = bd$original[get(input$bdFiltro5) %in% input$bdFiltroVAL5]
                
                if (input$bdFiltroLong1 != "NA" && !input$bdFiltroLongEx1)
                    bd$original = bd$original[get(input$bdFiltroLong1) %in% str_split(input$bdFiltroLongVAL1, pattern = ";")[[1]]]
                if (input$bdFiltroLong2 != "NA" && !input$bdFiltroLongEx2)
                    bd$original = bd$original[get(input$bdFiltroLong2) %in% str_split(input$bdFiltroLongVAL2, pattern = ";")[[1]]]
                if (input$bdFiltroLong1 != "NA" && input$bdFiltroLongEx1)
                    bd$original = bd$original[get(input$bdFiltroLong1) %notin% str_split(input$bdFiltroLongVAL1, pattern = ";")[[1]]]
                if (input$bdFiltroLong2 != "NA" && input$bdFiltroLongEx2)
                    bd$original = bd$original[get(input$bdFiltroLong2) %notin% str_split(input$bdFiltroLongVAL2, pattern = ";")[[1]]]
                
                if (input$bdFiltroNum1 != "NA")
                    bd$original = bd$original[get(input$bdFiltroNum1) >= input$bdFiltroNum1VAL1 & get(input$bdFiltroNum1) <= input$bdFiltroNum1VAL2]
                if (input$bdFiltroNum2 != "NA")
                    bd$original = bd$original[get(input$bdFiltroNum2) >= input$bdFiltroNum2VAL1 & get(input$bdFiltroNum2) <= input$bdFiltroNum2VAL2]
            })
            showNotification(paste0("Filtros aplicados"), duration = 5, type = "message", session = session)
        }
    })
    
    observeEvent(input$exeOpcionesDescriptiva, {
        if(!is.null(input$file)) {
            if(!is.null(input$descriptivaColumna) && input$descriptivaColumna != "NA") {
                opciones$descriptivaColumna = input$descriptivaColumna
                withProgress(message = "Calculando descriptiva",{
                    bd$descriptiva = descriptiva(bd$original, opciones$descriptivaColumna, opciones$valorCosto, opciones$pacientesPrestaciones)
                
                    output$descriptivaFinal = DT::renderDataTable({
                        DT::datatable({
                            if (isTRUE(input$filtroNULL))
                                bd$descriptiva
                            else
                                bd$descriptiva[Coef.var >= input$filtroCoeficiente[1] & Coef.var <= input$filtroCoeficiente[2]]
                        },
                                      options = list(
                                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                          pageLength = 50, autoWidth = FALSE, ordering=T, scrollX = TRUE, scrollY = "60vh"),
                                      rownames= FALSE
                        ) %>%
                            formatCurrency(c('P50','P75','P90','Media','Media truncada 10%','Media truncada 5%','Desv.tipica')) %>%
                            formatCurrency(c('Suma','Min.','Max.','Rango'), digits=0)
                    })   
                })
            }
        }
    })
    
    observeEvent(input$exeOpcionesOutliersPercentil, {
        if(!is.null(input$file)) {
            if(!is.null(input$outliersColumna) && input$outliersColumna != "NA") {
                opciones$outliersColumna = input$outliersColumna
                    bd$outliers = outlierspercentil(bd$original, opciones$outliersColumna, opciones$valorCosto, percentil = input$outliersPercentil, input$outliersFiltroFrecuencia, session = session)
                output$outliersFinal = DT::renderDataTable({
                    DT::datatable(bd$outliers,
                    options = list(
                        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                        pageLength = 15, autoWidth = FALSE, ordering=T, scrollX = TRUE),
                    rownames= FALSE
                    ) %>%
                        formatCurrency(c('VALOR'))
                })   
            }
        }
    })
    
    observeEvent(input$exeOpcionesOutliersIQR, {
        if(!is.null(input$file)) {
            if(!is.null(input$outliersColumna) && input$outliersColumna != "NA") {
                opciones$outliersColumna = input$outliersColumna
                bd$outliers = outliersIQR(bd$original, opciones$outliersColumna, opciones$valorCosto, multiplicativo = numerize(input$outliersIQR), frecuencia = input$outliersFiltroFrecuencia,session = session)
                output$outliersFinal = DT::renderDataTable({
                    DT::datatable(bd$outliers,
                                  options = list(
                                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                      pageLength = 15, autoWidth = FALSE, ordering=T, scrollX = TRUE),
                                  rownames= FALSE
                    ) %>%
                        formatCurrency(c('VALOR'))
                })   
            }
        }
    })
    
    observeEvent(input$exeNT, {
        if(!is.null(input$file)) {
            if (nrow(bd$descriptiva >= 1)) {
                bd$notatecnica = crear_notatecnica(x = bd$descriptiva, 
                                                   columnas = opciones$descriptivaColumna, 
                                                   meses = input$NTmeses,
                                                   poblacion = input$NTpoblacion)
                                                   bd$notatecnica$'Frecuencia a mes' = numerize(bd$notatecnica$'Frecuencia a mes')
                output$NTtabla = DT::renderDataTable(
                    DT::datatable(bd$notatecnica, 
                                  class = 'cell-border stripe', 
                                  rownames = FALSE, 
                                  extensions = 'ColReorder',
                                  options = list(
                                      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                      pageLength = 15, autoWidth = FALSE, ordering=T, scrollX = TRUE, colReorder = TRUE)) %>%
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
                                                        'a mes', sep = " "),
                                         'Valor episodio',
                                         'CME'))
                    )
            }
        }
    })
    observeEvent(input$exeNTremoveRow, {
        if(!is.null(input$file)) {
            if(!is.null(bd$notatecnica)) {
                bd$notatecnica = bd$notatecnica[-as.numeric(input$NTtabla_rows_selected),]
            }
        }
    })
    
    observeEvent(input$exeOpcionesHistograma, {
        if(!is.null(input$file)) {
            if(!is.null(input$histogramaColumna) && input$histogramaColumna != "NA") {
                withProgress(message = "Graficando", min = 0,{
                    totalcolumna = all(check.numeric(unlist(extractCol(bd$original, input$histogramaColumna))))
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
                        if (opciones$histogramaColumna == opciones$valorCosto)
                            bdPacientes = agregar(bd$original, columna_valor = opciones$valorCosto, columnas = c('NRO_IDENTIFICACION', opciones$histogramaRelleno), prestaciones = TRUE)
                        else
                            bdPacientes = agregar(bd$original, columna_valor = opciones$valorCosto, columnas = c('NRO_IDENTIFICACION', opciones$histogramaRelleno, opciones$histogramaColumna), prestaciones = TRUE)
                            
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
                            bdPrestaciones = bd$original[get(opciones$histogramaColumna) >= opciones$histogramaFiltroX[1] & get(opciones$histogramaColumna) <= opciones$histogramaFiltroX[2]]
                        else
                            bdPrestaciones = bd$original
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
                choices = unique(bd$original[,get(input$bigotesColumna)])
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
                            bdPacientes = agregar(bd$original[get(input$bigotesColumna) %in% input$bigotesSeleccionar], columna_valor = opciones$valorCosto, columnas = c('NRO_IDENTIFICACION', opciones$bigotesColumna), prestaciones = TRUE)
                            bigotes = ggplot(bdPacientes, 
                                             aes(x=get(opciones$bigotesColumna), 
                                                 y=get(opciones$valorCosto), 
                                                 group = get(opciones$bigotesColumna)
                                                 )) +
                                      geom_boxplot() +
                                      scale_y_continuous(labels = scales::comma, name = opciones$valorCosto) +
                                      coord_cartesian(ylim = c(opciones$bigotesFiltroY1, opciones$bigotesFiltroY2)) +
                                      xlab(opciones$bigotesColumna) +
                                      ylab(opciones$valorCosto) +
                                      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12))
                            bdPacientes = NULL
                        }
                        else {
                            bdPacientes = NULL
                            bigotes = ggplot(bd$original[get(input$bigotesColumna) %in% input$bigotesSeleccionar], 
                                             aes(x=get(opciones$bigotesColumna), 
                                                 y=get(opciones$valorCosto), 
                                                 group = get(opciones$bigotesColumna)
                                                 )) +
                                      geom_boxplot() +
                                      scale_y_continuous(labels = scales::comma, name = opciones$valorCosto) +
                                      coord_cartesian(ylim = c(opciones$bigotesFiltroY1, opciones$bigotesFiltroY2)) +
                                      xlab(opciones$bigotesColumna) +
                                      ylab(opciones$valorCosto) +
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
        if(dim(bd$original)[1] != 0) {
            bdLinea = agregar(bd$original, columna_valor = opciones$valorCosto, columnas = "FECHA_PRESTACION", prestaciones = opciones$pacientesPrestaciones)
            LineaDeTiempo = ggplot(bdLinea, aes(x = FECHA_PRESTACION, y = get(opciones$valorCosto))) +
                                geom_line(linetype = "dashed")
            bdLinea = NULL
            output$lineadetiempoFinal = renderPlot({
                LineaDeTiempo +
                    geom_point() +
                    xlab("FECHA") +
                    ylab(opciones$valorCosto) +
                    scale_y_continuous(labels = scales::comma) 
            })
        }
    })


    
    output$descargaDescriptivaCSV = downloadHandler(
        filename = function() {
            paste("descriptiva-por",input$valorCosto,".",input$fechamin,"-",input$fechamax,".csv", sep="")
        },
        content = function(file) {
            write.csv(bd$descriptiva, file, row.names = FALSE, na="")
        }, contentType = "text/csv"
    )
    
    output$descargaDescriptivaEXCEL = downloadHandler(
        filename = function() {
            paste("descriptiva-por",input$valorCosto,".",input$fechamin,"-",input$fechamax,".xlsx", sep="")
        },
        content = function(file) {
            write_xlsx(bd$descriptiva, path = file)
        }, contentType = "xlsx"
    )
    
    output$descargaOutliersCSV = downloadHandler(
        filename = function() {
            paste("outliers por",input$valorCosto,"-",opciones$outliersColumna,".csv", sep="")
        },
        content = function(file) {
            write.csv(bd$outliers, file, row.names = FALSE, na="")
        }, contentType = "text/csv"
    )
    
    output$descargaOutliersEXCEL = downloadHandler(
        filename = function() {
            paste("outliers por",input$valorCosto,"-",opciones$outliersColumna,".xlsx", sep="")
        },
        content = function(file) {
            write_xlsx(bd$outliers, path = file)
        }, contentType = "xlsx"
    )
    
    output$descargaAgregacionesCSV = downloadHandler(
        filename = function() {
            paste("agregaciones",".xlsx", sep="")
        },
        content = function(file) {
            write.csv(bd$agregaciones, file, row.names = FALSE, na="")
        }, contentType = "text/csv"
    )
    
    output$descargaAgregacionesEXCEL = downloadHandler(
        filename = function() {
            paste("agregaciones",".xlsx", sep="")
        },
        content = function(file) {
            write_xlsx(bd$agregaciones, path = file)
        }, contentType = "xlsx"
    )
    
    output$descargaNTExcel = downloadHandler(
        filename = function() {
            paste("notatécnica",".xlsx", sep="")
        },
        content = function(file) {
            write_xlsx(bd$notatecnica, path = file)
        }, contentType = "xlsx"
    )
    
    output$sumasDescriptivaRegistros <- renderText({
        if (!is.null(input$file)) {
            paste("Total registros:", 
                  formatC(
                      length(bd$original$NRO_IDENTIFICACION), big.mark = ",", format = "f", digits = 0
                  ),
                  sep = " "
                  )
        }
        })
    output$sumasDescriptivaPacientes <- renderText({
        if (!is.null(input$file)) {
            paste("Total pacientes:", 
                  formatC(
                      length(unique(bd$original$NRO_IDENTIFICACION)), big.mark = ",", format = "f", digits = 0
                  ),
                  sep = " "
            )
        }
    })
    output$sumasDescriptivaValorCosto <- renderText({
        if (!is.null(input$file)) {
            if(nrow(bd$descriptiva) >= 1) {
                paste("Total", paste0(tolower(opciones$valorCosto), ":"), 
                      formatC(
                          sum(bd$descriptiva$Suma, na.rm = TRUE), big.mark = ",", format = "f", digits = 0
                      ),
                      sep = " "
                )
            }
        }
    })
    output$sumasDescriptivaCUPS <- renderText({
        if (!is.null(input$file)) {
            if("CODIGO_CUPS" %in% names(bd$original)) {
                paste("Total prestaciones:", 
                      formatC(
                          length(unique(bd$original$CODIGO_CUPS)), big.mark = ",", format = "f", digits = 0
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
                                sum(bd$notatecnica$'Primer escenario P75', na.rm = TRUE),
                                big.mark = ",",
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
                                 sum(bd$notatecnica$'Primer escenario P75', na.rm = TRUE)/opciones$meses,
                                 big.mark = ",",
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
                              sum(bd$notatecnica$'Primer escenario P75', na.rm = TRUE)/sum(bd$descriptiva$Suma, na.rm = TRUE)*100
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
                                     sum(bd$notatecnica$'Segundo escenario media', na.rm = TRUE),
                                     big.mark = ",",
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
                                     sum(bd$notatecnica$'Segundo escenario media', na.rm = TRUE)/opciones$meses,
                                     big.mark = ",",
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
                                  sum(bd$notatecnica$'Segundo escenario media', na.rm = TRUE)/sum(bd$descriptiva$Suma, na.rm = TRUE)*100
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
                                     sum(bd$notatecnica$'Tercer escenario media truncada 10%', na.rm = TRUE),
                                     big.mark = ",",
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
                                     sum(bd$notatecnica$'Tercer escenario media truncada 10%', na.rm = TRUE)/opciones$meses,
                                     big.mark = ",",
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
                                  sum(bd$notatecnica$'Tercer escenario media truncada 10%', na.rm = TRUE)/sum(bd$descriptiva$Suma, na.rm = TRUE)*100
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
                                     sum(bd$notatecnica$'Cuarto escenario media truncada 5%', na.rm = TRUE),
                                     big.mark = ",",
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
                                     sum(bd$notatecnica$'Cuarto escenario media truncada 5%', na.rm = TRUE)/opciones$meses,
                                     big.mark = ",",
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
                                  sum(bd$notatecnica$'Cuarto escenario media truncada 5%', na.rm = TRUE)/sum(bd$descriptiva$Suma, na.rm = TRUE)*100
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
                                     sum(bd$notatecnica$'Escenario combinado mayor', na.rm = TRUE),
                                     big.mark = ",",
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
                                     sum(bd$notatecnica$'Escenario combinado mayor', na.rm = TRUE)/opciones$meses,
                                     big.mark = ",",
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
                                  sum(bd$notatecnica$'Escenario combinado mayor', na.rm = TRUE)/sum(bd$descriptiva$Suma, na.rm = TRUE)*100
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
                                     sum(bd$notatecnica$'Escenario por variabilidad y frecuencia', na.rm = TRUE),
                                     big.mark = ",",
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
                                     sum(bd$notatecnica$'Escenario por variabilidad y frecuencia', na.rm = TRUE)/opciones$meses,
                                     big.mark = ",",
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
                                  sum(bd$notatecnica$'Escenario por variabilidad y frecuencia', na.rm = TRUE)/sum(bd$descriptiva$Suma, na.rm = TRUE)*100
                              ),
                              "%"
                          ),
                          sep = " "
                    )
                })
    
    ### Sección dashboard
                
    # PAQUETES = as.data.table(readr::read_csv("PAQUETES/PAQUETES.csv"))
    # PAQUETE_PP = PAQUETES[`COMPONENTE` == "PAQUETE"]
    # PAQUETES_CC = PAQUETES[`COMPONENTE` != "PAQUETE"]
    # REF_PAQUETES = as.data.table(readr::read_csv("PAQUETES/REFERENTE-PAQUETES.csv"))
    # REF = as.data.table(readr::read_csv("PAQUETES/REFERENTE.csv"))
                
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
                          text = "¿Seguro que quieres actualizar los paquetes? Alfinal, deberá reiniciar la aplicación."
                          , showCloseButton = TRUE
                          , btn_labels = c("Cancelar", "Confirmar"))
    })

        observeEvent(input$DASHBOARD_actualizar_conf, {
            if (isTRUE(input$DASHBOARD_actualizar_conf)) {
                unlink("PAQUETES/", recursive = TRUE)
                dir.create("PAQUETES")
                withProgress(value = 0, message = "Actualizando paquetes...", {
                    write_feather(sheets_read(paquete_path, sheet = "PAQUETES", col_types = "cccdcccccccdd") 
                                  ,"PAQUETES/PAQUETES.feather")
                        incProgress(0.3)
                    write_feather(sheets_read(paquete_path, sheet = "REFERENTE-PAQUETES") 
                                  ,"PAQUETES/REFERENTE-PAQUETES.feather")
                        incProgress(0.3)
                    write_feather(sheets_read(paquete_path, sheet = "REFERENTE") 
                                  ,"PAQUETES/REFERENTE.feather")
                        incProgress(0.3)
                })
                sendSweetAlert(session, title = "¡Paquete actualizados efectivamente!", text = "Para ver los datos y gráficos actualizados, por favor recargar la página.", type = "success")
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
            selectedDash$servicio = PAQUETE_PP[`CODIGO PAQUETE` == input$paquete]$SERVICIO
            selectedDash$especialidad = PAQUETE_PP[`CODIGO PAQUETE` == input$paquete]$ESPECIALIDAD
            selectedDash$descripcion = PAQUETE_PP[`CODIGO PAQUETE` == input$paquete]$`DESCRIPCION`
            selectedDash$inclusiones = PAQUETE_PP[`CODIGO PAQUETE` == input$paquete]$`INCLUSIONES`
            selectedDash$exclusiones = PAQUETE_PP[`CODIGO PAQUETE` == input$paquete]$`EXCLUSIONES`
        }
    })
    
    output$TABLA_paquete = DT::renderDataTable({
        if(!is.null(PAQUETES)) {
            datatable(
                PAQUETES_CC[, c("CUMS/CUPS", "PRESTACION", "COMPONENTE", "TIPO DE COSTO", "VALOR", "COSTO")]
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
                value = formatAsCurrency(PAQUETES[`CODIGO PAQUETE` == selectedDash$paquete & `COMPONENTE` == "PAQUETE"]$VALOR)
                , subtitle = "Valor del paquete"
                , icon = icon("tags", lib = "font-awesome")
                , color = "green"
            )
        }
    })
    
    output$BOX_costototal = renderValueBox({
        if(!is.null(PAQUETES)) {
            valueBox(
                value = formatAsCurrency(PAQUETES[`CODIGO PAQUETE` == selectedDash$paquete & `COMPONENTE` == "PAQUETE"]$COSTO)
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
            REFPgrafico(paquetes = PAQUETES[`CODIGO PAQUETE` == selectedDash$paquete & `COMPONENTE` == "PAQUETE"], referente = REF_PAQUETES, CUMSCUPS = selectedDash$paquete, valorcosto = input$DASHBOARD_valorcosto)
    })
    
    output$PLOT_ref_cumscups = renderggiraph({
        if(!is.null(input$TABLA_paquete_rows_selected))
            REFgrafico(paquetes = PAQUETES_CC[`CODIGO PAQUETE` == selectedDash$paquete], referente = REF[`CODIGO PAQUETE` == selectedDash$paquete], CUMSCUPS = PAQUETES_CC[input$TABLA_paquete_rows_selected, list(`CUMS/CUPS`)], valorcosto = input$DASHBOARD_valorcosto)
    })
    
    output$PLOT_pie_paquete = renderggiraph({
        if(!is.null(PAQUETES)) 
            PIEchart(paquetes = PAQUETES_CC[`CODIGO PAQUETE` == selectedDash$paquete], columna = input$COL_pie_paquete, valorcosto = input$DASHBOARD_valorcosto)
    })
    
    output$TABLA_pie_paquete_resumen = DT::renderDataTable({
        if(!is.null(PAQUETES)) {
            datatable(
                resumenComp(tabla = PAQUETES_CC[`CODIGO PAQUETE` == selectedDash$paquete], columna = input$COL_pie_paquete, colsum = input$DASHBOARD_valorcosto)
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
            PIEchart(paquetes = PAQUETES_CC[`CODIGO PAQUETE` == selectedDash$paquete & COMPONENTE %in% input$COL_pie_componente], columna = input$COL_pie_componente_en, valorcosto = input$DASHBOARD_valorcosto)
    })
    
    output$TABLA_pie_componente = DT::renderDataTable({
        if(!is.null(input$COL_pie_componente)) {
            datatable(
                resumenComp(tabla = PAQUETES_CC[`CODIGO PAQUETE` == selectedDash$paquete & COMPONENTE %in% input$COL_pie_componente], columna = input$COL_pie_componente_en, colsum = input$DASHBOARD_valorcosto)
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
            PIEchart(paquetes = PAQUETES_CC[`CODIGO PAQUETE` == selectedDash$paquete & `TIPO DE COSTO` %in% input$COL_pie_tipocosto], columna = input$COL_pie_tipocosto_en, valorcosto = input$DASHBOARD_valorcosto)
    })
    
    output$TABLA_pie_tipocosto = DT::renderDataTable({
        if(!is.null(input$COL_pie_tipocosto)) {
            datatable(
                resumenComp(tabla = PAQUETES_CC[`CODIGO PAQUETE` == selectedDash$paquete & `TIPO DE COSTO` %in% input$COL_pie_tipocosto], columna = input$COL_pie_tipocosto_en, colsum = input$DASHBOARD_valorcosto)
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
                          text = "¿Seguro que quieres actualizar el pricing? Alfinal, deberá reiniciar la aplicación."
                          , showCloseButton = TRUE
                          , btn_labels = c("Cancelar", "Confirmar"))
    })
    
    observeEvent(input$pricingActualizar_conf, {
        if (isTRUE(input$pricingActualizar_conf)) {
            withProgress(value = 0, message = "Actualizando pricing...", min = 0, max = 1.1, {
                unlink("PRICING/", recursive = TRUE)
                dir.create("PRICING")
                pricingList = drive_ls(path = as_id(drive_path))
                incProgress(0.1, message = "¡Archivos leidos!")
                i = 1
                while (i <= length(pricingList$id)) {
                    drive_download(file = as_id(pricingList$id[i]), path = paste0("PRICING/", pricingList$name[i]), overwrite = T)
                    i = i + 1
                    incProgress(1/length(pricingList$id))
                }
                i = NULL

            })
            sendSweetAlert(session, title = "¡Pricing actualizados efectivamente!", text = "Para ver los datos y gráficos actualizados, por favor recargar la página.", type = "success")
        }
    })
    
    pricing = reactiveValues()
    
    observeEvent(input$pricingSelect, {
        pricing$bd = as.data.table(readr::read_csv(paste0("PRICING/", input$pricingSelect, ".csv"), na = c("-", "#N/A", "#DIV/0!", "NA")))
        
            pricing$bd$CODIGO_CUPS = as.character(pricing$bd$CODIGO_CUPS)
            pricing$bd$VALOR_UNITARIO = as.numeric(as.character(pricing$bd$VALOR_UNITARIO))
            pricing$bd$MAXIMO = as.numeric(as.character(pricing$bd$MAXIMO))
            pricing$bd$MINIMO = as.numeric(as.character(pricing$bd$MINIMO))
            pricing$bd$`MEDIA` = as.numeric(as.character(pricing$bd$"MEDIA"))
            pricing$bd$RELATIVO_MINIMO = as.numeric(as.character(pricing$bd$RELATIVO_MINIMO))
            pricing$bd$RELATIVO_MEDIA = as.numeric(as.character(pricing$bd$RELATIVO_MEDIA))
            
        pricing$bdsd.media = round(abs(sd(pricing$bd$RELATIVO_MEDIA, na.rm = TRUE)), digits = 3)
        pricing$bdsd.min = round(abs(median(pricing$bd$RELATIVO_MINIMO, na.rm = TRUE)), digits = 3)
        pricing$descriptiva = data.frame("x" = c("Valor Unitario Mínimo", "Valor Unitario Máximo", "Valor Unitario Promedio", "Mínimo", "Máximo", "Media"), "y" = c(0,0,0,0,0,0))

    })
    
    output$pricingOutPrestacion = renderUI({
        pickerInput("pricingFiltroPrestacion", label = "Nombre de Prestación",
                    choices = unique(pricing$bd$NOMBRE_PRESTACION), multiple = TRUE,
                    options = list(`actions-box` = TRUE, `deselect-all-text` = "Deseleccionar todos",
                                   `select-all-text` = "Seleccionar todos", `live-search` = TRUE))
    })
    
    output$pricingOutEntidad = renderUI({
        pickerInput("pricingFiltroEntidad", label = "Nombre Entidad",
                    choices = unique(pricing$bd$NOMBRE_ENTIDAD), multiple = TRUE,
                    options = list(`actions-box` = TRUE, `deselect-all-text` = "Deseleccionar todos",
                                   `select-all-text` = "Seleccionar todos", `live-search` = TRUE))
    })
    
    output$pricingOutObs = renderUI({
        pickerInput("pricingFiltroObservacion", label = "Observación",
                    choices = unique(pricing$bd$OBSERVACIÓN),
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
                formatCurrency(c("MIN", "MEDIA", "MAX", "MEDIA MERCADO"))
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


})
