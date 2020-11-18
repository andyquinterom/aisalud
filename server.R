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
  
 
  # Modulo descriptiva y episodios --------------------------------------------
  
  callModule(
    module = episodios_server, 
    id = "episodios_modulo",
    opciones = opciones,
    nombre_id = "episodios_modulo",
    datos = datos_modulos
  )
  
 
  # Modulo outliers -----------------------------------------------------------
  
  callModule(
    module = outliers_server,
    id = "outliers_modulo",
    datos = datos_modulos,
    opciones = opciones,
    nombre_id = "outliers_modulo"
  )
  
 
  # Modulo generar nota técnica -----------------------------------------------
  
  callModule(
    module = nota_tecnica_server,
    id = "nota_tecnica_modulo",
    nombre_id = "nota_tecnica_modulo",
    datos = datos_modulos,
    opciones = opciones
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
                       showlegend = FALSE) %>% 
                config(locale = "es")
            } else {
              
              datos_grafico <- histograma_datos[
                ,
                .(conteo = .N),
                by = c(opciones$histograma_col)]
              
              ggplotly(
                ggplot(datos_grafico,
                       aes_string(x = opciones$histograma_col, 
                                  y = "conteo")) +
                  geom_bar(stat = "identity", width = opciones$histograma_width,
                           fill = "#8a2be2") +
                  coord_flip()+
                  labs(x = opciones$histograma_col, y = "Frecuencia") +
                  scale_y_continuous(labels = scales::comma) +
                  theme_minimal() +
                  theme(legend.title = element_blank())) %>% 
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
                       barmode = "stack") %>% 
                config(locale = "es")
              
            } else {
              
              datos_grafico <- histograma_datos[
                ,
                .(conteo = .N),
                by = c(opciones$histograma_col, opciones$histograma_fill)]
              
              
              ggplotly(
                ggplot(datos_grafico,  
                       aes_string(x = opciones$histograma_col, 
                                  y = "conteo",
                                  fill = opciones$histograma_fill)) +
                  geom_bar(stat = "identity",
                           width = opciones$histograma_width) +
                  coord_flip()+
                  labs(x = opciones$histograma_col, y = "Frecuencia") +
                  scale_fill_brewer(palette = "Set1") +
                  scale_y_continuous(labels = scales::comma) +
                  theme_minimal() +
                  theme(legend.title = element_blank())) %>% 
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
  
 
})