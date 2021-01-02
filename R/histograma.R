histograma_agrupador <- function(data, titulo = "Valor", numero_bins = NULL,
                                 columnas_sep) {
  
  histograma <- plot_ly(type = "bar")
  
  lapply(
    X = names(data),
    FUN = function(i) {
      if (!is.null(data[[i]])) {
        x <- data[[i]] %>%
          ungroup() %>%
          merge(columnas_sep) %>%
          select(valor_calculos) %>%
          collect() %>%
          unlist() %>%
          as.numeric()
        if (!identical(x, numeric(0))) {
          numero_bins <- ifelse(
            test = is.null(numero_bins),
            yes = round(6.6*log10(length(x)), digits = 0),
            no = numero_bins)
          temp_histograma <- hist(
            x = x,
            breaks = numero_bins,
            plot = FALSE,
          )
          histograma <<- histograma %>%
            add_trace(x = temp_histograma$mids, y = temp_histograma$counts,
                      name = toupper(i))
        }
        
      }
    }
  )
  
  histograma %>%
    config(locale = "es") %>%
    layout(barmode = "overlay",
           xaxis = list(title = "Valor",
                        tickformat = ",.2f"),
           yaxis = list(title = "Conteo",
                        tickformat = ",.2f"),
           showlegend = TRUE)
}

histograma_edades <- function(data, columna_numero, columna_sep) {
  numero_bins <- range(data[[columna_numero]], na.rm = TRUE)
  
  grafico <- plot_ly(alpha = 0.7, type = "histogram")
  
  if (is.null(columna_sep)) {
    grafico <- grafico %>% 
      add_histogram(
        x = data[[columna_numero]],
        name = "") %>%
      config(locale = "es") %>%
      layout(barmode = "overlay",
             xaxis = list(title = "Edad"),
             yaxis = list(title = "Conteo",
                          tickformat = ",.2f"),
             showlegend = FALSE)
  } else {
    lapply(
      X = unique(data[[columna_sep]]),
      FUN = function(i) {
        grafico <<- grafico %>%
          add_histogram(
            x = data[get(columna_sep) == i][[columna_numero]],
            name = i
          )
      }
    )
    
    grafico <- grafico %>%
      config(locale = "es") %>%
      layout(barmode = "overlay",
             xaxis = list(title = "Edad"),
             yaxis = list(title = "Conteo",
                          tickformat = ",.2f"),
             showlegend = TRUE)
  }
  
  return(grafico)
  
}

grafico_barras <- function(data, columna) {
  datos_grafico <- data[
    ,
    .(conteo = .N),
    by = c(columna)]

  ggplotly(
    ggplot(datos_grafico,
           aes_string(x = columna,
                      y = "conteo")) +
      geom_bar(stat = "identity", width = 1,
               fill = "#8a2be2") +
      coord_flip()+
      labs(x = columna, y = "Frecuencia") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(legend.title = element_blank())) %>%
    config(locale = "es") %>%
    layout(legend = list(x= 1, y = 0.5))
}

grafico_barras_descriptiva <- function(data, columna_numeros, columnas_sep) {
  
  bar_plot <- NULL
  
  lapply(
    X = 1:nrow(columnas_sep),
    FUN = function(i) {
      data_temp <- merge.data.table(
        x = columnas_sep[i],
        y = data
      )[[columna_numeros]]
      if (i == 1) {
        bar_plot <<- plot_ly(type = "bar", y = data_temp, x = paste(
          unlist(columnas_sep[i]), collapse = "\n"
        ))
      } else {
        bar_plot <<- bar_plot %>%
          add_trace(y = data_temp, x = paste(
            unlist(columnas_sep[i]), collapse = "\n"
          ))
      }
    }
  )
  
  yaxis <- c(
    "Suma de valor", "Frecuencia", "Media", "Mediana", "Coeficiente de variación"
  )[c("Suma", "Frecuencia", "Media", "P50", "Coef.var") == columna_numeros] %>%
    unname()
  
  return(bar_plot %>% 
           config(locale = "es") %>%
           layout(xaxis = list(title = "Agrupador"),
                  yaxis = list(title = yaxis,
                               tickformat = ",.2f"),
                  showlegend = FALSE))
  
}