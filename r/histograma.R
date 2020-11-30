histograma_agrupador <- function(data, titulo = "Valor") {
  numero_bins <- round(6.6*log10(length(data)), digits = 0)
  histogram <- hist(
    x = data,
    breaks = numero_bins,
    plot = FALSE,
  )
  plot_ly(x = histogram$mids, y = histogram$counts) %>%
    add_bars(name = titulo) %>%
    config(locale = "es") %>%
    layout(xaxis = list(title = "Valor"),
           yaxis = list(title = "Conteo",
                        tickformat = ",.2f"),
           showlegend = FALSE)
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
    "Suma de valor", "Frecuencia", "Media", "MEdiana", "Coeficiente de variación"
  )[c("Suma", "Frecuencia", "Media", "P50", "Coef.var") == columna_numeros] %>%
    unname()
  
  return(bar_plot %>% 
           config(locale = "es") %>%
           layout(xaxis = list(title = "Agrupador"),
                  yaxis = list(title = yaxis,
                               tickformat = ",.2f"),
                  showlegend = FALSE))
  
}