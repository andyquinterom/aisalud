histograma_agrupador <- function(data, titulo = "Valor") {
  numero_bins <- round(6.6*log10(length(data)), digits = 0)
  histogram <- hist(
    x = data,
    breaks = numero_bins,
    plot = FALSE,
  )
  plot_ly(x = histogram$mids, y = histogram$counts) %>%
    add_bars(name = titulo) %>%
    layout(xaxis = list(title = "Valor"),
           yaxis = list(title = "Conteo"),
           showlegend = FALSE) %>%
    config(locale = "es")
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