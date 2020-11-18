histograma_agrupador <- function(data, titulo) {
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

