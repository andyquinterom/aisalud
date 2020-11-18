caja_de_bigotes_agrupador <- function(data, titulo) {
  plot_ly(y = data, type = "box", name = titulo) %>%
    config(locale = "es")
}

