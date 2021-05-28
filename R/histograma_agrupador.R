histograma_agrupador <- function(data, titulo = "Valor", numero_bins = NULL,
                                 columnas_sep) {

  histograma <- plot_ly(type = "bar")

  lapply(
    X = names(data),
    FUN = function(i) {
      if (!is.null(data[[i]])) {
        if (!is.null(numero_bins)) {
          x <- data[[i]] %>%
            ungroup() %>%
            right_join(columnas_sep, copy = TRUE) %>%
            db_compute_bins(valor_calculos, bins = numero_bins)
        } else {
          x <- data[[i]] %>%
            ungroup() %>%
            right_join(columnas_sep, copy = TRUE) %>%
            db_compute_bins(valor_calculos)
        }
        histograma <<- histograma %>%
          add_bars(x = x[["valor_calculos"]],
                    y = x[["count"]],
                    name = toupper(i))
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
