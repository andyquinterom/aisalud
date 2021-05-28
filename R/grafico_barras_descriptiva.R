grafico_barras_descriptiva <- function(data, columna_numeros, columnas_sep) {

  bar_plot <- NULL

  lapply(
    X = seq_len(nrow(columnas_sep)),
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
    "Suma de valor", "Frecuencia", "Media", "Mediana",
    "Coeficiente de variación")[c(
      "Suma",
      "Frecuencia",
      "Media",
      "P50",
      "Coef.var") == columna_numeros] %>%
    unname()

  return(bar_plot %>%
           config(locale = "es") %>%
           layout(xaxis = list(title = "Agrupador"),
                  yaxis = list(title = yaxis,
                               tickformat = ",.2f"),
                  showlegend = FALSE))

}
