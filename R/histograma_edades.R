histograma_edades <- function(data, columna_numero, columna_sep = NULL,
                              numero_bins = NULL) {

  numero_bins <- ifelse(
    test = is.null(numero_bins),
    yes = {
      data %>%
        ungroup() %>%
        select(!!as.name(columna_numero)) %>%
        distinct() %>%
        summarise(length = n()) %>%
        collect() %>%
        unlist() %>%
        as.numeric()},
    no = numero_bins
  )

  valores_col_sep <- list()
  valores_col_sep <- ifelse(
    test = is.null(columna_sep),
    yes = list(),
    no = {
      list(data %>%
        ungroup() %>%
        select(!!as.name(columna_sep)) %>%
        distinct() %>%
        collect() %>%
        unlist() %>%
        unname())
    }
  )

  grafico <- plot_ly(alpha = 0.7, type = "histogram", colors = "Dark24")

  if (is.null(columna_sep)) {
    datos_bins <- data %>%
      ungroup() %>%
      db_compute_bins(x = !!as.name(columna_numero), bins = numero_bins)
    grafico <- grafico %>%
      add_bars(
        x = datos_bins[[columna_numero]],
        y = datos_bins[["count"]],
        name = "") %>%
      config(locale = "es") %>%
      layout(barmode = "overlay",
             xaxis = list(title = "Edad"),
             yaxis = list(title = "Conteo",
                          tickformat = ",.2f"),
             showlegend = FALSE)
  } else {
    lapply(
      X = valores_col_sep[[1]],
      FUN = function(i) {
        datos_bins_temp <- data %>%
          ungroup() %>%
          filter(!!as.name(columna_sep) == i) %>%
          db_compute_bins(x = !!as.name(columna_numero), bins = numero_bins)
        grafico <<- grafico %>%
          add_bars(
            x = datos_bins_temp[[columna_numero]],
            y = datos_bins_temp[["count"]],
            name = i)
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
