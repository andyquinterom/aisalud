descriptiva <- function(data, columnas, columna_valor, columna_suma,
                        prestaciones = FALSE, frec_cantidad = FALSE) {

  columnas <- unique(columnas)
  data <- data %>%
    mutate(valor_calculos = as.numeric(!!as.name(columna_valor)))

  if (!prestaciones) {
    data <- data %>%
      group_by(!!!rlang::syms(unique(c(columna_suma, columnas)))) %>%
      summarise(valor_calculos = sum(valor_calculos, na.rm = TRUE),
                cantidad = 1)
  }
  if ("data.frame" %in% class(data)) {
    data_descriptiva <- data %>%
      group_by(!!!rlang::syms(columnas)) %>%
      summarise(
        "Frecuencia" = ifelse(
          test = prestaciones && frec_cantidad,
          yes = sum(cantidad, na.rm = TRUE),
          no = n()),
        "Suma" = sum(valor_calculos, na.rm = TRUE),
        "Media" = NA,
        "P25" = round(quantile(valor_calculos, probs = 0.25, na.rm = TRUE), 2),
        "P50" = round(quantile(valor_calculos, probs = 0.5, na.rm = TRUE), 2),
        "P75" = round(quantile(valor_calculos, probs = 0.75, na.rm = TRUE), 2),
        "P90" = round(quantile(valor_calculos, probs = 0.9, na.rm = TRUE), 2),
        "Desv.tipica" = round(sd(valor_calculos, na.rm = TRUE), 2),
        "Coef.var" = round(na_if(sd(valor_calculos, na.rm = TRUE), 0) /
                             na_if(mean(valor_calculos, na.rm = TRUE), 0), 2),
        "Min." = min(valor_calculos, na.rm = TRUE),
        "Max." = max(valor_calculos, na.rm = TRUE),
        "Rango" = max(valor_calculos, na.rm = TRUE) -
          min(valor_calculos, na.rm = TRUE)
      )
  } else {
    data_descriptiva <- data %>%
      group_by(!!!rlang::syms(columnas)) %>%
      summarise(
        "Frecuencia" = ifelse(
          test = prestaciones && frec_cantidad,
          yes = sum(cantidad, na.rm = TRUE),
          no = n()),
        "Suma" = sum(valor_calculos, na.rm = TRUE),
        "Media" = NA,
        "P25" = round(quantile(valor_calculos, probs = 0.25), 2),
        "P50" = round(quantile(valor_calculos, probs = 0.5), 2),
        "P75" = round(quantile(valor_calculos, probs = 0.75), 2),
        "P90" = round(quantile(valor_calculos, probs = 0.9), 2),
        "Desv.tipica" = round(sd(valor_calculos, na.rm = TRUE), 2),
        "Coef.var" = round(na_if(sd(valor_calculos, na.rm = TRUE), 0) /
                             na_if(mean(valor_calculos, na.rm = TRUE), 0), 2),
        "Min." = min(valor_calculos, na.rm = TRUE),
        "Max." = max(valor_calculos, na.rm = TRUE),
        "Rango" = max(valor_calculos, na.rm = TRUE) -
          min(valor_calculos, na.rm = TRUE)
      )
  }

  data_descriptiva <- data_descriptiva %>%
    mutate(Media = round(Suma / na_if(Frecuencia, 0), 2)) %>%
    arrange(Suma)
  fields <- data_descriptiva %>%
    colnames()

  if (".add" %in% fields) {
    data_descriptiva <- data_descriptiva %>%
      select(-`.add`)
  }
  data_descriptiva <- data_descriptiva %>%
    collect()
  setDT(data_descriptiva)

  setnames(
    data_descriptiva,
    c(columnas,
      "Frecuencia",
      "Suma", "Media",
      "P25",
      "P50",
      "P75",
      "P90",
      "Desv.tipica",
      "Coef.var",
      "Min.",
      "Max.",
      "Rango")
  )

  return(list(
    "descriptiva" = data_descriptiva,
    "data" = data
  ))
}
