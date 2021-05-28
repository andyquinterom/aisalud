outliers_percentil <- function(data, columna, columna_valor, percentil,
                             frecuencia = 1, frec_cantidad = FALSE) {

  data <- data %>%
    mutate(valor_calculos = as.numeric(!!as.name(columna_valor)))

  valor_total <- data %>%
    transmute(valor_total = sum(valor_calculos, na.rm = TRUE)) %>%
    distinct() %>%
    collect() %>%
    unlist() %>%
    as.numeric()

  valor_maximo <- data %>%
    group_by(!!as.name(columna)) %>%
    summarise(valor_calculos = sum(valor_calculos, na.rm = TRUE)) %>%
    select(valor_calculos) %>%
    collect() %>%
    transmute(percentil = quantile(valor_calculos, probs = percentil)) %>%
    distinct() %>%
    collect() %>%
    unlist() %>%
    as.numeric()

  outliers <- data %>%
    group_by(!!as.name(columna)) %>%
    summarise(valor_calculos = sum(valor_calculos, na.rm = TRUE),
              frec = ifelse(
                test = frec_cantidad,
                yes = sum(cantidad, na.rm = TRUE),
                no = n())) %>%
    mutate(porcentaje = paste0(
      round(100*valor_calculos/valor_total, digits = 2),
      "%")) %>%
    filter(frec >= frecuencia) %>%
    filter(valor_calculos >= valor_maximo) %>%
    arrange(desc(valor_calculos)) %>%
    collect() %>%
    as.data.table()

  return(outliers)

}
