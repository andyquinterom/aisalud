outliers_iqr <- function(data, columna, columna_valor, multiplicativo,
                        frecuencia = 1, frec_cantidad = FALSE) {

  multiplicativo <- as.numeric(multiplicativo)

  data <- data %>%
    mutate(valor_calculos = as.numeric(!!as.name(columna_valor)))

  valor_total <- data %>%
    transmute(valor_total = sum(valor_calculos, na.rm = TRUE)) %>%
    distinct() %>%
    collect() %>%
    unlist() %>%
    as.numeric()

  limites <- data %>%
    group_by(!!as.name(columna)) %>%
    summarise(valor_calculos = sum(valor_calculos, na.rm = TRUE)) %>%
    select(valor_calculos) %>%
    collect() %>%
    transmute(
      lower = quantile(valor_calculos, 0.25) - multiplicativo*IQR(valor_calculos),
      upper = quantile(valor_calculos, 0.75) + multiplicativo*IQR(valor_calculos)
    ) %>%
    distinct() %>%
    collect()

  upper <- limites[["upper"]] %>% unname() %>% as.numeric()
  lower <- limites[["lower"]] %>% unname() %>% as.numeric()

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
    filter(valor_calculos >= upper | valor_calculos <= lower) %>%
    arrange(desc(valor_calculos)) %>%
    collect() %>%
    as.data.table()

  return(outliers)

}
