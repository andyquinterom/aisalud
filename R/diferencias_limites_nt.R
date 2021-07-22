#'
#'
#'
#'
#'
#'
#' @param timeseries Descriptiva en serie de tiempo o version reducida
#' @param nota_tecnica Tabla de nota técnica producida por el parser
#' @param agrupador Nombre de la columna agrupadora
#'
#'
#'
#'
#'
#'

diferencias_limites_nt <- function(timeseries, nota_tecnica, agrupador) {

  # Quitar agrupaciones que no tienen frec_mes_max o frec_mes_min
   nota_tecnica <- nota_tecnica %>%
     filter((!is.na(frec_mes_min)) | (!is.na(frec_mes_max)))

  # Juntar la nota técnica con la serie de tiempoi

  timeseries <- timeseries %>%
    rename(agrupador = !!rlang::sym(agrupador)) %>%
    inner_join(nota_tecnica, by = "agrupador", copy = TRUE) %>%
    mutate(
      diferencia = case_when(
        Frecuencia < frec_mes_min ~ Frecuencia - frec_mes_min,
        Frecuencia > frec_mes_max ~ Frecuencia - frec_mes_max,
        TRUE ~ 0))  %>%
    mutate(across(
      .cols = c(frec_mes_min, frec_mes_max, diferencia),
      .fns = ~ case_when(
        is.na(.x) ~ 0,
        TRUE ~ .x))) %>%
    group_by(agrupador) %>%
    mutate(diferencia_total = sum(diferencia, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(mes_anio_num) %>%
    collect() %>%
    mutate(mes_nombre = mes_spanish_juntos(mes_anio_num))


  # Pivot la tabla para tener formato de seguimiento
  diferencias <- timeseries %>%
    pivot_wider(
      id_cols = c(agrupador, frec_mes_min, frec_mes_max, diferencia_total),
      names_from = mes_nombre,
      values_from = diferencia)

  return(diferencias)

}