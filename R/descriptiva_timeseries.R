descriptiva_timeseries <- function(
  data, agrupador, col_anio = "ais_anio", col_mes = "ais_mes") {

  if (nrow(data) == 0) stop("Data.frame vacio")

  if (!identical(c("ais_mes", "ais_anio"), c(col_mes, col_anio))) {
    data <- data %>%
      rename(ais_mes = !!rlang::sym(col_mes), ais_anio = !!rlang::sym(ais_anio))
  }

  unidades_conteo <- data %>%
    group_by(!!!rlang::syms(agrupador)) %>%
    summarise(unidad_conteo = first(unidad_conteo))

  data <- data %>%
    mutate(mes_anio_num = ais_anio * 100 + ais_mes) %>%
    select(-unidad_conteo)

  meses_limites <- data %>%
    select(ais_mes, ais_anio, mes_anio_num) %>%
    ungroup() %>%
    summarise(
      min = min(mes_anio_num),
      max = max(mes_anio_num),
      anio_min = min(ais_anio),
      anio_max = max(ais_anio),
      mes_min = min(ais_mes),
      mes_max = max(ais_mes)
    )

  meses_completos <- tibble(
    anio = sort(rep(meses_limites$anio_min:meses_limites$anio_max, 12)),
    meses = rep(1:12, meses_limites$anio_max - meses_limites$anio_min + 1)) %>%
    mutate(mes_anio_num = anio * 100 + meses) %>%
    filter(mes_anio_num >= meses_limites$min &
      mes_anio_num <= meses_limites$max) %>%
    select(mes_anio_num) %>%
    mutate(placeholder_key = "key")

  data_to_join <- data %>%
    group_by(!!!rlang::syms(agrupador)) %>%
    summarise(placeholder_key = "key") %>%
    ungroup()

  meses_completos <- data_to_join %>%
    full_join(meses_completos, copy = TRUE) %>%
    select(-placeholder_key)

  data <- data %>%
    group_by(!!!rlang::syms(agrupador), mes_anio_num) %>%
    full_join(meses_completos, copy = TRUE) %>%
    mutate(across(.cols = everything(), .fns = function(x) {
      case_when(
        is.na(x) ~ 0,
        TRUE ~ x
      )
    })) %>%
    mutate(
      ais_anio = mes_anio_num %/% 100,
      ais_mes  = mes_anio_num %% 100
    ) %>%
    left_join(unidades_conteo) %>%
    relocate(unidad_conteo) %>%
    collect()

  return(data)

}
