frecuencias <- function(
  data, agrupador, columna_fecha, columna_suma,
  intervalo = "mes", prestaciones = FALSE, frec_cantidad = FALSE) {

  agrupador <- unique(agrupador)

  data <- data %>%
    mutate_at(vars(agrupador), as.character)

  if (intervalo == "mes") {
    data <- data %>%
      mutate(mes_num = month(!!as.name(columna_fecha)),
             anio_num = year(!!as.name(columna_fecha))) %>%
      mutate(mes_anio_num = anio_num * 100 + mes_num)
  } else if (intervalo == "dia") {
    data <- data %>%
      mutate(mes_anio_num = !!as.name(columna_fecha))
  } else if (intervalo == "semana") {
    data <- data %>%
      mutate(mes_num = round((yday(!!as.name(columna_fecha))  - 1) %/% 7 + 1),
             anio_num = year(!!as.name(columna_fecha))) %>%
      mutate(mes_anio_num = anio_num * 100 + mes_num)
  }

  if (!prestaciones) {
    data <- data %>%
      group_by(!!!rlang::syms(unique(c(columna_suma, agrupador)))) %>%
      summarise(mes_anio_num = max(mes_anio_num), cantidad = 1)
  }

  meses_limites <- data %>%
    select(mes_anio_num) %>%
    ungroup() %>%
    summarise(min = min(mes_anio_num), max = max(mes_anio_num)) %>%
    collect() %>%
    mutate(
      anio_min = min %/% 100,
      anio_max = max %/% 100,
      mes_min = min %% 100,
      mes_max = max %% 100)

  meses_completos <- tibble(
    anio = sort(rep(meses_limites$anio_min:meses_limites$anio_max, 12)),
    meses = rep(1:12, meses_limites$anio_max - meses_limites$anio_min + 1)) %>%
  mutate(mes_anio_num = anio * 100 + meses) %>%
  filter(mes_anio_num >= meses_limites$min &
    mes_anio_num <= meses_limites$max) %>%
  select(mes_anio_num)

  data <- data %>%
    group_by(!!!rlang::syms(agrupador), mes_anio_num) %>%
    summarise(Frecuencia = ifelse(
      test = prestaciones && frec_cantidad,
      yes = sum(cantidad, na.rm = TRUE),
      no = n())) %>%
    full_join(meses_completos, copy = TRUE) %>%
    mutate(Frecuencia = case_when(
        is.na(Frecuencia) ~ 0,
        TRUE ~ Frecuencia)) %>%
    group_by(!!!rlang::syms(agrupador)) %>%
    arrange(mes_anio_num) %>%
    collect()

  data <- data %>%
    pivot_wider(
      names_from = mes_anio_num,
      values_from = Frecuencia) %>%
    mutate(across(
        .cols = -seq_len(length(agrupador)),
        .fns = function(x) case_when(is.na(x) ~ 0, TRUE ~ x)))

  if (intervalo == "mes") {
    data <- data %>%
      rename_with(mes_spanish_juntos, .cols = -seq_len(length(agrupador)))
  } else if (intervalo == "semana") {
    data <- data %>%
      rename_with(function(x) {
        paste(substr(x, 1, 4), substr(x, 5, 6), sep = " - ")
      }, .cols = -seq_len(length(agrupador)))
  }

  data <- data %>%
    mutate(
      frec_media = round(rowMeans(across(), na.rm = TRUE), digits = 3),
      frec_suma  = round(
        rowSums(across(-frec_media), na.rm = TRUE),
        digits = 3)) %>%
  relocate(!!!rlang::syms(agrupador), frec_suma, frec_media)

  return(data)
}
