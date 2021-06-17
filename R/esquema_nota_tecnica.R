esquema_nota_tecnica <- function(timeseries, agrupador, perfil = NULL,
  poblacion = 1) {
  if (is.null(poblacion) | is.na(poblacion)) poblacion <- 1
  resumen_inicial <- timeseries %>%
    group_by(!!!rlang::syms(agrupador)) %>%
    summarise(
      cm = round(quantile(Media, 0.5, na.rm = TRUE), digits = 0),
      frec_m = round(mean(Frecuencia, na.rm = TRUE), digits = 3),
      n_minimo = min(Frecuencia, na.rm = TRUE),
      n_maximo = max(Frecuencia, na.rm = TRUE)
    )

  agrupadores <- resumen_inicial[[agrupador]]
  names(agrupadores) <- agrupadores
  agrupadores <- purrr::map(agrupadores, function(x) {
    filtrado <- resumen_inicial %>%
      filter(!!rlang::sym(agrupador) == x)
    cm <- filtrado %>%
      pull(cm)
    frec_m <- filtrado %>%
      pull(frec_m)
    frec_m_min <- filtrado %>%
      pull(n_minimo)
    frec_m_max <- filtrado %>%
      pull(n_maximo)
    return(
      list(
        cm = cm,
        n = frec_m,
        percentil = 0.5,
        n_max = frec_m_max,
        n_min = frec_m_min
      )
    )
  })

  return(
    list(
      nota_tecnica = list(
        poblacion = poblacion,
        agrupadores = agrupadores,
        perfil = perfil
      )
    )
  )

}
