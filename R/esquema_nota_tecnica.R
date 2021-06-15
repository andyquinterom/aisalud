esquema_nota_tecnica <- function(timeseries, agrupador, perfil = NULL) {
  resumen_inicial <- timeseries %>%
    group_by(!!!rlang::syms(agrupador)) %>%
    summarise(
      cm = quantile(Media, 0.5, na.rm = TRUE),
      frec_m = mean(Frecuencia, 0.5, na.rm = TRUE)
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
    return(list(cm = cm, n = frec_m, percentil = 0.5))
  })

  return(
    list(
      nota_tecnica = list(
        poblacion = 1,
        agrupadores = agrupadores,
        perfil = perfil
      )
    )
  )

}
