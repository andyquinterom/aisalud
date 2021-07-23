pivot_frecuencias <- function(frecuencias, agrupador, intervalo = "mes") {

  agrupador <- unique(agrupador)

  means_sums <- frecuencias %>%
    group_by(!!!rlang::syms(agrupador)) %>%
    summarise(
      frec_media = round(mean(Frecuencia, na.rm = TRUE), digits = 3),
      frec_suma = round(sum(Frecuencia, na.rm = TRUE), digits = 3)
    )

  frecuencias <- frecuencias %>%
    group_by(!!!rlang::syms(agrupador)) %>%
    arrange(mes_anio_num) %>%
    pivot_wider(
      id_cols = c(unidad_conteo, !!!rlang::syms(agrupador)),
      names_from = mes_anio_num,
      values_from = Frecuencia)

  if (intervalo == "mes") {
    frecuencias <- frecuencias %>%
      rename_with(
        mes_spanish_juntos,
        .cols = -seq_len(length(agrupador) + 1))
  }

  frecuencias <- frecuencias %>%
      left_join(means_sums) %>%
      relocate(unidad_conteo, !!!rlang::syms(agrupador), frec_suma, frec_media)

  return(frecuencias)

# Queda comentada esta sección hastas que quede actualizado dbplyr
# https://github.com/tidyverse/dbplyr/pull/676
#  if (intervalo == "semana") {
#    frecuencias <- frecuencias %>%
#      rename_with(function(x) {
#        paste(substr(x, 1, 4), substr(x, 5, 6), sep = " - ")
#      }, .cols = -seq_len(length(agrupador)))
#  }

}