comparar_nt_frecuencias <- function(frecuencias, nota_tecnica, agrupador) {

  frec_y_nt <- frecuencias %>%
    rename(agrupador = !!as.name(agrupador)) %>%
    inner_join(nota_tecnica %>%
                 select(agrupador, frec_mes, cm)) %>%
    group_by(unidad_conteo, agrupador, frec_mes, cm) %>%
    mutate(
      valor_mes = frec_mes * cm,
      frec_media_por_cm = frec_media * cm,
      frec_suma_por_cm = frec_suma * cm) %>%
    group_by(unidad_conteo, agrupador, frec_mes, cm, valor_mes, frec_suma,
      frec_media, frec_suma_por_cm, frec_media_por_cm) %>%
    relocate(unidad_conteo, agrupador, frec_mes, cm, valor_mes, frec_suma,
      frec_media, frec_suma_por_cm, frec_media_por_cm)

}
