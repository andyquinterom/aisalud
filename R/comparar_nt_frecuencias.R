comparar_nt_frecuencias <- function(frecuencias, nota_tecnica, agrupador,
                                    indicador = "diff") {

  frec_y_nt <- frecuencias %>%
    rename(agrupador = !!as.name(agrupador)) %>%
    inner_join(nota_tecnica %>%
                 select(agrupador, frec_mes, cm)) %>%
    group_by(agrupador, frec_mes, cm) %>%
    mutate(valor_mes = frec_mes * cm) %>%
    group_by(agrupador, frec_mes, cm, valor_mes)

  frec_y_nt %>%
    rename(media = frec_media, total = frec_suma) %>%
    mutate(across(.fns = replace_na, replace = 0)) %>%
    {if (indicador == "diff") {
      mutate(., across(.fns = ~ .x - frec_mes))
    } else if (indicador == "perc") {
      mutate(., across(.fns = ~ .x / na_if(frec_mes, 0))) %>%
      mutate(media_valor = media * valor_mes)
    } else if (indicador == "diff_cm") {
      mutate(., across(.fns = ~ (.x - frec_mes) * cm))
    } else if (indicador == "cm") {
      mutate(., across(.fns = ~ .x * cm))
    } else {.}} %>%
    {if (indicador == "diff") {
      mutate(., total_valor = total * cm)
    } else {.}} %>%
    relocate(agrupador, frec_mes, cm, valor_mes) %>%
    return()

}
