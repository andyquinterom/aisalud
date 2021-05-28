comparar_nt_valor_factura <- function(
  indicador, descriptiva_tabla, nota_tecnica, agrupador, col_anio, col_mes) {

  descriptiva_tabla %>%
    select(!!!rlang::syms(c(agrupador, col_anio, col_mes)), Suma) %>%
    rename(agrupador = !!as.name(agrupador)) %>%
    arrange(!!rlang::sym(col_anio), !!rlang::sym(col_mes)) %>%
    mutate(ais_mes_nombre = mes_spanish(!!!rlang::syms(col_mes))) %>%
    pivot_wider(
      id_cols = c(agrupador),
      names_from = c(ais_anio, ais_mes_nombre),
      values_from = Suma,
      names_sep = " - ") %>%
    inner_join(nota_tecnica %>%
                 select(agrupador, valor_mes)) %>%
    mutate(across(.fns = replace_na, replace = 0)) %>%
    group_by(agrupador, valor_mes) %>%
    relocate(agrupador, valor_mes) %>%
    {if (indicador == "diff") {
      mutate(., across(.fns = ~ .x - valor_mes))
    } else if (indicador == "perc") {
      mutate(., across(.fns = ~ .x / na_if(valor_mes, 0)))
    } else {.}} %>%
    {if (indicador %in% c("diff", "suma")) {
      mutate(., total = rowSums(across(), na.rm = TRUE))
    } else if (indicador == "perc") {
      mutate(., media = rowMeans(across(), na.rm = TRUE),
             media_valor = media * valor_mes)
    } else {.}}

}
