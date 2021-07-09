#
# Analítica Integrada Salud
#
# Derechos de autor 2021 por MD&CO Consulting Group (NIT 901.119.781-5)
# Copyright (C) 2021 by MD&CO Consulting Group
#
# Este programa es software libre: puede redistribuirlo o modificarlo bajo
# los términos de la licencia Affero General Public License tal cual
# publicada por la Free Software Foundation, sea la versión 3 de la licencia
# o cualquier versión posterior. Este programa se distribuye SIN GARANTÍA
# EXPERSA O IMPLÍCITA, INCLUIDAS LAS DE NO INFRACCIÓN, COMERCIABILIDAD O
# APTITUD PARA UN PROPÓSITO PARTICULAR. Referir a la
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) para más detalles.
#

comparar_nt_valor_factura <- function(
  indicador, descriptiva_tabla, nota_tecnica, agrupador, col_anio, col_mes) {

  descriptiva_tabla %>%
    select(unidad_conteo, !!!rlang::syms(c(agrupador, col_anio, col_mes)),
      Suma) %>%
    descriptiva_timeseries(agrupador = agrupador, col_anio, col_mes) %>%
    rename(agrupador = !!as.name(agrupador)) %>%
    arrange(ais_anio, ais_mes) %>%
    mutate(ais_mes_nombre = mes_spanish(ais_mes)) %>%
    pivot_wider(
      id_cols = c(unidad_conteo, agrupador),
      names_from = c(ais_anio, ais_mes_nombre),
      values_from = Suma,
      names_sep = " - ") %>%
    inner_join(nota_tecnica %>%
                 select(agrupador, valor_mes)) %>%
    mutate(across(.fns = replace_na, replace = 0)) %>%
    group_by(unidad_conteo, agrupador, valor_mes) %>%
    relocate(unidad_conteo, agrupador, valor_mes) %>%
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
