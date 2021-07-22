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
#' @title Diferencia límites frecuencia
#' @description Compara frecuencias con límites establecidos por agrupador y
#' devuelve la diferencia por arriba y abajo.
#' @param timeseries Descriptiva en serie de tiempo o version reducida
#' @param nota_tecnica Tabla de nota técnica producida por el parser
#' @param agrupador Nombre de la columna agrupadora
#' @return Tabla con comparación con los límites de los agrupadores de mes
#' a mes.
#' @example
#' diferencias_limites <- diferencias_limites_nt(
#'  timeseries = episodios$timeseries,
#'  nota_tecnica = nota_tecnica,
#'  agrupador = "tipo_ambito"
#' )

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
    mutate(
      diferencia_total = sum(diferencia, na.rm = TRUE),
      diferencia_media = mean(diferencia, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(mes_anio_num) %>%
    collect() %>%
    mutate(mes_nombre = mes_spanish_juntos(mes_anio_num))


  # Pivot la tabla para tener formato de seguimiento
  diferencias <- timeseries %>%
    pivot_wider(
      id_cols = c(
        agrupador, frec_mes_min, frec_mes_max, diferencia_total,
        diferencia_media),
      names_from = mes_nombre,
      values_from = diferencia)

  return(diferencias)

}