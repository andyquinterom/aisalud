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

comparar_nt <- function(timeseries, nota_tecnica, agrupador) {

  timeseries <- timeseries %>%
    # Juntar la nota_tecnica con la serie de tiempo
    rename(agrupador = !!rlang::sym(agrupador)) %>%
    right_join(
      select(nota_tecnica, agrupador),
      by = "agrupador",
      copy = TRUE) %>%
    mutate(
      ais_mes = mes_anio_num %% 100,
      ais_anio = mes_anio_num %/% 100,
      unidad_conteo = case_when(
        is.na(unidad_conteo) ~ "Sin registros",
        TRUE ~ unidad_conteo)) %>%
    completar_meses(agrupador = c("unidad_conteo", "agrupador")) %>%
    filter(!is.na(mes_anio_num)) %>%
    inner_join(nota_tecnica, by = "agrupador", copy = TRUE) %>%
    # Diferencias entre la frecuencia y la nota técnica
    mutate(
      Frecuencia = case_when(
        is.na(Frecuencia) ~ 0,
        TRUE ~ Frecuencia),
      Suma = case_when(
        is.na(Suma) ~ 0,
        TRUE ~ Suma),
      diff_lim = case_when(
        Frecuencia < frec_mes_min ~ Frecuencia - frec_mes_min,
        Frecuencia > frec_mes_max ~ Frecuencia - frec_mes_max,
        TRUE ~ 0),
      diff_base = Frecuencia - frec_mes,
      frec_efectiva = frec_mes + diff_lim,
      diff_efectiva = Frecuencia - frec_efectiva,
      diff_base_x_cm = diff_base * cm,
      diff_efectiva_x_cm = diff_efectiva * cm,
      diff_lim_x_cm = diff_lim * cm,
      valor_contratado = cm * frec_efectiva,
      valor_ejecutado_cm = cm * Frecuencia,
      diff_facturado = Suma - valor_contratado,
      mes_anio = mes_spanish_juntos(mes_anio_num)
    )

  return(timeseries)

}
