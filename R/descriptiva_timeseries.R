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

descriptiva_timeseries <- function(
  data, agrupador, col_anio = "ais_anio", col_mes = "ais_mes") {

  if (nrow(data) == 0) stop("Data.frame vacio")

  if (!identical(c("ais_mes", "ais_anio"), c(col_mes, col_anio))) {
    data <- data %>%
      rename(ais_mes = !!rlang::sym(col_mes), ais_anio = !!rlang::sym(ais_anio))
  }

  unidades_conteo <- data %>%
    group_by(!!!rlang::syms(agrupador)) %>%
    summarise(unidad_conteo = first(unidad_conteo))

  data <- data %>%
    select(-unidad_conteo) %>%
    completar_meses(agrupador = agrupador) %>%
    group_by(!!!rlang::syms(agrupador), mes_anio_num) %>%
    mutate(across(.cols = everything(), .fns = function(x) {
      case_when(
        is.na(x) ~ 0,
        TRUE ~ x
      )
    })) %>%
    mutate(
      ais_anio = mes_anio_num %/% 100,
      ais_mes  = mes_anio_num %% 100
    ) %>%
    left_join(unidades_conteo) %>%
    relocate(unidad_conteo) %>%
    collect()

  return(data)

}
