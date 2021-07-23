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

frecuencias <- function(
  data, agrupador, columna_fecha, columna_suma,
  intervalo = "mes", prestaciones = FALSE, frec_cantidad = FALSE) {

  agrupador <- unique(agrupador)

  unidad <- "Episodio"
  if (columna_suma == "nro_factura") unidad <- "Factura"
  if (columna_suma == "nro_identificacion") unidad <- "Paciente"
  if (prestaciones) unidad <- "Prestación"

  data <- data %>%
    mutate_at(vars(agrupador), as.character)

  if (intervalo == "dia") {
    data <- frecuencias_dia(
      data = data,
      agrupador = agrupador,
      columna_fecha = columna_fecha,
      columna_suma = columna_suma,
      prestaciones = prestaciones,
      frec_cantidad = frec_cantidad)
    }

  if (intervalo == "mes") {
    data <- frecuencias_mes(
      data = data,
      agrupador = agrupador,
      columna_fecha = columna_fecha,
      columna_suma = columna_suma,
      prestaciones = prestaciones,
      frec_cantidad = frec_cantidad)
  }

  data <- data %>%
    mutate("unidad_conteo" = unidad) %>%
    relocate(unidad_conteo, !!!rlang::syms(agrupador)) %>%
    dplyr::collect()

  return(data)
}
