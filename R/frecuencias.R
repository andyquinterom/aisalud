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

#' @title Tabla de frecuencias por agrupador
#' @description Calcula tabla de frecuencias para diferentes agrupadores por 
#' diferentes unidades de conteo.
#' @param data tabla cargada a Análitica integrada
#' @param agrupador Agrupador o agrupadores para calcular la frecuencia
#' @param columna_fecha Nombre columna de tipo fecha
#' @param columna_suma Columna sobre la cual se realiza el conteo 
#' @param intervalo periocidad diaria ("dia") o mensual ("mes").
#' @param prestaciones boolean que define el conteo por prestación
#' @param frec_cantidad boolean que define si es conteo de prestaciones se hará
#' contando registros o sumando la columna *cantidad*
#' @return Tabla con frecuencias
#' @example frecuencias(
#'  data = data_episodios,
#'  agrupador = c(columnas, columna_sep),
#'  columna_suma = columna_suma,
#'  prestaciones = FALSE,
#'  columna_fecha = columna_fecha,
#'  intervalo = intervalo)


frecuencias <- function(
  data, agrupador, columna_fecha, columna_suma,
  intervalo = "mes", prestaciones = FALSE, frec_cantidad = FALSE) {

  agrupador <- unique(agrupador)

  unidad <- "Episodio"
  if (prestaciones) unidad <- "Prestación"
  if (columna_suma == "nro_factura") unidad <- "Factura"
  if (columna_suma == "nro_identificacion") unidad <- "Paciente"

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

  data <- collect(data)

  means_sums <- data %>%
    summarise(
      frec_media = round(mean(Frecuencia, na.rm = TRUE), digits = 3),
      frec_suma = round(sum(Frecuencia, na.rm = TRUE), digits = 3)
    )

  data <- data %>%
    pivot_wider(
      names_from = mes_anio_num,
      values_from = Frecuencia)

  if (intervalo == "mes") {
    data <- data %>%
      rename_with(mes_spanish_juntos, .cols = -seq_len(length(agrupador)))
  }

# Queda comentada esta sección hastas que quede actualizado dbplyr
# https://github.com/tidyverse/dbplyr/pull/676
#  if (intervalo == "semana") {
#    data <- data %>%
#      rename_with(function(x) {
#        paste(substr(x, 1, 4), substr(x, 5, 6), sep = " - ")
#      }, .cols = -seq_len(length(agrupador)))
#  }

  data <- data %>%
    left_join(means_sums) %>%
    mutate("unidad_conteo" = unidad) %>%
    relocate(unidad_conteo, !!!rlang::syms(agrupador), frec_suma, frec_media)

  return(data)
}
