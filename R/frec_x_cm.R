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

#' @title Cálculo de valor multiplicativo frecuencia por costo medio en una 
#' serie de tiempo.
#' @description Calcula la multiplicación de la frecuencia por el costo medio de
#' la nota técnica en la serie de tiempo
#' @param timeseries tabla con serie de tiempo unidad de conteo y agrupador
#' @param nota_tecnica tabla con nota_tecnica
#' @param agrupador agrupador para calcular la frecuencia
#' @return tabla serie de tiempo donde la columna suma es la multiplicación de 
#' la frecuencia por el costo medio de la nota técnica.
#' @example frec_x_cm(nota_tecnica$timeseries,
#'  nota_tecnica = nota_tecnica$parsed,
#'  agrupador = episodios$agrupador)

frec_x_cm <- function(timeseries, nota_tecnica, agrupador) {

  by_vector <- "agrupador"
  names(by_vector) <- agrupador
  nota_tecnica <- nota_tecnica %>%
    select(agrupador, cm)

  
  frec <- timeseries %>%
    inner_join(nota_tecnica, by = by_vector) %>%
    mutate(Suma = Frecuencia * cm) %>%
    ungroup()

  return(frec)

}
