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

#' @title Año_Mes en números
#' @description Función que recibe caracteres con formato AAAA - nombre mes en
#' español y devuelve AAAAMM en números. Función inversa a *mes_spanish_juntos*
#' @param x vector de fechas en formato AAAA - nombre mes español
#' @return número
#' @examples
#' mes_spanish_inv("2020 - Octubre")
#' 202010


mes_spanish_inv <- function(x) {
  meses <- c(
    "Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "Septiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
  )

  divididos <- str_split(x, " - ")

  divididos_numerico <- purrr::map(
    .x = divididos,
    .f = function(x) {
      as.numeric(x[1]) * 100 + which(x[2] == meses)
    })

  return(unlist(divididos_numerico))


}
