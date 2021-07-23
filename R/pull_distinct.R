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

#' @title Valores únicos de columna seleccionada
#' @description Identifica los valores únicos de la columna seleccionada y las devuelve desde 
#' la base de datos.
#' @param data Tabla
#' @param col columna
#' @return tabla con valores únicos
#' @examples
#' pull_distinct(mtcars,"gear")


pull_distinct <- function(data, col) {
  data %>%
    select(!!rlang::sym(col)) %>%
    distinct() %>%
    pull(!!rlang::sym(col))
}
