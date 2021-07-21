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

#' @title Contador independiente.
#' @description Crea un contador independiente debido a que incrementa su propio
#' valor en 1.
#' @return Función que al ejecutarse aumenta su estado interno por 1.
#' @examples
#' Variable <- counter()
#' print(Variable())
#' 1
#' print(Variable())
#' 2

counter <- function() {
  starting_val <- 0
  counter_fn <- function() {
    starting_val <<- starting_val + 1
  }
}
