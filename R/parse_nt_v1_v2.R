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

#' @title Convierte notas técnicas de V1 a V2
#' @description Recorre las listas de notas técnicas en estructura JSON V1 y las
#' convierte en notas técnicas en estrcutura JSON v2
#' @param x Lista con notas técnicas
#' @return lista de caracteres con notas técnicas

parse_nt_v1_v2 <- function(x) {
  notas_tecnicas_v2 <- purrr::map(x, function(y) {
    if (!is.null(y[["vigente"]]))  y[["vigente"]] <- as.logical(y[["vigente"]])
    agrupadores_names <- names(y[["agrupadores"]])
    agrupadores <- purrr::map2(
      y[["agrupadores"]],
      agrupadores_names,
      function(i, w) {
        if (is.null(names(i))) i <- list("n" = i[1], "cm" = i[2])
        return(i)
      }
    )
    y[["agrupadores"]] <- agrupadores
    return(y)
  })
  return(
    as.character(
      toJSON(notas_tecnicas_v2, pretty = TRUE, auto_unbox = TRUE)
    )
  )
}
