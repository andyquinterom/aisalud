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

#' @title Convierte listas de notas técnicas a tabla.
#' @description Esta función recorre las listas de las notas técnicas en 
#' estructura JSON  y la almacena en una tabla.
#' @param x Lista de notas técnicas.
#' @return tabla con notas técnicas.


parse_nt <- function(x) {

  purrr::map(x, function(y) {
    poblacion <- y[["poblacion"]]
    agrupadores_names <- names(y[["agrupadores"]])

    purrr::map2(y[["agrupadores"]], agrupadores_names, function(i, w) {
      if (is.null(names(i))) i <- list("n" = i[1], "cm" = i[2])
      i[["cm"]] <- as.double(i[["cm"]])
      i[["n"]]  <- as.double(i[["n"]])
      temp <- data.frame("agrupador" = w, "frec_mes" = as.double(i[["n"]]),
                         "cm" = as.double(i[["cm"]]),
                         "frecuencia_pc" = as.double(i[["n"]] / poblacion),
                         "valor_mes" = as.double(i[["n"]] * i[["cm"]]))
      return(temp)
    }) %>% rbindlist()

  }) %>%
    rbindlist(idcol = "nt")

}
