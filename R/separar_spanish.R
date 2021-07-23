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

#' @title Separador de palabras en español
#' @description Separa vector de palabras usando conectores (",", "y") 
#' @param word_array Vector de palabras
#' @return Variable con texto
#' @examples separar_spanish(c("Tom","Jerry","Andrés"))
#' "Andrés, Jerry y Tom"



separar_spanish <- function(word_array) {
  if (!is.null(word_array)) {
    paste(
      lapply(
        X = length(word_array):1,
        word_array = word_array,
        function(i, word_array) {
          if (i == 2) {
            return(
              paste0(word_array[i], " y ")
            )
          } else if (i == 1) {
            return(word_array[i])
          } else {
            return(
              paste0(word_array[i], ", ")
            )
          }
        }
      ),
      collapse = ""
    )
  }
}
