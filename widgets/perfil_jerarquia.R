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

perfil_jerarquia <- function(perfiles, perfil_select, items,
                             funcion_jerarquia, ns) {

  perfiles_seleccionado <- perfiles[[perfil_select]][["jerarquia"]]

  items_nivel_1 <- perfiles_seleccionado[["episodio"]]
  items_nivel_1 <- items_nivel_1[items_nivel_1 %in% items]

  items_nivel_2 <- perfiles_seleccionado[["factura"]]
  items_nivel_2 <- items_nivel_2[items_nivel_2 %in% items]

  items_nivel_3 <- perfiles_seleccionado[["paciente"]]
  items_nivel_3 <- items_nivel_3[items_nivel_3 %in% items]

  items_nivel_4 <- setdiff(
    x = items,
    y = c(items_nivel_1, items_nivel_2, items_nivel_3))

  if (identical(items_nivel_4, character(0))) {
    items_nivel_4 <- NULL
  }

  funcion_jerarquia(
    ns = ns,
    items_nivel_1 = items_nivel_1,
    items_nivel_2 = items_nivel_2,
    items_nivel_3 = items_nivel_3,
    items_nivel_4 = items_nivel_4
  )

}
