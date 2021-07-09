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

comparar_cajas_jerarquia <- function(
  ns,
  items_nivel_1 = NULL,
  items_nivel_2 = NULL,
  items_nivel_3 = NULL,
  items_nivel_4 = NULL) {
  return(
    tags$div(
      class = "comparar_jerarquia_row",
      box(
        width = 3,
        orderInput(
          inputId = ns("comparar_jerarquia_nivel_1"),
          label = actionLink(ns("seleccionar_episodio"), label = "Episodio"),
          items = items_nivel_1,
          width = "100%",
          height = "100%",
          connect = c(
            ns("comparar_jerarquia_nivel_2"),
            ns("comparar_jerarquia_nivel_3"),
            ns("comparar_jerarquia_nivel_4")))
      ),
      box(
        width = 3,
        orderInput(
          inputId = ns("comparar_jerarquia_nivel_2"),
          label = actionLink(ns("seleccionar_factura"), label = "Factura"),
          items = items_nivel_2,
          width = "100%",
          height = "100%",
          connect = c(
            ns("comparar_jerarquia_nivel_1"),
            ns("comparar_jerarquia_nivel_3"),
            ns("comparar_jerarquia_nivel_4")))
      ),
      box(
        width = 3,
        orderInput(
          inputId = ns("comparar_jerarquia_nivel_3"),
          label = actionLink(ns("seleccionar_paciente"), label = "Paciente"),
          items = items_nivel_3,
          width = "100%",
          height = "100%",
          connect = c(
            ns("comparar_jerarquia_nivel_1"),
            ns("comparar_jerarquia_nivel_2"),
            ns("comparar_jerarquia_nivel_4")))
      ),
      box(
        width = 3,
        orderInput(
          inputId = ns("comparar_jerarquia_nivel_4"),
          label = actionLink(ns("seleccionar_prestacion"),
            label = "Prestación"),
          items = items_nivel_4,
          width = "100%",
          height = "100%",
          connect = c(
            ns("comparar_jerarquia_nivel_1"),
            ns("comparar_jerarquia_nivel_2"),
            ns("comparar_jerarquia_nivel_3")))
      )
    )
  )
}
