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

agrupadores_widget <- function(id = NULL, separadores = FALSE, ...) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId = ns("agrupador"),
      label = "Agrupador principal:",
      choices = NULL,
      multiple = FALSE),
    if (separadores) {
      selectizeInput(
        inputId = ns("separadores"),
        label = "Separadores:",
        choices = NULL,
        multiple = TRUE)
    },
    ...,
    checkboxInput(
      inputId = ns("episodios"),
      label = "Agrupador por episodios",
      value = FALSE),
    uiOutput(outputId = ns("episodios_col_rel")),
    tags$div(
      style = "overflow-y: scroll; max-height: 300px;",
      uiOutput(outputId = ns("episodios_jerarquia")))
    )
}
