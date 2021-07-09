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

pie_chart <- function(paquetes, columna, valor_costo, nombre_legend = "") {
  data <- copy(paquetes)[, list(
    TOTAL = sum(get(valor_costo), na.rm = TRUE)), by = columna]

  plot_pie_chart <- plot_ly(
    data = data,
    labels = ~get(columna),
    values = ~TOTAL,
    type = "pie"
  )

  plot_pie_chart <- plot_pie_chart %>%
    config(locale = "es") %>%
    layout(
      xaxis = list(
        showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(
        showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      legend = list(
        orientation = "h", x = 0, y = 0)) %>%
    style(legendgroup = NULL)

  return(
    plot_pie_chart
  )
}
