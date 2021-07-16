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

caja_de_bigotes_agrupador <- function(data, columnas_sep) {
  data <- copy(data)
  options(warn = -1)
  box_plot <- plot_ly(type = "box")
  data <- data %>%
    mutate(
      upper_fence = P50 + 1.5 * (P75 - P25),
      lower_fence = P50 - 1.5 * (P75 - P25)
    )
  lapply(
    X = 1:nrow(columnas_sep),
    FUN = function(i) {
      data_temp <- merge.data.table(
        x = columnas_sep[i],
        y = data)
      box_plot <<- box_plot %>%
        add_trace(
          y = paste(unlist(columnas_sep[i]), collapse = "\n"),
          q1 = data_temp[["P25"]],
          median = data_temp[["P50"]],
          q3 = data_temp[["P75"]],
          lowerfence = data_temp[["lower_fence"]],
          upperfence = data_temp[["upper_fence"]],
          mean = data_temp[["Media"]],
          sd = data_temp[["Desv.tipica"]],
          name = paste(unlist(columnas_sep[i]), collapse = "\n")
          )

    }
  )
  return(box_plot %>%
           config(locale = "es") %>%
           layout(yaxis = list(title = "Agrupador"),
                  xaxis = list(title = "Valor",
                               tickformat = ",.2f",
                               type = "linear"),
                  showlegend = FALSE))
  options(warn = 0)
}
