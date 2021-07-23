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

#' @title Gráfico de barras
#' @description Gráfico de barras para realizar comparaciones entre agrupadores
#' @param data tabla descriptiva
#' @param columna_numeros variable a comparar en el gráfico
#' @param columna_sep variable de segmentación 
#' @return Gráfico de barras
#' @examples
#' grafico_barras_descriptiva(
#'  data = episodios$tabla[["descriptiva"]],
#'  columna_numeros = input$grafico_barras_indicador,
#'  columnas_sep = episodios$lista_agrupadores[
#'  input$barras_agrupador_rows_selected])


grafico_barras_descriptiva <- function(data, columna_numeros, columnas_sep) {

  bar_plot <- NULL

  lapply(
    X = seq_len(nrow(columnas_sep)),
    FUN = function(i) {
      data_temp <- merge.data.table(
        x = columnas_sep[i],
        y = data
      )[[columna_numeros]]
      if (i == 1) {
        bar_plot <<- plot_ly(type = "bar", y = data_temp, x = paste(
          unlist(columnas_sep[i]), collapse = "\n"
        ))
      } else {
        bar_plot <<- bar_plot %>%
          add_trace(y = data_temp, x = paste(
            unlist(columnas_sep[i]), collapse = "\n"
          ))
      }
    }
  )

  yaxis <- c(
    "Suma de valor", "Frecuencia", "Media", "Mediana",
    "Coeficiente de variación")[c(
      "Suma",
      "Frecuencia",
      "Media",
      "P50",
      "Coef.var") == columna_numeros] %>%
    unname()

  return(bar_plot %>%
           config(locale = "es") %>%
           layout(xaxis = list(title = "Agrupador"),
                  yaxis = list(title = yaxis,
                               tickformat = ",.2f"),
                  showlegend = FALSE))

}
