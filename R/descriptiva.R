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

#' @title Información descriptiva
#' @description Calcula información descriptiva sobre el valor de las columnas 
#' seleccionadas.
#' @param data Tabla cargada a Análitica integrada.
#' @param columnas Agrupador o agrupadores para realizar el análisis.
#' descriptivo.
#' @param columna_valor Columna númerica que contiene valores de calculo.
#' @param columna_suma Columna referente a la unidad de conteo de los 
#' agrupadores.
#' @param prestaciones Boolean que define la frecuencia por prestación.
#' @param frec_cantidad Boolean que define si la frecuencia se realiza haciendo 
#' el conteo de registros o  sumando la columna cantidad.
#' @return Lista con tabla descriptiva y tabla resumen con la suma de la columna
#' *columna_valor* agrupada por *columa_suma* y *columnas*



descriptiva <- function(data, columnas, columna_valor, columna_suma,
                        prestaciones = FALSE, frec_cantidad = FALSE) {
  
  unidad <- "Episodio"
  if (prestaciones) unidad <- "Prestación"
  if (columna_suma == "nro_factura") unidad <- "Factura"
  if (columna_suma == "nro_identificacion") unidad <- "Paciente"

  columnas <- unique(columnas)
  data <- data %>%
    mutate(valor_calculos = as.numeric(!!as.name(columna_valor)))

  if (!prestaciones) {
    data <- data %>%
      group_by(!!!rlang::syms(unique(c(columna_suma, columnas)))) %>%
      summarise(valor_calculos = sum(valor_calculos, na.rm = TRUE),
                cantidad = 1)
  }
  if ("data.frame" %in% class(data)) {
    data_descriptiva <- data %>%
      group_by(!!!rlang::syms(columnas)) %>%
      summarise(
        "Frecuencia" = ifelse(
          test = prestaciones && frec_cantidad,
          yes = sum(cantidad, na.rm = TRUE),
          no = n()),
        "Suma" = sum(valor_calculos, na.rm = TRUE),
        "Media" = NA,
        "P25" = round(quantile(valor_calculos, probs = 0.25, na.rm = TRUE), 2),
        "P50" = round(quantile(valor_calculos, probs = 0.5, na.rm = TRUE), 2),
        "P75" = round(quantile(valor_calculos, probs = 0.75, na.rm = TRUE), 2),
        "P90" = round(quantile(valor_calculos, probs = 0.9, na.rm = TRUE), 2),
        "Desv.tipica" = round(sd(valor_calculos, na.rm = TRUE), 2),
        "Coef.var" = round(na_if(sd(valor_calculos, na.rm = TRUE), 0) /
                             na_if(mean(valor_calculos, na.rm = TRUE), 0), 2),
        "Min." = min(valor_calculos, na.rm = TRUE),
        "Max." = max(valor_calculos, na.rm = TRUE),
        "Rango" = max(valor_calculos, na.rm = TRUE) -
          min(valor_calculos, na.rm = TRUE)
      )
  } else {
    data_descriptiva <- data %>%
      group_by(!!!rlang::syms(columnas)) %>%
      summarise(
        "Frecuencia" = ifelse(
          test = prestaciones && frec_cantidad,
          yes = sum(cantidad, na.rm = TRUE),
          no = n()),
        "Suma" = sum(valor_calculos, na.rm = TRUE),
        "Media" = NA,
        "P25" = round(quantile(valor_calculos, probs = 0.25), 2),
        "P50" = round(quantile(valor_calculos, probs = 0.5), 2),
        "P75" = round(quantile(valor_calculos, probs = 0.75), 2),
        "P90" = round(quantile(valor_calculos, probs = 0.9), 2),
        "Desv.tipica" = round(sd(valor_calculos, na.rm = TRUE), 2),
        "Coef.var" = round(na_if(sd(valor_calculos, na.rm = TRUE), 0) /
                             na_if(mean(valor_calculos, na.rm = TRUE), 0), 2),
        "Min." = min(valor_calculos, na.rm = TRUE),
        "Max." = max(valor_calculos, na.rm = TRUE),
        "Rango" = max(valor_calculos, na.rm = TRUE) -
          min(valor_calculos, na.rm = TRUE)
      )
  }

  data_descriptiva <- data_descriptiva %>%
    mutate(Media = round(Suma / na_if(Frecuencia, 0), 2))
  fields <- data_descriptiva %>%
    colnames()

  if (".add" %in% fields) {
    data_descriptiva <- data_descriptiva %>%
      select(-`.add`)
  }
  data_descriptiva <- data_descriptiva %>%
    mutate("unidad_conteo" = unidad) %>%
    relocate(unidad_conteo) %>%
    collect()
  setDT(data_descriptiva)

  setnames(
    data_descriptiva,
    c("unidad_conteo",
      columnas,
      "Frecuencia",
      "Suma", "Media",
      "P25",
      "P50",
      "P75",
      "P90",
      "Desv.tipica",
      "Coef.var",
      "Min.",
      "Max.",
      "Rango")
  )

  return(list(
    "descriptiva" = data_descriptiva,
    "data" = data
  ))
}
