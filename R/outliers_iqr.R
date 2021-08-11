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

#' @title Calculo de outliers con n vecces el iqr
#' @description Calcula los outliers de una columna usando el iqr de 
#' la columna_valor. El outlier se calcula dependiendo del parámetro 
#' multiplicativo
#' @param data tabla con información a analizar
#' @param columna carácter con la columna a realizar analisis de percentil
#' @param columna_valor carácter especificando la columna númerica
#' @param multiplicativo Número de veces por valor IQR que está alejado de los 
#' cuartiles 1 y 3 (Q1 o Q3).
#' @param frecuencia número de veces que se repite el valor columna
#' @param frec_cantidad boolean para escoger si realizar analisis por frecuencia
#' o cantidad
#' @return tabla resumen con outliers
#' @examples
#' outliers_iqr(
#'  data = iris,
#'  columna = "Species",
#'  columna_valor = "Sepal.Length",
#'  percentil = .5,
#'  frecuencia = 1)


outliers_iqr <- function(data, columna, columna_valor, multiplicativo,
                        frecuencia = 1, frec_cantidad = FALSE) {

  multiplicativo <- as.numeric(multiplicativo)

  data <- data %>%
    mutate(valor_calculos = as.numeric(!!as.name(columna_valor)))

  valor_total <- data %>%
    transmute(valor_total = sum(valor_calculos, na.rm = TRUE)) %>%
    distinct() %>%
    collect() %>%
    unlist() %>%
    as.numeric()

  limites <- data %>%
    group_by(!!as.name(columna)) %>%
    summarise(valor_calculos = sum(valor_calculos, na.rm = TRUE)) %>%
    select(valor_calculos) %>%
    collect() %>%
    transmute(
      lower = quantile(valor_calculos, 0.25, na.rm = TRUE) -
        multiplicativo * IQR(valor_calculos, na.rm = TRUE),
      upper = quantile(valor_calculos, 0.75, na.rm = TRUE) +
        multiplicativo * IQR(valor_calculos, na.rm = TRUE)
    ) %>%
    distinct() %>%
    collect()

  upper <- limites[["upper"]] %>% unname() %>% as.numeric()
  lower <- limites[["lower"]] %>% unname() %>% as.numeric()

  outliers <- data %>%
    group_by(!!as.name(columna)) %>%
    summarise(valor_calculos = sum(valor_calculos, na.rm = TRUE),
              frec = ifelse(
                test = frec_cantidad,
                yes = sum(cantidad, na.rm = TRUE),
                no = n())) %>%
    mutate(porcentaje = paste0(
      round(100 * valor_calculos / valor_total, digits = 2),
      "%")) %>%
    filter(frec >= frecuencia) %>%
    filter(valor_calculos >= upper | valor_calculos <= lower) %>%
    arrange(desc(valor_calculos)) %>%
    collect() %>%
    as.data.table()

  return(outliers)

}
