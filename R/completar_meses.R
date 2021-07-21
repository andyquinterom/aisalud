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

#' @title completar meses
#' @description expande una descriptiva por uno o varios agrupadores y separada
#' por el número del mes y el año para llenar vacios. Por ejemplo, si hubo
#' registros en Enero, Febrero y Abril pero no Marzo, esta función creará
#' una fila para Marzo y la completará con ceros.
#' @param data tabla descriptiva por la function descriptiva o una versión 
#' reducida o expandida 
#' @param agrupador agrupador o agrupadores en forma de vector de la descriptiva
#' @param col_anio columna que indica el año de los registros
#' @param col_mes columna del número del més de los registros
#' @param columna_fecha nombre de una columna de tipo fecha
#' @return tabla

completar_meses <- function(
  data, agrupador, col_anio = "ais_anio", col_mes = "ais_mes",
  columna_fecha = NULL) {

  # Si el usuario pasa la variable columna_fecha se generarán las columnas
  # de mes y año
  if (!is.null(columna_fecha)) {
    data <- data %>%
      mutate(ais_mes = month(!!as.name(columna_fecha)),
             ais_anio = year(!!as.name(columna_fecha)))
    col_anio <- "ais_anio"
    col_mes  <- "ais_mes"
  }

  # Si los nombres de las columnas de mes y año no son "ais_mes" y "ais_anio"
  # se cambian a estos nombres
  if (!identical(c("ais_mes", "ais_anio"), c(col_mes, col_anio))) {
    data <- data %>%
      rename(ais_mes = !!rlang::sym(col_mes), ais_anio = !!rlang::sym(col_anio))
  }

  # Se crea columna mes_anio_num cómo identificador único de la combinación
  # de mes y año.
  data <- data %>%
    mutate(mes_anio_num = ais_anio * 100 + ais_mes)

  # Backup de la descriptiva tal cual entra
  data_original <- data

  data <- data %>%
    select(!!!rlang::syms(agrupador), ais_anio, ais_mes, mes_anio_num)

  # Se genera una tabla con las fechas mínimas y máximas
  meses_limites <- data %>%
    select(ais_mes, ais_anio, mes_anio_num) %>%
    ungroup() %>%
    summarise(
      min = min(mes_anio_num),
      max = max(mes_anio_num),
      anio_min = min(ais_anio),
      anio_max = max(ais_anio),
      mes_min = min(ais_mes),
      mes_max = max(ais_mes)
    ) %>%
    collect()

  # Se crea una tabla con cada año y cada mes entre la fecha mímima y máxima
  # de la descriptiva.
  # Se repite cada año 12 veces (una para cada mes del año)
  # Se repite cada mes el número de años
  # Se quedan solo los meses mayores a la fecha mínima y menores a la fecha
  # máxima
  # Esta columna nos permitirá facilmente juntar y rellenar
  meses_completos <- tibble(
    anio = sort(rep(meses_limites$anio_min:meses_limites$anio_max, 12)),
    meses = rep(1:12, meses_limites$anio_max - meses_limites$anio_min + 1)) %>%
    mutate(mes_anio_num = anio * 100 + meses) %>%
    filter(mes_anio_num >= meses_limites$min &
      mes_anio_num <= meses_limites$max) %>%
    select(mes_anio_num) %>%
    mutate(placeholder_key = "key")

  # Tabla dummy para juntar cada agrupador a cada mes y año
  data_to_join <- data %>%
    group_by(!!!rlang::syms(agrupador)) %>%
    summarise(placeholder_key = "key") %>%
    ungroup()

  # Se junta la tabla dummy con la lista de todos los meses y años
  meses_completos <- data_to_join %>%
    full_join(meses_completos, copy = TRUE) %>%
    select(-placeholder_key)

  # Se expande la tabla, los meses que previamente no estaban presentes quedan
  # como NA o "" dependiendo si es numérico o character
  data_completa <- data_original %>%
    full_join(meses_completos)

  return(data_completa)
}
