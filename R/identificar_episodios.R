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

#' @title Identificar episodios
#' @description Tabla con cada uno de los valores únicos de columna_suma junto a 
#' su agrupador respetando la jerarquía indicada.
#' @param data tabla cargada a analítica integrada
#' @param agrupador el agrupador de la jerarquía
#' @param columna_suma la columna que relaciona las prestaciones dentro de los 
#' episodios
#' @param jerarquia es un vector ordenado con la jerarquia deseada para los 
#' agrupadores
#' @return tabla

identificar_episodios <- function(data, agrupador, columna_suma, jerarquia) {
  
  index_episodios <- data.frame(
    index = 1:length(jerarquia),
    agrupador = jerarquia
  )
  colnames(index_episodios) <- c("index", agrupador)
  
  episodios <- data %>%
    select(!!as.name(columna_suma), !!as.name(agrupador)) %>%
    filter(!!as.name(agrupador) %in% jerarquia) %>%
    distinct() %>%
    right_join(index_episodios, copy = TRUE) %>%
    group_by(!!as.name(columna_suma)) %>%
    window_order(index) %>%
    mutate(!!agrupador := first(!!as.name(agrupador))) %>%
    ungroup() %>%
    distinct(!!as.name(columna_suma), !!as.name(agrupador))
 
  return(episodios) 
  
}