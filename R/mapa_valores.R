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

#' @title Mapa de Colombia con información de contratos
#' @description Consolida la información de contratos y los proyecta en el mapa
#' de Colombia
#' @param indice Tabla con información de los contratos
#' @param departamentos tabla con información georeferencial de Colombia
#' @return objeto con mapa de Colombia 


mapa_valores <- function(indice, departamentos, ...) {

  # Se resume en indice por departamento
  indice_summarise <- indice %>%
    mutate(cod_departamento = as.numeric(cod_departamento)) %>%
    group_by(cod_departamento)

  prestadores <- NULL
  aseguradores <- NULL

  # Si hay prestadores se suman
  if ("nom_prestador" %in% colnames(indice_summarise)) {
    prestadores <- indice_summarise %>%
      summarise(
        prestadores = paste(unique(nom_prestador), collapse = ", "))
  }

  # Si hay aseguradores se suman
  if ("nom_asegurador" %in% colnames(indice_summarise)) {
    aseguradores <- indice_summarise %>%
      summarise(
        aseguradores = paste(unique(nom_asegurador), collapse = ", "))
  }

  # Resumen completo
  valores <- indice_summarise %>%
    summarise(valor_mes = sum(valor_mes, na.rm = TRUE),
              ciudades = paste(unique(ciudades), collapse = ", ")) %>%
    {if (!is.null(prestadores)) {
      left_join(., prestadores)
    } else {.}} %>%
    {if (!is.null(aseguradores)) {
      left_join(., aseguradores)
    } else {.}} %>%
    ungroup()

  # Se juntan datos geograficos con datos de notas tecnicas
  departamentos@data <- departamentos@data %>%
    left_join(valores) %>%
    mutate(valor_mes = replace_na(valor_mes, 0))

  p <- departamentos %>%
    mapView(zcol = "valor_mes", legend = FALSE)

  return(p)

}
