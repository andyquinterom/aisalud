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

#' @title Esquema final nota técnica.
#' @description Se define el esquema final que el usuario final tendrá como nota
#' técnica.
#' @param nota_tecnica Lista con esquema de nota técnica
#' @return Lista con nota técnica final

debloat_nt <- function(nota_tecnica) {

  nt2 <- list()

  agrupadores <- nota_tecnica$agrupadores

  agrupadores <- purrr::map(agrupadores, function(agrup) {
    return(list(n = agrup$n, cm = agrup$cm))
  })

  if (!is.null(nota_tecnica$perfil)) nt2$perfil <- nota_tecnica$perfil
  nt2$poblacion <- nota_tecnica$poblacion
  if (is.null(nota_tecnica$poblacion)) nt2$poblacion <- 1
  nt2$departamento <- "Bogotá D.C"
  nt2$cod_departamento <- 11
  nt2$vigente <- FALSE
  nt2$ciudades <- "Bogotá D.C"
  nt2$agrupadores <- agrupadores

  return(nt2)

}
