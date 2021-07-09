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

parse_nt_indice <- function(x, tabla_agrupadores) {

  nombres_nt <- names(x)

  indice_datos <- purrr::map2(x, nombres_nt,  function(y, cod_nt) {
    prestador <- y[["prestador"]]
    asegurador <- y[["asegurador"]]

    tibble(
      cod_nt = cod_nt,
      nom_prestador = NA,
      nom_asegurador = NA,
      poblacion = as.double(y[["poblacion"]]),
      departamento = y[["departamento"]],
      ciudades = y[["ciudades"]],
      cod_departamento = y[["cod_departamento"]],
      vigente = ifelse(is.null(y[["vigente"]]),
                               yes = FALSE,
                               no = as.logical(y[["vigente"]]))
    ) %>%
      {if (!is.null(prestador)) {
        mutate(., nom_prestador = y[["prestador"]])
      } else {.}} %>%
      {if (!is.null(asegurador)) {
        mutate(., nom_asegurador = y[["asegurador"]])
      } else {.}}
  }) %>%
    rbindlist(fill = TRUE, use.names = TRUE) %>%
    select(where(~!all(is.na(.x))))

  valores_mes <- tabla_agrupadores %>%
    group_by(nt) %>%
    summarise(valor_mes = sum(valor_mes, na.rm = TRUE))

  indice <- indice_datos %>%
    inner_join(valores_mes, by = c("cod_nt" = "nt")) %>%
    mutate(cod_departamento = as.character(cod_departamento)) %>%
    return()

}
