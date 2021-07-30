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

parse_paquetes <- function(data_list) {

  paquetes <- data_list[["paquetes"]]
  prestaciones <- data_list[["prestaciones"]]
  tabla_prestaciones <- data.frame()

  tabla_paquetes <- purrr::map(
    .x = paquetes,
    .f = function(paq) {
      purrr::map(
        .x = paq[["componentes"]],
        .f = function(prestacion) {
          prestacion[["cantidad"]] <- as.double(prestacion[["cantidad"]])
          purrr::map(
            .x = c(
              "descripcion",
              "tipo_costo",
              "valor_unitario",
              "costo_unitario"
            ),
            .f = ~ if (is.null(prestacion[[.x]])) prestacion[[.x]] <<- NA
          )
          data.frame(
            descripcion_prestacion = prestacion[["descripcion"]],
            tipo_costo = prestacion[["tipo_costo"]],
            valor_unitario = prestacion[["valor_unitario"]],
            costo_unitario = prestacion[["costo_unitario"]]
          )
        }
      ) %>%
        rbindlist(idcol = "codigo_prestacion") %>%
        mutate(
          codigo_paquete = paq[["codigo"]],
          descripcion_prestacion = as.character(descripcion_prestacion),
          valor_unitario = as.numeric(valor_unitario),
          costo_unitario = as.numeric(costo_unitario)
        )
    }
  ) %>%
    rbindlist(idcol = "paquete")

  if (length(prestaciones) > 0) {
    tabla_prestaciones <- purrr::map(
      .x = prestaciones,
      .f = function(prestacion) {
        data.frame(
          descripcion_prestacion_ref = prestacion[["descripcion"]],
          tipo_costo_ref = prestacion[["tipo_costo"]],
          valor_unitario_ref = prestacion[["valor_unitario"]],
          costo_unitario_ref = prestacion[["costo_unitario"]]
        )
      }
    ) %>%
      rbindlist(idcol = "codigo_prestacion")

    tabla_paquetes <- tabla_paquetes %>%
      left_join(tabla_prestaciones)

    purrr::map(
      .x = c(
        "descripcion_prestacion",
        "tipo_costo",
        "valor_unitario",
        "costo_unitario"
      ),
      .f = function(x) {
        tabla_paquetes <<- tabla_paquetes %>%
          mutate(
            !!x := case_when(
              is.na(!!rlang::sym(x)) ~ !!rlang::sym(paste0(x, "_ref")),
              TRUE ~ !!rlang::sym(x)
            )
          ) %>%
          select(-!!rlang::sym(paste0(x, "_ref")))
      }
    )
  }

  indice_paquetes <- purrr::map(
    .x = paquetes,
    .f = function(paq) {
      data.frame(
        codigo_paquete = paq[["codigo"]],
        cups_paquete = paq[["cups_paquete"]],
        descripcion_paquete = paq[["descripcion"]],
        especialidad = paq[["especialidad"]],
        servicio = paq[["servicio"]],
        valor_paquete = paq[["valor_paquete"]],
        costo_paquete = paq[["costo_paquete"]]
      )
    }
  ) %>%
    rbindlist(idcol = "paquete")

  return(
    list(
      paquetes = tabla_paquetes,
      prestaciones = tabla_prestaciones,
      indice_paquetes = indice_paquetes
    )
  )

}
