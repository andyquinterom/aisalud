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

episodios_jerarquia <- function(data, columnas, columna_valor, columna_suma,
                                columna_sep, nivel_1, nivel_2, nivel_3,
                                nivel_4, columna_fecha = NULL,
                                frec_cantidad = FALSE) {

  data <- data %>%
    mutate(ASIGNACION_NIVEL = "")

  episodios_nivel_1 <- data.table()
  episodios_nivel_2 <- data.table()
  episodios_nivel_3 <- data.table()
  episodios_nivel_4 <- data.table()

  data_episodios <- NULL
  if (!(is.null(nivel_1) || is.na(nivel_1))) {

    episodios <- identificar_episodios(
      data = data, 
      agrupador = columnas, 
      columna_suma = columna_suma, 
      jerarquia = nivel_1)
    
    data_episodios <- data %>%
      {if (!is.null(columna_fecha)) {
        group_by(., !!as.name(columna_suma)) %>%
        mutate(!!columna_fecha := max(!!as.name(columna_fecha)))
      } else {.}} %>%
      {if (!is.null(columna_sep)) {
        group_by(., !!!rlang::syms(unique(c(columna_suma, columna_sep))))
      } else {
        group_by(., !!as.name(columna_suma))
      }}

    data_episodios <- data_episodios %>%
      summarise(
        valor_calculos = sum(!!as.name(columna_valor), na.rm = TRUE)) %>%
      right_join(episodios)
    data_temp <- descriptiva(
      data = data_episodios,
      columnas = c(columnas, columna_sep),
      columna_valor = "valor_calculos",
      columna_suma = columna_suma,
      prestaciones = FALSE
    )
    episodios_nivel_1 <- data_temp[["descriptiva"]] %>%
      mutate(unidad_conteo = "Episodio")
    episodios <- episodios %>%
      select(!!as.name(columna_suma))
    data <- data %>%
      anti_join(episodios)
    data_temp <- NULL
  }

  episodios_nivel_2_data <- NULL
  if (!(is.null(nivel_2) || is.na(nivel_2))) {
    data_temp <- descriptiva(
      data = data,
      columnas = c(columnas, columna_sep),
      columna_valor = columna_valor,
      columna_suma = "nro_factura",
      prestaciones = FALSE
    )
    episodios_nivel_2 <- data_temp[["descriptiva"]] %>%
      filter(!!as.name(columnas) %in% nivel_2)
    episodios_nivel_2_data <- data_temp[["data"]] %>%
      filter(!!as.name(columnas) %in% nivel_2)
    data_temp <- NULL
  }

  episodios_nivel_3_data <- NULL
  if (!(is.null(nivel_3) || is.na(nivel_3))) {
    data_temp <- descriptiva(
      data = data,
      columnas = c(columnas, columna_sep),
      columna_valor = columna_valor,
      columna_suma = "nro_identificacion",
      prestaciones = FALSE
    )
    episodios_nivel_3 <- data_temp[["descriptiva"]] %>%
      filter(!!as.name(columnas) %in% nivel_3)
    episodios_nivel_3_data <- data_temp[["data"]] %>%
      filter(!!as.name(columnas) %in% nivel_3)
    data_temp <- NULL
  }

  episodios_nivel_4_data <- NULL
  if (!(is.null(nivel_4) || is.na(nivel_4))) {
    data_temp <- descriptiva(
      data = data,
      columnas = c(columnas, columna_sep),
      columna_valor = columna_valor,
      columna_suma = "",
      prestaciones = TRUE,
      frec_cantidad = frec_cantidad
    )
    episodios_nivel_4 <- data_temp[["descriptiva"]] %>%
      filter(!!as.name(columnas) %in% nivel_4)
    episodios_nivel_4_data <- data_temp[["data"]] %>%
      filter(!!as.name(columnas) %in% nivel_4)
    data_temp <- NULL
  }

  return(
    list(
      "descriptiva" = rbind(
        episodios_nivel_1,
        episodios_nivel_2,
        episodios_nivel_3,
        episodios_nivel_4
      ),
      "data" = list(
        "episodios" = data_episodios,
        "facturas" = episodios_nivel_2_data,
        "pacientes" = episodios_nivel_3_data,
        "prestaciones" = episodios_nivel_4_data
      )
    )
  )
}
