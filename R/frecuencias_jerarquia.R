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

#' @title Consolida tablas de frecuencias por jerarquía
#' @description Consolida tablas de frecuencias agrupadas por episodios y 
#' determinada por jerarquía 
#' @param data tabla cargada a Análitica integrada
#' @param columnas Agrupador o agrupadores para calcular la frecuencia
#' @param columna_suma Columna sobre la cual se realiza el conteo relacionando 
#' los episodios seleccionados.
#' @param columna_fecha Columna con fecha de prestación
#' @param nivel_1 Columna(s) asignada(s) al nivel 1 en la jerarquía para el 
#' calculo de frecuencias. El orden del nivel_1 define la jerarquía de los 
#' episodios
#' @param nivel_2 Columna(s) asignada(s) al nivel 2 en la jerarquía para el 
#' calculo de frecuencias
#' @param nivel_3 Columna(s) asignada(s) al nivel 3 en la jerarquía para el 
#' calculo de frecuencias
#' @param nivel_4 Columna(s) asignada(s) al nivel 4 en la jerarquía para el 
#' calculo de frecuencias
#' @param intervalo periocidad diaria ("dia") o mensual ("mes").
#' @param frec_cantidad boolean que define si es conteo de prestaciones se hará
#' contando registros o sumando la columna *cantidad*
#' @return lista de tablas con frecuencias de niveles


frecuencias_jerarquia <- function(data, columnas, columna_suma, columna_fecha,
                                columna_sep, nivel_1, nivel_2, nivel_3,
                                nivel_4, intervalo = "mes",
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
    
    if (!is.null(columna_sep)) {
      data_episodios <- data %>%
        group_by(!!!rlang::syms(unique(c(
          columna_sep, columna_suma, columna_fecha)))) %>%
        count()
    } else {
      data_episodios <- data %>%
        group_by(!!!rlang::syms(unique(c(columna_suma, columna_fecha)))) %>%
        count()
    }

    data_episodios <- data_episodios %>%
      right_join(episodios)

    data_temp <- frecuencias(
      data = data_episodios,
      agrupador = c(columnas, columna_sep),
      columna_suma = columna_suma,
      prestaciones = FALSE,
      columna_fecha = columna_fecha,
      intervalo = intervalo) %>% 
      mutate(unidad_conteo = "Episodio")
    
    
    episodios_nivel_1 <- data_temp
    episodios <- episodios %>%
      select(!!as.name(columna_suma))
    data <- data %>%
      anti_join(episodios)
    data_temp <- NULL
    print("Nivel 1: Completo.")
  }

  episodios_nivel_2_data <- NULL
  if (!(is.null(nivel_2) || is.na(nivel_2))) {
    data_temp <- frecuencias(
      data = data,
      agrupador = c(columnas, columna_sep),
      columna_fecha = columna_fecha,
      columna_suma = "nro_factura",
      prestaciones = FALSE,
      intervalo = intervalo
    )
    episodios_nivel_2 <- data_temp %>%
      filter(!!as.name(columnas) %in% nivel_2)
    data_temp <- NULL
    print("Nivel 2: Completo.")

  }

  episodios_nivel_3_data <- NULL
  if (!(is.null(nivel_3) || is.na(nivel_3))) {
    data_temp <- frecuencias(
      data = data,
      agrupador = c(columnas, columna_sep),
      columna_fecha = columna_fecha,
      columna_suma = "nro_identificacion",
      prestaciones = FALSE,
      intervalo = intervalo
    )
    episodios_nivel_3 <- data_temp %>%
      filter(!!as.name(columnas) %in% nivel_3)
    data_temp <- NULL
    print("Nivel 3: Completo.")

  }

  episodios_nivel_4_data <- NULL
  if (!(is.null(nivel_4) || is.na(nivel_4))) {
    print("Nivel 4: Generando descriptiva.")
    data_temp <- frecuencias(
      data = data,
      agrupador = c(columnas, columna_sep),
      columna_fecha = columna_fecha,
      columna_suma = "nro_identificacion",
      prestaciones = TRUE,
      frec_cantidad = frec_cantidad,
      intervalo = intervalo
    )
    print("Nivel 4: Descriptiva generada.")
    episodios_nivel_4 <- data_temp %>%
      filter(!!as.name(columnas) %in% nivel_4)
    data_temp <- NULL
    print("Nivel 4: Completo.")

  }

  return(
    list(
      "descriptiva" = rbindlist(
        list(
          episodios_nivel_1,
          episodios_nivel_2,
          episodios_nivel_3,
          episodios_nivel_4
        ),
        fill = TRUE
      )
    )
  )

}
