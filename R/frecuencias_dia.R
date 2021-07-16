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

# Función frecuencias mes
#
# Parametros:
#
# data - una tabla tal cual cargada a Analítica Integrada Salud
#
# agrupador - el agrupador o agrupadores para cálcular las frecuencias
#
# columna_fecha - el nombre de una columna de tipo fecha
#
# columna_suma - la columna sobre la cual se hará el conteo
#
# prestaciones - Bool que define si el conteo se hará por la columna_suma
# o por prestaciones
#
# frec_cantidad - Bool indicando si el conteo de prestaciones se hará contando
# registros o sumando la columna cantidad
#
# Esta función genera una tabla agrupada con el conteo de frecuencias por día

frecuencias_dia <- function(data, agrupador, columna_fecha, columna_suma,
  prestaciones = FALSE, frec_cantidad = FALSE) {

  # Se sacan los nombres de columnas unicos
  agrupador <- unique(agrupador)

  # Se cambia el nombre de columna_fecha a mes_anio_num
  data <- data %>%
    mutate_at(vars(agrupador), as.character) %>%
    mutate(mes_anio_num = !!rlang::sym(columna_fecha))

  # Si las frecuencias no son por prestación, se resumen para hacer un conteo
  # de 1 por cada valor de la columna_suma
  if (!prestaciones) {
    data <- data %>%
      group_by(!!!rlang::syms(unique(c(columna_suma, agrupador)))) %>%
      summarise(mes_anio_num = max(mes_anio_num), cantidad = 1)
  }

  # Se hace el conteo de de los agrupadores
  # Se utiliza la función completar_meses para llenar meses sin datos
  # Los valores NA de convierten a 0
  data <- data %>%
    group_by(!!!rlang::syms(agrupador), mes_anio_num) %>%
    summarise(Frecuencia = ifelse(
      test = prestaciones && frec_cantidad,
      yes = sum(cantidad, na.rm = TRUE),
      no = n())) %>%
    group_by(!!!rlang::syms(agrupador), mes_anio_num) %>%
    mutate(Frecuencia = case_when(
        is.na(Frecuencia) ~ 0,
        TRUE ~ Frecuencia)) %>%
    select(!!!rlang::syms(agrupador), mes_anio_num, Frecuencia) %>%
    group_by(!!!rlang::syms(agrupador)) %>%
    arrange(mes_anio_num)

  return(data)

}
