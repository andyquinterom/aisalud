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

#' @title Datos composición
#' @description Resume la participación de la *columna_explorar* dentro del
#' episodio agrupador
#' @param data Tabla ingresada a Análitica integral
#' @param columna_episodios Agrupador a realizar análisis
#' @param columna_valor Columna numerica que contiene los valores de calculo
#' @param columna_suma Columna referente a la unidad de conteo de los 
#' agrupadores
#' @param columna_explorar Nombre de columna a realizar composición dentro 
#' del episodio
#' @param prioridad Nombre del episodio que tiene prioridad sobre los otros
#' servicios dentro del agrupador
#' @param frec_cantidad Boolean que define si la frecuencia se realiza haciendo 
#' el conteo de registros o  sumando la columna cantidad.
#' @return Tabla resumen con composición


datos_composicion <- function(data, columna_episodios, columna_valor,
                              columna_suma, columna_explorar, prioridad,
                              frec_cantidad = FALSE) {
print(prioridad)
  index_episodios <- data.frame(
    index = seq_len(length(prioridad)),
    agrupador = prioridad
  )
  colnames(index_episodios) <- c("index", columna_episodios)

  if (columna_explorar == columna_episodios) {
    columna_explorar_nuevo <- paste(columna_explorar, "(Explorar)")
    data <- data %>%
      mutate(!!columna_explorar_nuevo := !!rlang::sym(columna_explorar))
    columna_explorar <- columna_explorar_nuevo
  }

  episodios <- data %>%
    select(!!!rlang::syms(unique(c(columna_suma, columna_episodios)))) %>%
    filter(!!as.name(columna_episodios) %in% prioridad) %>%
    group_by(!!as.name(columna_suma)) %>%
    distinct() %>%
    right_join(index_episodios, copy = TRUE) %>%
    window_order(index) %>%
    mutate(!!columna_episodios := first(!!as.name(columna_episodios))) %>%
    ungroup() %>%
    distinct(!!!rlang::syms(unique(c(columna_suma, columna_episodios)))) %>%
    group_by(!!as.name(columna_episodios)) %>%
    mutate(n_episodios = n())

  sumas_episodios <- data %>%
    group_by(!!as.name(columna_suma)) %>%
    summarise(valor_calculos = sum(!!as.name(columna_valor), na.rm = TRUE)) %>%
    right_join(episodios) %>%
    ungroup() %>%
    group_by(!!as.name(columna_episodios)) %>%
    summarise(valor_episodios = sum(valor_calculos))

  datos_explorar <- data %>%
    group_by(!!!rlang::syms(unique(c(columna_suma, columna_explorar)))) %>%
    summarise(
      valor_explorar = sum(!!as.name(columna_valor), na.rm = TRUE),
      registros_explorar = ifelse(
        test = frec_cantidad,
        yes = sum(cantidad, na.rm = TRUE),
        no = n())) %>%
    right_join(episodios) %>%
    group_by(!!!rlang::syms(unique(c(columna_episodios, columna_explorar)))) %>%
    summarise(incluida_n_episodios = n(),
              n_registros = sum(registros_explorar),
              n_episodios = max(n_episodios),
              valor_explorar = sum(valor_explorar)) %>%
    right_join(sumas_episodios) %>%
    mutate(media_explorar_episodio = valor_explorar /
             na_if(incluida_n_episodios, 0),
           media_explorar_registro = valor_explorar /
             na_if(n_registros, 0),
           registros_por_episodios = n_registros /
             na_if(incluida_n_episodios, 0),
           media_episodio = valor_episodios /
             na_if(n_episodios, 0)) %>%
    mutate(participacion_en_episodios =
             round(100 * incluida_n_episodios / na_if(n_episodios, 0)),
           participacion_valor =
             round(100 * valor_explorar / na_if(valor_episodios, 0))) %>%
    select(!!as.name(columna_episodios),
           valor_episodios, media_episodio, n_episodios,
           !!as.name(columna_explorar),
           participacion_en_episodios, participacion_valor,
           n_registros, incluida_n_episodios, valor_explorar,
           registros_por_episodios,
           media_explorar_registro, media_explorar_episodio)

  return(datos_explorar)
}
