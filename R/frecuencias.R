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

frecuencias <- function(
  data, agrupador, columna_fecha, columna_suma,
  intervalo = "mes", prestaciones = FALSE, frec_cantidad = FALSE) {

  agrupador <- unique(agrupador)

  unidad <- "Episodio"
  if (prestaciones) unidad <- "Prestación"
  if (columna_suma == "nro_factura") unidad <- "Factura"
  if (columna_suma == "nro_identificacion") unidad <- "Paciente"

  data <- data %>%
    mutate_at(vars(agrupador), as.character)

  if (intervalo == "mes") {
    data <- data %>%
      mutate(mes_num = month(!!as.name(columna_fecha)),
             anio_num = year(!!as.name(columna_fecha))) %>%
      mutate(mes_anio_num = anio_num * 100 + mes_num)
    meses_limites <- data %>%
      select(mes_anio_num) %>%
      ungroup() %>%
      summarise(
        min = min(mes_anio_num, na.rm = TRUE),
        max = max(mes_anio_num, na.rm = TRUE)) %>%
      collect() %>%
      mutate(
        anio_min = min %/% 100,
        anio_max = max %/% 100,
        mes_min = min %% 100,
        mes_max = max %% 100)

    meses_completos <- tibble(
      anio = sort(rep(meses_limites$anio_min:meses_limites$anio_max, 12)),
      meses = rep(1:12, meses_limites$anio_max -
        meses_limites$anio_min + 1)) %>%
      mutate(mes_anio_num = anio * 100 + meses) %>%
      filter(mes_anio_num >= meses_limites$min &
        mes_anio_num <= meses_limites$max) %>%
      select(mes_anio_num) %>%
      mutate(placeholder_key = "key")

    data_to_join <- data %>%
      group_by(!!!rlang::syms(agrupador)) %>%
      summarise(placeholder_key = "key") %>%
      ungroup()

    meses_completos <- data_to_join %>%
      full_join(meses_completos, copy = TRUE) %>%
      select(-placeholder_key)
  }

  if (intervalo == "dia") {
    data <- data %>%
      mutate(mes_anio_num = !!as.name(columna_fecha))
  }

  if (!prestaciones) {
    data <- data %>%
      group_by(!!!rlang::syms(unique(c(columna_suma, agrupador)))) %>%
      summarise(mes_anio_num = max(mes_anio_num), cantidad = 1)
  }


  data <- data %>%
    group_by(!!!rlang::syms(agrupador), mes_anio_num) %>%
    summarise(Frecuencia = ifelse(
      test = prestaciones && frec_cantidad,
      yes = sum(cantidad, na.rm = TRUE),
      no = n())) %>%
    {if (intervalo == "mes") full_join(., meses_completos, copy = TRUE)
     else .} %>%
    mutate(Frecuencia = case_when(
        is.na(Frecuencia) ~ 0,
        TRUE ~ Frecuencia)) %>%
    group_by(!!!rlang::syms(agrupador)) %>%
    arrange(mes_anio_num) %>%
    collect()

  means_sums <- data %>%
    summarise(
      frec_media = round(mean(Frecuencia, na.rm = TRUE), digits = 3),
      frec_suma = round(sum(Frecuencia, na.rm = TRUE), digits = 3)
    )

  data <- data %>%
    pivot_wider(
      names_from = mes_anio_num,
      values_from = Frecuencia)

  if (intervalo == "mes") {
    data <- data %>%
      rename_with(mes_spanish_juntos, .cols = -seq_len(length(agrupador)))
  }

# Queda comentada esta sección hastas que quede actualizado dbplyr
# https://github.com/tidyverse/dbplyr/pull/676
#  if (intervalo == "semana") {
#    data <- data %>%
#      rename_with(function(x) {
#        paste(substr(x, 1, 4), substr(x, 5, 6), sep = " - ")
#      }, .cols = -seq_len(length(agrupador)))
#  }

  data <- data %>%
    left_join(means_sums) %>%
    mutate("unidad_conteo" = unidad) %>%
    relocate(unidad_conteo, !!!rlang::syms(agrupador), frec_suma, frec_media)

  return(data)
}
