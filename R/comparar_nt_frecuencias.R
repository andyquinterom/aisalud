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

comparar_nt_frecuencias <- function(frecuencias, nota_tecnica, agrupador) {

  frec_y_nt <- frecuencias %>%
    rename(agrupador = !!as.name(agrupador)) %>%
    inner_join(nota_tecnica %>%
                 select(agrupador, frec_mes, cm)) %>%
    group_by(unidad_conteo, agrupador, frec_mes, cm) %>%
    mutate(
      valor_mes = frec_mes * cm,
      frec_media_por_cm = frec_media * cm,
      frec_suma_por_cm = frec_suma * cm) %>%
    group_by(unidad_conteo, agrupador, frec_mes, cm, valor_mes, frec_suma,
      frec_media, frec_suma_por_cm, frec_media_por_cm) %>%
    relocate(unidad_conteo, agrupador, frec_mes, cm, valor_mes, frec_suma,
      frec_media, frec_suma_por_cm, frec_media_por_cm)

}
