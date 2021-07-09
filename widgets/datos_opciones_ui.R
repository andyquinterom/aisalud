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

# UI para las opciones utilizadas para cargar datos
datos_opciones_ui <- function(
  id, file_type, value_decimal, value_delimitador, value_sheet, value_range,
  value_file) {
  # Si el tipo de archivo es CSV se return el UI para csv
  # En caso de que sea un archivo feather no mostrará opciones
  # Si se necesitan agregar más tipos de archivo aquí se pueden agregar
  if (file_type == "csv") {
    return(
      datos_opciones_csv_ui(
        id = id,
        value_decimal = value_decimal,
        value_delimitador = value_delimitador)
    )
  }
}
