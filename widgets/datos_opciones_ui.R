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
