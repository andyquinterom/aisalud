conversion_tiposervicio <- function(tipo_servicio) {
  # Convertir vector a character
  tipo_servicio <- as.character(tipo_servicio)
  
  # Reemplazar valores númericos con sus correspondientes
  return(
    str_replace_all(tipo_servicio,
                    c("1" = "Materiales e insumos",
                      "2" = "Traslados",
                      "3" = "Estancias",
                      "4" = "Honorarios")
    )
  )
}

conversion_tipomedicamento <- function(tipo_medicamento) {
  # Convertir vector a character
  tipo_medicamento <- as.character(tipo_medicamento)
  
  # Reemplazar valores númericos con sus correspondientes
  return(
    str_replace_all(tipo_medicamento,
                    c("1" = "Medicamentos PBS",
                      "2" = "Medicamentos no PBS")
    )
  )
}

conversion_ambito <- function(ambito) {
  # Convertir vector a character
  ambito <- as.character(ambito)
  
  # Reemplazar valores númericos con sus correspondientes
  return(
    str_replace_all(ambito,
                    c("1" = "Ambulatorio",
                      "2" = "Hospitalario",
                      "3" = "Urgencias")
    )
  )
}

conversion_tipousuario <- function(tipo_usuario) {
  # Convertir vector a character
  tipo_usuario <- as.character(tipo_usuario)
  
  # Reemplazar valores númericos con sus correspondientes
  return(
    str_replace_all(tipo_usuario,
                    c("1" = "Contributivo",
                      "2" = "Subsidiado",
                      "3" = "Vinculado",
                      "4" = "Particular",
                      "5" = "Otro",
                      "6" = "Victima con afiliacion al Regimen Contributivo",
                      "7" = "Victima con afiliacion al Regimen Subsidiado",
                      "8" = "Victima no asegurado")
    )
  )
}