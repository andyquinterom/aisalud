quitar_duplicados <- function(bd) {
  duplicados <- duplicated(bd[, c("nro_factura", "nro_identificacion")])
  
  duplicado_bd <- bd[duplicados, c("nro_factura", "nro_identificacion")]
  
  duplicado_bd <- unique(duplicado_bd)
  
  duplicado_bd_datos <- as.data.table(merge(bd,
                                            duplicado_bd,
                                            by = c("nro_identificacion",
                                                   "nro_factura")))
  
  noduplicado_bd <- ajoin(bd,
                          duplicado_bd,
                          by = c("nro_factura", "nro_identificacion"))
  
  duplicado_bd_datos <- as.data.table(duplicado_bd_datos)
  setkeyv(duplicado_bd_datos, c("nro_identificacion", "nro_factura"))
  
  duplicado_bd_datos$fecha_inicio_hora <- 
    paste0(duplicado_bd_datos$fecha_prestacion,
           " ",
           duplicado_bd_datos$hora_prestacion,
           ":00")
  
  duplicado_bd_datos$fecha_egreso_hora <-
    paste0(duplicado_bd_datos$fecha_egreso,
           " ",
           duplicado_bd_datos$hora_egreso,
           ":00")
  
  duplicado_bd_datos$fecha_inicio_hora <-
    as.POSIXct(duplicado_bd_datos$fecha_inicio_hora,
               format = "%d/%m/%Y %H:%M:%S")
  
  duplicado_bd_datos$fecha_egreso_hora <-
    as.POSIXct(duplicado_bd_datos$fecha_egreso_hora,
               format = "%d/%m/%Y %H:%M:%S")
  
  duplicado_bd_datos$tiempo_hos <-
    difftime(time1 = duplicado_bd_datos$fecha_egreso_hora,
             time2 = duplicado_bd_datos$fecha_inicio_hora,
             units = "days")
  
  
  duplicado_bd_datos_max <-
    duplicado_bd_datos[, list(tiempo_hos = max(tiempo_hos,
                                               na.rm = TRUE)),
                         by = c("nro_factura", "nro_identificacion")]
  
  duplicado_bd_datos <- merge(duplicado_bd_datos,
                             duplicado_bd_datos_max,
                             by = c("nro_factura",
                                     "nro_identificacion",
                                     "tiempo_hos"))
  
  duplicados <- duplicated(duplicado_bd_datos[, list(nro_factura,
                                                    nro_identificacion,
                                                    tiempo_hos)])
  
  rownames(duplicado_bd_datos) <- NULL
  
  duplicado_bd_datos <- duplicado_bd_datos[!duplicados]
  
  
  duplicado_bd_datos[, "tiempo_hos" := NULL]
  duplicado_bd_datos[, "fecha_inicio_hora" := NULL]
  duplicado_bd_datos[, "fecha_egreso_hora" := NULL]
  
  bd <- rbind(noduplicado_bd, duplicado_bd_datos)
  
  return(bd)
}

quitar_usuarios_duplicados <- function(us) {
  
  us$tipo_usuario <- as.character(us$tipo_usuario)
  us_duplicados <- us[duplicated(us$nro_identificacion), 
                      list("duplicado" = "1"), 
                      by = c("nro_identificacion")]
  us <- merge(us, us_duplicados, all.x = TRUE, by = c("nro_identificacion"))
  us$tipo_usuario[us$duplicado == "1"] <- "Movilidad"
  us_max_edad <- us[, list(edad = max(edad, na.rm = TRUE)),
                     by = c("nro_identificacion")]
  us_max_edad_datos <- merge(us,
                             us_max_edad, 
                             by = c("nro_identificacion"))
  us_max_edad_datos <- 
    us_max_edad_datos[!duplicated(us_max_edad_datos$nro_identificacion)]
  us <- us_max_edad_datos
  us[, "duplicado" := NULL]
  us[, "edad.x" := NULL]
  setnames(us, "edad.y", "edad")

  return(us)
  
}
