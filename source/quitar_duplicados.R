quitar_duplicados <- function(bd) {
  duplicados <- duplicated(bd[, c("NRO_FACTURA", "NRO_IDENTIFICACION")])
  
  duplicado_bd <- bd[duplicados, c("NRO_FACTURA", "NRO_IDENTIFICACION")]
  
  duplicado_bd <- unique(duplicado_bd)
  
  duplicado_bd_datos <- as.data.table(merge(bd,
                                            duplicado_bd,
                                            by = c("NRO_IDENTIFICACION",
                                                   "NRO_FACTURA")))
  
  noduplicado_bd <- ajoin(bd,
                          duplicado_bd,
                          by = c("NRO_FACTURA", "NRO_IDENTIFICACION"))
  
  duplicado_bd_datos <- as.data.table(duplicado_bd_datos)
  setkeyv(duplicado_bd_datos, c("NRO_IDENTIFICACION", "NRO_FACTURA"))
  
  duplicado_bd_datos$FECHA_INICIO_HORA <- 
    paste0(duplicado_bd_datos$FECHA_PRESTACION,
           " ",
           duplicado_bd_datos$HORA_PRESTACION,
           ":00")
  
  duplicado_bd_datos$FECHA_EGRESO_HORA <-
    paste0(duplicado_bd_datos$FECHA_EGRESO,
           " ",
           duplicado_bd_datos$HORA_EGRESO,
           ":00")
  
  duplicado_bd_datos$FECHA_INICIO_HORA <-
    as.POSIXct(duplicado_bd_datos$FECHA_INICIO_HORA,
               format = "%d/%m/%Y %H:%M:%S")
  
  duplicado_bd_datos$FECHA_EGRESO_HORA <-
    as.POSIXct(duplicado_bd_datos$FECHA_EGRESO_HORA,
               format = "%d/%m/%Y %H:%M:%S")
  
  duplicado_bd_datos$TIEMPO_HOS <-
    difftime(time1 = duplicado_bd_datos$FECHA_EGRESO_HORA,
             time2 = duplicado_bd_datos$FECHA_INICIO_HORA,
             units = "days")
  
  
  duplicado_bd_datos_max <-
    duplicado_bd_datos[, list(TIEMPO_HOS = max(TIEMPO_HOS,
                                               na.rm = TRUE)),
                         by = c("NRO_FACTURA", "NRO_IDENTIFICACION")]
  
  duplicado_bd_datos <- merge(duplicado_bd_datos,
                             duplicado_bd_datos_max,
                             by = c("NRO_FACTURA",
                                     "NRO_IDENTIFICACION",
                                     "TIEMPO_HOS"))
  
  duplicados <- duplicated(duplicado_bd_datos[, list(NRO_FACTURA,
                                                    NRO_IDENTIFICACION,
                                                    TIEMPO_HOS)])
  
  rownames(duplicado_bd_datos) <- NULL
  
  duplicado_bd_datos <- duplicado_bd_datos[!duplicados]
  
  
  duplicado_bd_datos[, "TIEMPO_HOS" := NULL]
  duplicado_bd_datos[, "FECHA_INICIO_HORA" := NULL]
  duplicado_bd_datos[, "FECHA_EGRESO_HORA" := NULL]
  
  bd <- rbind(noduplicado_bd, duplicado_bd_datos)
  
  return(bd)
}

quitar_usuarios_duplicados <- function(us) {
  
  us$TIPO_USUARIO <- as.character(us$TIPO_USUARIO)
  us_duplicados <- us[duplicated(us$NRO_IDENTIFICACION), 
                      list("DUPLICADO" = "1"), 
                      by = c("NRO_IDENTIFICACION")]
  us <- merge(us, us_duplicados, all.x = TRUE, by = c("NRO_IDENTIFICACION"))
  us$TIPO_USUARIO[us$DUPLICADO == "1"] <- "Movilidad"
  us_max_edad <- us[, list(EDAD = max(EDAD, na.rm = TRUE)),
                     by = c("NRO_IDENTIFICACION")]
  us_max_edad_datos <- merge(us,
                             us_max_edad, 
                             by = c("NRO_IDENTIFICACION"))
  us_max_edad_datos <- 
    us_max_edad_datos[!duplicated(us_max_edad_datos$NRO_IDENTIFICACION)]
  us <- us_max_edad_datos
  us[, "DUPLICADO" := NULL]
  us[, "EDAD.x" := NULL]
  setnames(us, "EDAD.y", "EDAD")

  return(us)
  
}
