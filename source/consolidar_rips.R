consolidar_rips <- function(prestadores, ac, af, ah, am, ap, at, au, us, cups) {
  
  # Los diferentes archivos se transforman a un objecto de tipo data.table
  # Se filtran para solo obtener datos de los prestadores seleccionados
  ac <- as.data.table(ac)[COD_PRESTADOR %in% prestadores]
  af <- as.data.table(af)[COD_PRESTADOR %in% prestadores]
  ah <- as.data.table(ah)[COD_PRESTADOR %in% prestadores]
  am <- as.data.table(am)[COD_PRESTADOR %in% prestadores]
  ap <- as.data.table(ap)[COD_PRESTADOR %in% prestadores]
  at <- as.data.table(at)[COD_PRESTADOR %in% prestadores]
  au <- as.data.table(au)[COD_PRESTADOR %in% prestadores]
  
  # El ambito de consultas siempre es ambulatorio
  ac$AMBITO <- "1"
  
  # EL ambito de medicamentos sera su tipo
  am$AMBITO <- am$TIPO_MEDICAMENTO
  
  # La cantidad de consultas y procedimientos siemore es 1
  ac$CANTIDAD <- 1
  ap$CANTIDAD <- 1
  
  # Consultas y procedimientos no incluyen nombre de prestación
  ac$NOMBRE_PRESTACION <- ""
  ap$NOMBRE_PRESTACION <- ""
  
  # Se eliminan columnas innecesarias
  ah$TIPO_IDENTIFICACION <- NULL
  au$TIPO_IDENTIFICACION <- NULL
  us$TIPO_IDENTIFICACION <- NULL
  us$COD_EAPB            <- NULL
  
  # Conversión de tipo de servicio númerico a valor correspondiente
  
  tryCatch(
    expr = {
      at$TIPO_SERVICIO <- conversion_tiposervicio(at$TIPO_SERVICIO)
    },
    error = function(e) {
      print(e)
    }
  )
  
  # Conversión de tipo de medicamento a valor correspondiente
  
  tryCatch(
    expr = {
      am$AMBITO <- conversion_tipomedicamento(am$AMBITO)
    },
    error = function(e) {
      print(e)
    }
  )
  
  # Se cambian nombres de columnas para merge
  colnames(ac)[7] <- "COD_PRESTACION"
  colnames(ap)[7] <- "COD_PRESTACION"
  colnames(at)[7] <- "COD_PRESTACION"
  colnames(am)[6] <- "COD_PRESTACION"
  colnames(at)[8] <- "NOMBRE_PRESTACION"
  colnames(am)[8] <- "NOMBRE_PRESTACION"
  colnames(at)[6] <- "AMBITO"
  
  # Se hace merge entre el archivo de transacciones y los de prestaciones
  
  columnas_merge_af <- c("NRO_FACTURA",
                         "NRO_IDENTIFICACION",
                         "VALOR", "COD_PRESTACION",
                         "NOMBRE_PRESTACION",
                         "AMBITO",
                         "CANTIDAD")
  
  merge_af_ac <- merge(af, ac[, columnas_merge_af, with = FALSE],
                       by = "NRO_FACTURA")
  merge_af_ap <- merge(af, ap[, columnas_merge_af, with = FALSE],
                       by = "NRO_FACTURA")
  merge_af_at <- merge(af, at[, columnas_merge_af, with = FALSE],
                       by = "NRO_FACTURA")
  merge_af_am <- merge(af, am[, columnas_merge_af, with = FALSE],
                       by = "NRO_FACTURA")
  
  merge_af <- rbind(
    merge_af_ac,
    merge_af_ap,
    merge_af_at,
    merge_af_am
  )
  
  # Conversión de ambito a valor correspondiente
  
  merge_af$AMBITO <- conversion_ambito(merge_af$AMBITO)
  
  # Encontrar duplicados y eliminar
  
  tryCatch(
    expr = {
      au <- quitar_duplicados(au)
    },
    error = function(e) {
      print(e)
    }
  )
  
  tryCatch(
    expr = {
      ah <- quitar_duplicados(ah)
    },
    error = function(e) {
      print(e)
    }
  )
  
  tryCatch(
    expr = {
      us <- quitar_usuarios_duplicados(us)
    },
    error = function(e) {
      print(e)
    }
  )
  
  # Quitar fechas innecesarias
  
  au$FECHA_PRESTACION <- NULL
  ah$FECHA_PRESTACION <- NULL
  
  # Merge archivo de urgencias y hospotilización con el de prestaciones
  
  # Si hay datos para hospitalización pero no para urgencias
  if (nrow(au) == 0 & nrow(ah) != 0) {
    merge_prestaciones <- merge.data.table(x = merge_af,
                                           y = ah,
                                           by = c("NRO_FACTURA",
                                                  "NRO_IDENTIFICACION",
                                                  "COD_PRESTADOR"),
                                           all.x = TRUE
                                           )
  }
  
  # Si hay datos para urgencias pero no para hospitalización
  if (nrow(au) != 0 & nrow(ah) == 0) {
    merge_prestaciones <- merge.data.table(x = merge_af,
                                           y = au,
                                           by = c("NRO_FACTURA",
                                                  "NRO_IDENTIFICACION",
                                                  "COD_PRESTADOR"),
                                           all.x = TRUE
    )
  }
  
  # Si noy hay datos para urgencias ni para hospitalización
  if (nrow(au) == 0 & nrow(ah) == 0) {
    merge_prestaciones <- merge_af
  }
  
  # Si hay datos para urgencias y hospitalización
  if (nrow(au) != 0 & nrow(ah) != 0) {
    # Prestaciones de hospitalización
    merge_prestaciones_ah <- merge.data.table(x = merge_af,
                                             y = ah,
                                             by = c("NRO_FACTURA",
                                                    "NRO_IDENTIFICACION",
                                                    "COD_PRESTADOR"))
    
    # Prestaciones que no son hospitalizaciones
    merge_prestaciones_sin_ah <- ajoin(x = merge_af,
                                       y = merge_prestaciones_ah,
                                       by = c("NRO_FACTURA",
                                              "NRO_IDENTIFICACION",
                                              "COD_PRESTADOR"))
    
    # Prestaciones de urgencias que no se encuentran en hospitalizaciones
    merge_prestaciones_au <- merge.data.table(x = merge_prestaciones_sin_ah,
                                              y = au,
                                              by = c("NRO_FACTURA",
                                                     "NRO_IDENTIFICACION",
                                                     "COD_PRESTADOR"),
                                              all.x = TRUE)
    
    # Juntar hospitalizaciones con urgencias
    merge_prestaciones <- rbind(merge_prestaciones_ah,
                                merge_prestaciones_au,
                                fill = TRUE)
    
  }
  
  # Cambiar nombres de columnas VALOR.x y VALOR.y después del merge
  col_merge_pres <- colnames(merge_prestaciones)
  col_merge_pres[which(col_merge_pres == "VALOR.x")] <- "VALOR_FACTURA"
  col_merge_pres[which(col_merge_pres == "VALOR.y")] <- "VALOR"
  colnames(merge_prestaciones) <- col_merge_pres
  
  # Juntar datos de usuarios con las prestaciones
  if (nrow(us) != 0) {
    
    merge_prestaciones <- merge(x = merge_prestaciones,
                                y = us,
                                by = "NRO_IDENTIFICACION",
                                all.x = TRUE
                                )
    
    # Conversión de tipo de usuario a su valor correspondiente
    merge_prestaciones$TIPO_USUARIO <- 
      conversion_tipousuario(merge_prestaciones$TIPO_USUARIO)
    
  }
  
  # Generar nombres de prestación faltantes
  
  # Encontrar y marcar los valores solo numéricos
  prestacion_numerica <- !grepl("\\D", merge_prestaciones$COD_PRESTACION)
  
  # Convertir de character a numerico y devuelta
  cod_prestacion_num <- merge_prestaciones$COD_PRESTACION[prestacion_numerica]
  cod_prestacion_num <- as.character(
    as.numeric(as.character(cod_prestacion_num))
  )
  
  merge_prestaciones$COD_PRESTACION[prestacion_numerica] <- cod_prestacion_num
  
  # Merge codigo de prestación con indice de cups

  merge_prestaciones <- rbind(
    merge(
      merge_prestaciones[NOMBRE_PRESTACION == ""][, -c("NOMBRE_PRESTACION")],
      cups,
      by = c("COD_PRESTACION"),
      all.x = TRUE
      )[, union(names(merge_prestaciones), names(cups)), with = FALSE],
    merge_prestaciones[NOMBRE_PRESTACION != ""]
  )
  
  return(merge_prestaciones)
  
}