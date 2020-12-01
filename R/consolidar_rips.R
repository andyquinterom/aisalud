consolidar_rips <- function(prestadores, ac, af, ah, am, ap, at, au, us, cups) {
  
  # Los diferentes archivos se transforman a un objecto de tipo data.table
  # Se filtran para solo obtener datos de los prestadores seleccionados
  ac <- as.data.table(ac)[cod_prestador %in% prestadores]
  af <- as.data.table(af)[cod_prestador %in% prestadores]
  ah <- as.data.table(ah)[cod_prestador %in% prestadores]
  am <- as.data.table(am)[cod_prestador %in% prestadores]
  ap <- as.data.table(ap)[cod_prestador %in% prestadores]
  at <- as.data.table(at)[cod_prestador %in% prestadores]
  au <- as.data.table(au)[cod_prestador %in% prestadores]
  
  # El ambito de consultas siempre es ambulatorio
  ac$ambito <- "1"
  
  # EL ambito de medicamentos sera su tipo
  am$ambito <- am$tipo_medicamento
  
  # La cantidad de consultas y procedimientos siemore es 1
  ac$cantidad <- 1
  ap$cantidad <- 1
  
  # Consultas y procedimientos no incluyen nombre de prestación
  ac$nombre_prestacion <- ""
  ap$nombre_prestacion <- ""
  
  # Se eliminan columnas innecesarias
  ah$tipo_identificacion <- NULL
  au$tipo_identificacion <- NULL
  us$tipo_identificacion <- NULL
  us$cod_eapb            <- NULL
  
  # Conversión de tipo de servicio númerico a valor correspondiente
  
  tryCatch(
    expr = {
      at$tipo_servicio <- conversion_tiposervicio(at$tipo_servicio)
    },
    error = function(e) {
      print(e)
    }
  )
  
  # Conversión de tipo de medicamento a valor correspondiente
  
  tryCatch(
    expr = {
      am$ambito <- conversion_tipomedicamento(am$ambito)
    },
    error = function(e) {
      print(e)
    }
  )
  
  # Se cambian nombres de columnas para merge
  setnames(ac, 7, "cod_prestacion")
  
  setnames(ap, 7, "cod_prestacion")
  setnames(at, 7, "cod_prestacion")
  setnames(am, 6, "cod_prestacion")
  setnames(at, 8, "nombre_prestacion")
  setnames(am, 8, "nombre_prestacion")
  setnames(at, 6, "ambito")
  
  # Se hace merge entre el archivo de transacciones y los de prestaciones
  
  columnas_merge_af <- c("nro_factura",
                         "nro_identificacion",
                         "valor", "cod_prestacion",
                         "nombre_prestacion",
                         "ambito",
                         "cantidad")
  
  merge_af_ac <- merge(af, ac[, columnas_merge_af, with = FALSE],
                       by = "nro_factura")
  merge_af_ap <- merge(af, ap[, columnas_merge_af, with = FALSE],
                       by = "nro_factura")
  merge_af_at <- merge(af, at[, columnas_merge_af, with = FALSE],
                       by = "nro_factura")
  merge_af_am <- merge(af, am[, columnas_merge_af, with = FALSE],
                       by = "nro_factura")
  
  merge_af <- rbind(
    merge_af_ac,
    merge_af_ap,
    merge_af_at,
    merge_af_am
  )
  
  # Conversión de ambito a valor correspondiente
  
  merge_af$ambito <- conversion_ambito(merge_af$ambito)
  
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
  
  au$fecha_prestacion <- NULL
  ah$fecha_prestacion <- NULL
  
  # Merge archivo de urgencias y hospotilización con el de prestaciones
  
  # Si hay datos para hospitalización pero no para urgencias
  if (nrow(au) == 0 & nrow(ah) != 0) {
    merge_prestaciones <- merge.data.table(x = merge_af,
                                           y = ah,
                                           by = c("nro_factura",
                                                  "nro_identificacion",
                                                  "cod_prestador"),
                                           all.x = TRUE
                                           )
  }
  
  # Si hay datos para urgencias pero no para hospitalización
  if (nrow(au) != 0 & nrow(ah) == 0) {
    merge_prestaciones <- merge.data.table(x = merge_af,
                                           y = au,
                                           by = c("nro_factura",
                                                  "nro_identificacion",
                                                  "cod_prestador"),
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
                                             by = c("nro_factura",
                                                    "nro_identificacion",
                                                    "cod_prestador"))
    
    # Prestaciones que no son hospitalizaciones
    merge_prestaciones_sin_ah <- ajoin(x = merge_af,
                                       y = merge_prestaciones_ah,
                                       by = c("nro_factura",
                                              "nro_identificacion",
                                              "cod_prestador"))
    
    # Prestaciones de urgencias que no se encuentran en hospitalizaciones
    merge_prestaciones_au <- merge.data.table(x = merge_prestaciones_sin_ah,
                                              y = au,
                                              by = c("nro_factura",
                                                     "nro_identificacion",
                                                     "cod_prestador"),
                                              all.x = TRUE)
    
    # Juntar hospitalizaciones con urgencias
    merge_prestaciones <- rbind(merge_prestaciones_ah,
                                merge_prestaciones_au,
                                fill = TRUE)
    
  }
  
  # Cambiar nombres de columnas valor.x y valor.y después del merge
  setnames(merge_prestaciones, "valor.x", "valor_factura")
  setnames(merge_prestaciones, "valor.y", "valor")
  
  # Juntar datos de usuarios con las prestaciones
  if (nrow(us) != 0) {
    
    merge_prestaciones <- merge(x = merge_prestaciones,
                                y = us,
                                by = "nro_identificacion",
                                all.x = TRUE
                                )
    
    # Conversión de tipo de usuario a su valor correspondiente
    merge_prestaciones$tipo_usuario <- 
      conversion_tipousuario(merge_prestaciones$tipo_usuario)
    
  }
  
  # Generar nombres de prestación faltantes
  
  # Encontrar y marcar los valores solo numéricos
  prestacion_numerica <- !grepl("\\D", merge_prestaciones$cod_prestacion)
  
  # Convertir de character a numerico y devuelta
  cod_prestacion_num <- merge_prestaciones$cod_prestacion[prestacion_numerica]
  cod_prestacion_num <- as.character(
    as.numeric(as.character(cod_prestacion_num))
  )
  
  merge_prestaciones$cod_prestacion[prestacion_numerica] <- cod_prestacion_num
  
  # Merge codigo de prestación con indice de cups

  merge_prestaciones <- rbind(
    merge(
      merge_prestaciones[nombre_prestacion == ""][, -c("nombre_prestacion")],
      cups,
      by = c("cod_prestacion"),
      all.x = TRUE
      )[, union(names(merge_prestaciones), names(cups)), with = FALSE],
    merge_prestaciones[nombre_prestacion != ""]
  )
  
  return(merge_prestaciones)
  
}