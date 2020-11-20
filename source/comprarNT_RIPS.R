cbind.fill <- function(nm, agrupador){
  if (length(nm) == 1) {
    return(nm)
  }
  if (length(nm) > 1) {
    merged <- merge.data.table(nm[[1]], nm[[2]], by = agrupador, all = TRUE)
    nm <- append(list(merged), nm[-c(1:2)])
    cbind.fill(nm, agrupador = agrupador)
  }
}

mes_spanish <- function(x) {
  meses <- c(
    "Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "Septiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
  )
  return(
    meses[x]
  )
  
}

descriptiva_basica <- function(
  data, agrupador, columna_valor, prestaciones, columna_fecha, columna_suma) {
  data <- copy(data)
  setnames(data, columna_valor, "valor_calculos")
  data[, "valor_calculos" := numerize(valor_calculos)]
  if (!prestaciones) {
    data[, "MES_ANIO_NUM" := min(lubridate::year(
      get(columna_fecha))*100 + lubridate::month(get(columna_fecha)),
      na.rm = TRUE),
      by = c(columna_suma)]
    data[, "MES_ANIO" := paste(min(lubridate::year(
      get(columna_fecha))),
      mes_spanish(min(lubridate::month(get(columna_fecha)))),
      sep = " - "),
      by = c(columna_suma)]
    columnas = c(agrupador, "MES_ANIO_NUM", "MES_ANIO")
    data <- data[, list("valor_calculos" = sum(valor_calculos)),
                 by = c(columna_suma,
                        columnas[columnas != columna_suma])]
  } else {
    data[, "MES_ANIO_NUM" := lubridate::year(
      data[[columna_fecha]])*100 + lubridate::month(data[[columna_fecha]])]
    data[, "MES_ANIO" := paste(lubridate::year(
      data[[columna_fecha]]),
      mes_spanish(lubridate::month(data[[columna_fecha]])),
      sep = " - ")]
    columnas = c(agrupador, "MES_ANIO_NUM", "MES_ANIO")
  }
  data <- data[, list("Frecuencia" = length(valor_calculos),
                      "Suma" = sum(valor_calculos, na.rm = TRUE)),
                      by = c(columnas)]
  setnames(data, c(columnas, "Frecuencia", "Suma"))
  
  return(data[order(MES_ANIO_NUM)])
  data <- NULL
}

descriptiva_basica_episodios <- function(
  data, agrupador, columna_valor, prestaciones, columna_fecha, columna_suma) {
  data <- copy(data)
  setnames(data, columna_valor, "valor_calculos")
  data[, "valor_calculos" := numerize(valor_calculos)]
  data[, "MES_ANIO_NUM" := min(lubridate::year(
    get(columna_fecha))*100 + lubridate::month(get(columna_fecha)),
    na.rm = TRUE),
    by = c(columna_suma)]
  data[, "MES_ANIO" := paste(min(lubridate::year(
    get(columna_fecha))),
    mes_spanish(min(lubridate::month(get(columna_fecha)))),
    sep = " - "),
    by = c(columna_suma)]
  columnas = c(agrupador, "MES_ANIO_NUM", "MES_ANIO")
  data <- data[, list("FREC_PACIENTES" = uniqueN(get(columna_suma)),
                      "Suma_ep" = sum(valor_calculos, na.rm = TRUE),
                      "VAR_COLUMNAS" = unique(get(agrupador))),
               by = c(columna_suma, "MES_ANIO_NUM", "MES_ANIO")]
  setnames(data, "VAR_COLUMNAS", agrupador)
  data <- data[, list("Frecuencia" = length(Suma_ep),
                      "Suma" = sum(Suma_ep, na.rm = TRUE)),
               by = c(columnas)]
  setnames(data, c(columnas, "Frecuencia", "Suma"))
  
  return(data[order(MES_ANIO_NUM)])
  data <- NULL
}


descriptiva_basica_trans <- function(data, agrupador, frec = TRUE, suma = TRUE) {
  data <- copy(data)
  meses <- as.list(unique(data$MES_ANIO))
  agrupCompletos <- data.table(agrupador = unique(data[[agrupador]]))
  setnames(agrupCompletos, agrupador)
  data_sep <- lapply(
    meses,
    data = data,
    agrupador = agrupador,
    frec = frec,
    suma = suma,
    FUN = function(x, data, agrupador, frec, suma) {
      data <- data[MES_ANIO == x,
                  c(agrupador,
                    "MES_ANIO", c("Frecuencia", "Suma")[c(frec, suma)]), 
                  with = FALSE]
      setnames(data,
               c(agrupador,
                 "MES_ANIO",
                 paste(unique(data[["MES_ANIO"]]),
                       c("Frecuencia", "Suma")[c(frec, suma)]))) 

           return(data[,-c("MES_ANIO")])
      
  })

  return(cbind.fill(data_sep, agrupador = agrupador)[[1]])
  
}

multiplicar_cme <- function(frecs, nota_tecnica) {
  frecs <- copy(frecs)
  nota_tecnica <- copy(nota_tecnica)
  setnames(frecs, 1, "agrupador")
  agrupadores_compartidos <- intersect(nota_tecnica[["agrupador"]], 
                                frecs[["agrupador"]])
  nota_tecnica <- nota_tecnica[agrupador %in% agrupadores_compartidos]
  frecs <- frecs[agrupador %in% agrupadores_compartidos]
  
  nota_tecnica <- nota_tecnica[order(agrupador)]
  frecs <- frecs[order(agrupador)]
  
  return(
    as.matrix(apply(frecs[, -c("agrupador")], 2, as.numeric)) * 
      numerize(nota_tecnica[["cm"]])
    )
  
}

diferencia_valor_rips <- function(sumas, nota_tecnica, porcentaje = FALSE) {
  sumas <- copy(sumas)
  nota_tecnica <- copy(nota_tecnica)
  setnames(sumas, 1, "agrupador")
  agrupadores_compartidos <- intersect(nota_tecnica[["agrupador"]],
                                sumas[["agrupador"]])
  nota_tecnica <- nota_tecnica[
    agrupador %in% agrupadores_compartidos][order(agrupador)]
  sumas <- sumas[agrupador %in% agrupadores_compartidos][order(agrupador)]
  
  nota_tecnica[, "cm" := numerize(cm)]
  nota_tecnica[, "valor_mes" := numerize(valor_mes)]
  
  numero_meses <- 2:ncol(sumas)
  
  sumas[, (numero_meses) := lapply(.SD, numerize), .SDcols = numero_meses]
  
  if (porcentaje) {
    return(
      as.data.table(
        append(
          list(agrupador = sumas$agrupador),
          as.data.frame(
            sumas[, c(numero_meses) , with = FALSE] / 
              nota_tecnica[["valor_mes"]])))
    )
  } else {
    return(
      as.data.table(
        append(
          list(agrupador = sumas$agrupador),
          as.data.frame(
            sumas[, c(numero_meses), with = FALSE] -
              nota_tecnica[["valor_mes"]])))
    )
  }
}

diferencia_valor_cme <- function(frecs, nota_tecnica, porcentaje = FALSE) {
  frecs <- copy(frecs)
  nota_tecnica <- copy(nota_tecnica)
  setnames(frecs, 1, "agrupador")
  agrupadores_compartidos <- intersect(nota_tecnica[["agrupador"]],
                                frecs[["agrupador"]])
  nota_tecnica <- nota_tecnica[
    agrupador %in% agrupadores_compartidos][order(agrupador)]
  frecs <- frecs[agrupador %in% agrupadores_compartidos][order(agrupador)]
  
  nota_tecnica[["cm"]] <- numerize(nota_tecnica[["cm"]])
  nota_tecnica[["valor_mes"]] <- numerize(nota_tecnica[["valor_mes"]])
  
  numero_meses <- 2:ncol(frecs)
  frecs[, (numero_meses) := lapply(.SD, numerize), .SDcols = numero_meses]
  
  if (porcentaje) {
    return(
      as.data.table(
        append(list(agrupador = frecs$agrupador),
               as.data.frame((
                 frecs[, c(numero_meses) , with = FALSE] * 
                   numerize(nota_tecnica[["cm"]])/
                   nota_tecnica[["valor_mes"]]))))
    )
  } else {
     return(
      as.data.table(
        append(list(agrupador = frecs$agrupador), 
               as.data.frame((
                 frecs[, c(numero_meses) , with = FALSE] * 
                   numerize(nota_tecnica[["cm"]]) - 
                   nota_tecnica[["valor_mes"]]))))
    )
  }
}

diferencias_totales <- function(frecs, sumas, nota_tecnica) {
  frecs <- copy(frecs)
  sumas <- copy(sumas)
  nota_tecnica <- copy(nota_tecnica)
  
  setnames(frecs, 1, "agrupador")
  setnames(sumas, 1, "agrupador")
  agrupadores_compartidos <- intersect(nota_tecnica[["agrupador"]],
                                frecs[["agrupador"]])
  valor_ejecutar_mes <- sum(nota_tecnica[["valor_mes"]], na.rm = TRUE)
  nota_tecnica <- nota_tecnica[
    agrupador %in% agrupadores_compartidos][order(agrupador)]
  frecs <- frecs[agrupador %in% agrupadores_compartidos][order(agrupador)]
  sumas <- sumas[agrupador %in% agrupadores_compartidos][order(agrupador)]
  
  nota_tecnica[, "cm" := numerize(cm)]
  nota_tecnica[, "valor_mes" := numerize(valor_mes)]
  
  total_mes_rips <- data.table(
    "Mes" = c(colnames(sumas[, -c(1)])),
    "Total" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE)) -
                       valor_ejecutar_mes),
    "%" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE)) /
              valor_ejecutar_mes)
  )
  
  total_agrupador_rips <- data.table(
    "Agrupador" = c(sumas[["agrupador"]]),
    "Total" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE)) -
                       (nota_tecnica[["valor_mes"]] * ncol(sumas[, -c(1)]))),
    "%" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE)) /
              (nota_tecnica[["valor_mes"]] * ncol(sumas[, -c(1)])))
  )
  
  numero_meses <- 2:ncol(frecs)
  frecs[, (numero_meses) := lapply(.SD, numerize), .SDcols = numero_meses]
  
  sumas_cme <- frecs[, c(numero_meses) , with = FALSE] * 
    numerize(nota_tecnica[["cm"]])
  
  total_mes_cme <- data.table(
    "Mes" = c(colnames(sumas[, -c(1)])),
    "Total" = c(as.vector(apply(sumas_cme, 2, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas_cme, 2, sum, na.rm = TRUE)) - 
                       valor_ejecutar_mes),
    "%" = c(as.vector(apply(sumas_cme, 2, sum, na.rm = TRUE)) /
              valor_ejecutar_mes)
  )
  
  total_agrupador_cme <- data.table(
    "Mes" = c(sumas[["agrupador"]]),
    "Total" = c(as.vector(apply(sumas_cme, 1, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas_cme, 1, sum, na.rm = TRUE)) -
                       (nota_tecnica[["valor_mes"]] * ncol(sumas[, -c(1)]))),
    "%" = c(as.vector(apply(sumas_cme, 1, sum, na.rm = TRUE)) /
              (nota_tecnica[["valor_mes"]] * ncol(sumas[, -c(1)])))
  )
  
  valor_ejecutar <- valor_ejecutar_mes * ncol(sumas[, -c(1)])
  valor_ejecutado_rips <- sum(sumas[, -c(1)], na.rm = TRUE)
  valor_ejecutado_cme <- sum(sumas_cme, na.rm = TRUE)
  
  totales <- data.table(
    "Detalle" = c("Valor a ejecutar", "Ejecutado RIPS", "Ejecutado CM"),
    "Valor" = c(valor_ejecutar, valor_ejecutado_rips, valor_ejecutado_cme),
    "Diferencias" = c(NA,
                      valor_ejecutado_rips - valor_ejecutar,
                      valor_ejecutado_cme  - valor_ejecutar),
    "%" = c(NA,
                      valor_ejecutado_rips/valor_ejecutar,
                      valor_ejecutado_cme/valor_ejecutar)
  )
  
  
  
  return(
    list(
      total_mes_rips = total_mes_rips,
      total_mes_cme  = total_mes_cme,
      total_agrupador_rips = total_agrupador_rips,
      total_agrupador_cme  = total_agrupador_cme,
      totales = totales
    )
  )
  
}


descriptiva_basica_jerarquia <- function(
  data, columnas, columna_valor, columna_suma, nivel_1, nivel_2, 
  nivel_3, nivel_4, return_list = FALSE) {
  
  data <- copy(data)
  
  data[, "ASIGNACION_NIVEL" := ""]
  
  episodios_nivel_1 <- data.table()
  episodios_nivel_2 <- data.table()
  episodios_nivel_3 <- data.table()
  episodios_nivel_4 <- data.table()
  
  if (!is.null(nivel_1)) {
    episodios_nivel_1 <- list()
    lapply(
      X = nivel_1,
      FUN = function(i) {
        episodios_nivel_1[[i]] <<- descriptiva_basica_episodios(
          data = data,
          agrupador = columnas,  
          columna_valor = columna_valor,
          columna_suma = columna_suma,
          columna_fecha = "fecha_prestacion",
          prestaciones = FALSE
        )[get(columnas) %in% i]
        registros_procesados <- unique(data[
          get(columnas) %in% i][[columna_suma]])
        data[get(columna_suma) %in% registros_procesados,
             "ASIGNACION_NIVEL" := i]
        data <<- data[ASIGNACION_NIVEL != i]
        registros_procesados <- NULL
      }
    )
    episodios_nivel_1 <- rbindlist(episodios_nivel_1)
  }
  
  
  if (!is.null(nivel_2)) {
    episodios_nivel_2 <- descriptiva_basica(
      data = data,
      agrupador = columnas,  
      columna_valor = columna_valor,
      columna_suma = "nro_factura",
      columna_fecha = "fecha_prestacion",
      prestaciones = FALSE
    )[get(columnas) %in% nivel_2]
    registros_procesados <- unique(data[
      get(columnas) %in% i][["nro_factura"]])
    data[get(columna_suma) %in% registros_procesados,
         "ASIGNACION_NIVEL" := "nivel_2"]
    data <- data[ASIGNACION_NIVEL != "nivel_2"]
    registros_procesados <- NULL
  }
  
  if (!is.null(nivel_3)) {
    episodios_nivel_3 <- descriptiva_basica(
      data = data,
      agrupador = columnas,  
      columna_valor = columna_valor,
      columna_suma = "nro_identificacion",
      columna_fecha = "fecha_prestacion",
      prestaciones = FALSE
    )[get(columnas) %in% nivel_3]
    registros_procesados <- unique(data[
      get(columnas) %in% i][["nro_identificacion"]])
    data[get(columna_suma) %in% registros_procesados,
         "ASIGNACION_NIVEL" := "nivel_3"]
    data <- data[ASIGNACION_NIVEL != "nivel_3"]
  }
  
  if (!is.null(nivel_4)) {
    episodios_nivel_4 <- descriptiva_basica(
      data = data,
      agrupador = columnas,  
      columna_valor = columna_valor,
      columna_suma = "",
      columna_fecha = "fecha_prestacion",
      prestaciones = TRUE
    )[get(columnas) %in% nivel_4]
  }
  
  return(
      data.table::rbindlist(
        fill = TRUE,
        list(
          "episodio" = episodios_nivel_1,
          "factura"   = episodios_nivel_2,
          "paciente"  = episodios_nivel_3,
          "prestacion"= episodios_nivel_4
        )
      )
  )
  
}