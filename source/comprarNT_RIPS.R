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

descriptiva_basica <- function(data, agrupador, columna_valor, prestaciones, 
                              columna_fecha) {
  setnames(data, columna_valor, "VALOR")
  data[, "VALOR" := numerize(VALOR)]
  data[, "MES_ANIO_NUM" := lubridate::year(
    data[[columna_fecha]])*100 + lubridate::month(data[[columna_fecha]])]
  data[, "MES_ANIO" := paste(lubridate::year(
    data[[columna_fecha]]),
    mes_spanish(lubridate::month(data[[columna_fecha]])),
    sep = " - ")]
  columnas = c(agrupador, "MES_ANIO_NUM", "MES_ANIO")
  if (!prestaciones) {
    data <- data[, list("VALOR" = sum(VALOR)),
                 by = c('NRO_IDENTIFICACION',
                        columnas[columnas != 'NRO_IDENTIFICACION'])]
  }
  data <- data[, list("Frecuencia" = length(VALOR),
                      "Suma" = sum(VALOR, na.rm = TRUE)),
                      by = c(columnas)]
  setnames(data, c(columnas, "Frecuencia", "Suma"))
  
  return(data[order(MES_ANIO_NUM)])
  data <- NULL
}

descriptiva_basica_trans <- function(data, agrupador, frec = TRUE, suma = TRUE) {
  meses <- as.list(unique(data$MES_ANIO))
  agrupCompletos <- data.table(agrupador = unique(data[[agrupador]]))
  setnames(agrupCompletos, agrupador)
  dataSep <- lapply(
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

  return(cbind.fill(dataSep, agrupador = agrupador)[[1]])
  
}

multiplicar_cme <- function(frecs, nota_tecnica) {
  setnames(frecs, 1, "AGRUPADOR")
  agrupadores_compartidos <- intersect(nota_tecnica[["AGRUPADOR"]], 
                                frecs[["AGRUPADOR"]])
  nota_tecnica <- nota_tecnica[AGRUPADOR %in% agrupadores_compartidos]
  frecs <- frecs[AGRUPADOR %in% agrupadores_compartidos]
  
  nota_tecnica <- nota_tecnica[order(AGRUPADOR)]
  frecs <- frecs[order(AGRUPADOR)]
  
  return(
    as.matrix(apply(frecs[, -c("AGRUPADOR")], 2, as.numeric)) * 
      numerize(nota_tecnica[["CME"]])
    )
  
}

diferencia_valor_rips <- function(sumas, nota_tecnica, porcentaje = FALSE) {
  setnames(sumas, 1, "AGRUPADOR")
  agrupadores_compartidos <- intersect(nota_tecnica[["AGRUPADOR"]],
                                sumas[["AGRUPADOR"]])
  nota_tecnica <- nota_tecnica[
    AGRUPADOR %in% agrupadores_compartidos][order(AGRUPADOR)]
  sumas <- sumas[AGRUPADOR %in% agrupadores_compartidos][order(AGRUPADOR)]
  
  nota_tecnica[, "CME" := numerize(CME)]
  nota_tecnica[, "VALOR_MES" := numerize(VALOR_MES)]
  
  numero_meses <- 2:ncol(sumas)
  
  sumas[, (numero_meses) := lapply(.SD, numerize), .SDcols = numero_meses]
  
  if (porcentaje) {
    return(
      as.data.table(
        append(
          list(AGRUPADOR = sumas$AGRUPADOR),
          as.data.frame(
            sumas[, c(numero_meses) , with = FALSE] / 
              nota_tecnica[["VALOR_MES"]])))
    )
  } else {
    return(
      as.data.table(
        append(
          list(AGRUPADOR = sumas$AGRUPADOR),
          as.data.frame(
            sumas[, c(numero_meses), with = FALSE] -
              nota_tecnica[["VALOR_MES"]])))
    )
  }
}

diferencia_valor_cme <- function(frecs, nota_tecnica, porcentaje = FALSE) {
  setnames(frecs, 1, "AGRUPADOR")
  agrupadores_compartidos <- intersect(nota_tecnica[["AGRUPADOR"]],
                                frecs[["AGRUPADOR"]])
  nota_tecnica <- nota_tecnica[
    AGRUPADOR %in% agrupadores_compartidos][order(AGRUPADOR)]
  frecs <- frecs[AGRUPADOR %in% agrupadores_compartidos][order(AGRUPADOR)]
  
  nota_tecnica[["CME"]] <- numerize(nota_tecnica[["CME"]])
  nota_tecnica[["VALOR_MES"]] <- numerize(nota_tecnica[["VALOR_MES"]])
  
  numero_meses <- 2:ncol(frecs)
  frecs[, (numero_meses) := lapply(.SD, numerize), .SDcols = numero_meses]
  
  if (porcentaje) {
    return(
      as.data.table(
        append(list(AGRUPADOR = frecs$AGRUPADOR),
               as.data.frame((
                 frecs[, c(numero_meses) , with = FALSE] * 
                   numerize(nota_tecnica[["CME"]])/
                   nota_tecnica[["VALOR_MES"]]))))
    )
  } else {
     return(
      as.data.table(
        append(list(AGRUPADOR = frecs$AGRUPADOR), 
               as.data.frame((
                 frecs[, c(numero_meses) , with = FALSE] * 
                   numerize(nota_tecnica[["CME"]]) - 
                   nota_tecnica[["VALOR_MES"]]))))
    )
  }
}

diferencias_totales <- function(frecs, sumas, nota_tecnica) {

  setnames(frecs, 1, "AGRUPADOR")
  setnames(sumas, 1, "AGRUPADOR")
  agrupadores_compartidos <- intersect(nota_tecnica[["AGRUPADOR"]],
                                frecs[["AGRUPADOR"]])
  valor_ejecutar_mes <- sum(nota_tecnica[["VALOR_MES"]], na.rm = TRUE)
  nota_tecnica <- nota_tecnica[
    AGRUPADOR %in% agrupadores_compartidos][order(AGRUPADOR)]
  frecs <- frecs[AGRUPADOR %in% agrupadores_compartidos][order(AGRUPADOR)]
  sumas <- sumas[AGRUPADOR %in% agrupadores_compartidos][order(AGRUPADOR)]
  
  nota_tecnica[, "CME" := numerize(CME)]
  nota_tecnica[, "VALOR_MES" := numerize(VALOR_MES)]
  
  total_mes_rips <- data.table(
    "Mes" = c(colnames(sumas[, -c(1)])),
    "Total" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE)) -
                       valor_ejecutar_mes),
    "%" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE)) /
              valor_ejecutar_mes)
  )
  
  total_agrupador_rips <- data.table(
    "Agrupador" = c(sumas[["AGRUPADOR"]]),
    "Total" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE)) -
                       (nota_tecnica[["VALOR_MES"]] * ncol(sumas[, -c(1)]))),
    "%" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE)) /
              (nota_tecnica[["VALOR_MES"]] * ncol(sumas[, -c(1)])))
  )
  
  numero_meses <- 2:ncol(frecs)
  frecs[, (numero_meses) := lapply(.SD, numerize), .SDcols = numero_meses]
  
  sumas_cme <- frecs[, c(numero_meses) , with = FALSE] * 
    numerize(nota_tecnica[["CME"]])
  
  total_mes_cme <- data.table(
    "Mes" = c(colnames(sumas[, -c(1)])),
    "Total" = c(as.vector(apply(sumas_cme, 2, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas_cme, 2, sum, na.rm = TRUE)) - 
                       valor_ejecutar_mes),
    "%" = c(as.vector(apply(sumas_cme, 2, sum, na.rm = TRUE)) /
              valor_ejecutar_mes)
  )
  
  total_agrupador_cme <- data.table(
    "Mes" = c(sumas[["AGRUPADOR"]]),
    "Total" = c(as.vector(apply(sumas_cme, 1, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas_cme, 1, sum, na.rm = TRUE)) -
                       (nota_tecnica[["VALOR_MES"]] * ncol(sumas[, -c(1)]))),
    "%" = c(as.vector(apply(sumas_cme, 1, sum, na.rm = TRUE)) /
              (nota_tecnica[["VALOR_MES"]] * ncol(sumas[, -c(1)])))
  )
  
  valor_ejecutar <- valor_ejecutar_mes * ncol(sumas[, -c(1)])
  valor_ejecutado_rips <- sum(sumas[, -c(1)], na.rm = TRUE)
  valor_ejecutado_cme <- sum(sumas_cme, na.rm = TRUE)
  
  totales <- data.table(
    "Detalle" = c("Valor a ejecutar", "Ejecutado RIPS", "Ejecutado CME"),
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


