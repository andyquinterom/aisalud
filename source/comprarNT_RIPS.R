require(data.table)
require(lubridate)
require(dplyr)

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

mesSpanish <- function(x) {
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

descriptivaBasica <- function(data, agrupador, columna_valor, prestaciones, 
                              columna_fecha) {
  setnames(data, columna_valor, "VALOR")
  data[, "VALOR" := numerize(VALOR)]
  data[, "MES_ANIO_NUM" := lubridate::year(
    data[[columna_fecha]])*100 + lubridate::month(data[[columna_fecha]])]
  data[, "MES_ANIO" := paste(lubridate::year(
    data[[columna_fecha]]),
    mesSpanish(lubridate::month(data[[columna_fecha]])),
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

descriptivaBasicaTrans <- function(data, agrupador, frec = TRUE, suma = TRUE) {
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

multiplicarCME <- function(frecs, NT) {
  setnames(frecs, 1, "AGRUPADOR")
  agrupCompartidos <- intersect(NT[["AGRUPADOR"]], frecs[["AGRUPADOR"]])
  NT <- NT[AGRUPADOR %in% agrupCompartidos]
  frecs <- frecs[AGRUPADOR %in% agrupCompartidos]
  
  NT <- NT[order(AGRUPADOR)]
  frecs <- frecs[order(AGRUPADOR)]
  
  return(
    as.matrix(apply(frecs[, -c("AGRUPADOR")], 2, as.numeric)) * 
      numerize(NT[["CME"]])
    )
  
}

diferenciaValorRIPS <- function(sumas, NT, porcentaje = FALSE) {
  setnames(sumas, 1, "AGRUPADOR")
  agrupCompartidos <- intersect(NT[["AGRUPADOR"]], sumas[["AGRUPADOR"]])
  NT <- NT[AGRUPADOR %in% agrupCompartidos][order(AGRUPADOR)]
  sumas <- sumas[AGRUPADOR %in% agrupCompartidos][order(AGRUPADOR)]
  
  NT[, "CME" := numerize(CME)]
  NT[, "VALOR_MES" := numerize(VALOR_MES)]
  
  numero_meses <- 2:ncol(sumas)
  
  sumas[, (numero_meses) := lapply(.SD, numerize), .SDcols = numero_meses]
  
  if (porcentaje) {
    return(
      as.data.table(
        append(
          list(AGRUPADOR = sumas$AGRUPADOR),
          as.data.frame(
            sumas[, c(numero_meses) , with = FALSE] / NT[["VALOR_MES"]])))
    )
  } else {
    return(
      as.data.table(
        append(
          list(AGRUPADOR = sumas$AGRUPADOR),
          as.data.frame(
            sumas[, c(numero_meses), with = FALSE] - NT[["VALOR_MES"]])))
    )
  }
}

diferenciaValorCME <- function(frecs, NT, porcentaje = FALSE) {
  setnames(frecs, 1, "AGRUPADOR")
  agrupCompartidos <- intersect(NT[["AGRUPADOR"]], frecs[["AGRUPADOR"]])
  NT <- NT[AGRUPADOR %in% agrupCompartidos][order(AGRUPADOR)]
  frecs <- frecs[AGRUPADOR %in% agrupCompartidos][order(AGRUPADOR)]
  
  NT[["CME"]] <- numerize(NT[["CME"]])
  NT[["VALOR_MES"]] <- numerize(NT[["VALOR_MES"]])
  
  numero_meses <- 2:ncol(frecs)
  frecs[, (numero_meses) := lapply(.SD, numerize), .SDcols = numero_meses]
  
  if (porcentaje) {
    return(
      as.data.table(
        append(list(AGRUPADOR = frecs$AGRUPADOR),
               as.data.frame((
                 frecs[, c(numero_meses) , with = FALSE] * 
                   numerize(NT[["CME"]])/NT[["VALOR_MES"]]))))
    )
  } else {
     return(
      as.data.table(
        append(list(AGRUPADOR = frecs$AGRUPADOR), 
               as.data.frame((
                 frecs[, c(numero_meses) , with = FALSE] * 
                   numerize(NT[["CME"]]) - NT[["VALOR_MES"]]))))
    )
  }
}

diferenciasTotales <- function(frecs, sumas, NT) {

  setnames(frecs, 1, "AGRUPADOR")
  setnames(sumas, 1, "AGRUPADOR")
  agrupCompartidos <- intersect(NT[["AGRUPADOR"]], frecs[["AGRUPADOR"]])
  valor_ejecutar_mes <- sum(NT[["VALOR_MES"]], na.rm = TRUE)
  NT <- NT[AGRUPADOR %in% agrupCompartidos][order(AGRUPADOR)]
  frecs <- frecs[AGRUPADOR %in% agrupCompartidos][order(AGRUPADOR)]
  sumas <- sumas[AGRUPADOR %in% agrupCompartidos][order(AGRUPADOR)]
  
  NT[, "CME" := numerize(CME)]
  NT[, "VALOR_MES" := numerize(VALOR_MES)]
  
  totalMesRIPS <- data.table(
    "Mes" = c(colnames(sumas[, -c(1)])),
    "Total" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE)) -
                       valor_ejecutar_mes),
    "%" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE)) /
              valor_ejecutar_mes)
  )
  
  totalAgrupadorRIPS <- data.table(
    "Agrupador" = c(sumas[["AGRUPADOR"]]),
    "Total" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE)) -
                       (NT[["VALOR_MES"]] * ncol(sumas[, -c(1)]))),
    "%" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE)) /
              (NT[["VALOR_MES"]] * ncol(sumas[, -c(1)])))
  )
  
  numero_meses <- 2:ncol(frecs)
  frecs[, (numero_meses) := lapply(.SD, numerize), .SDcols = numero_meses]
  
  sumasCME <- frecs[, c(numero_meses) , with = FALSE] * numerize(NT[["CME"]])
  
  totalMesCME <- data.table(
    "Mes" = c(colnames(sumas[, -c(1)])),
    "Total" = c(as.vector(apply(sumasCME, 2, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumasCME, 2, sum, na.rm = TRUE)) - 
                       valor_ejecutar_mes),
    "%" = c(as.vector(apply(sumasCME, 2, sum, na.rm = TRUE)) /
              valor_ejecutar_mes)
  )
  
  totalAgrupadorCME <- data.table(
    "Mes" = c(sumas[["AGRUPADOR"]]),
    "Total" = c(as.vector(apply(sumasCME, 1, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumasCME, 1, sum, na.rm = TRUE)) -
                       (NT[["VALOR_MES"]] * ncol(sumas[, -c(1)]))),
    "%" = c(as.vector(apply(sumasCME, 1, sum, na.rm = TRUE)) /
              (NT[["VALOR_MES"]] * ncol(sumas[, -c(1)])))
  )
  
  valorEjecutar <- valor_ejecutar_mes * ncol(sumas[, -c(1)])
  valorEjecutadoRips <- sum(sumas[, -c(1)], na.rm = TRUE)
  valorEjecutadoCME <- sum(sumasCME, na.rm = TRUE)
  
  totales <- data.table(
    "Detalle" = c("Valor a ejecutar", "Ejecutado RIPS", "Ejecutado CME"),
    "Valor" = c(valorEjecutar, valorEjecutadoRips, valorEjecutadoCME),
    "Diferencias" = c(NA,
                      valorEjecutadoRips - valorEjecutar,
                      valorEjecutadoCME  - valorEjecutar),
    "%" = c(NA,
                      valorEjecutadoRips/valorEjecutar,
                      valorEjecutadoCME/valorEjecutar)
  )
  
  
  
  return(
    list(
      totalMesRIPS = totalMesRIPS,
      totalMesCME  = totalMesCME,
      totalAgrupadorRIPS = totalAgrupadorRIPS,
      totalAgrupadorCME  = totalAgrupadorCME,
      totales = totales
    )
  )
  
}


