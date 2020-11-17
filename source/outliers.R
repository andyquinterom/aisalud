outliers_percentil <- function(data, columna, columna_valor, percentil,
                             frecuencia = 1) {
  
  data <- as.data.frame(data)
  data <- data[, c('NRO_IDENTIFICACION', columna, columna_valor)]
  setnames(data, c('NRO_IDENTIFICACION', columna, "VALOR"))
  data$VALOR <- numerize(data$VALOR)
  data <- data.table(data, key= 'NRO_IDENTIFICACION')
  data <- data[, list("VALOR" = sum(VALOR)),
               by = c('NRO_IDENTIFICACION', columna)]
  datapacientes <- data
  data <- data[, list("Condicion" = quantile(VALOR,
                                             probs = percentil,
                                             na.rm = TRUE),
                      "Frec" = length(VALOR)), by = c(columna)]  
  data <- data[Frec >= frecuencia]
  data <- merge.data.table(
    x = datapacientes,
    y = data,
    by = columna
  )
  data[, "DIFERENCIA" := Condicion - VALOR]
  data <- data[DIFERENCIA < 0]
  setorder(data, -VALOR)
  
  data <- data[, list(get(columna), NRO_IDENTIFICACION, VALOR)]
  
  setnames(data, c(columna, "NRO_IDENTIFICACION", "VALOR"))
  
  return(data)
  
}

outliers_iqr <- function(data, columna, columna_valor, multiplicativo,
                        frecuencia = 1) {

  multiplicativo <- numerize(multiplicativo)
  data <- as.data.frame(data)
  data <- data[, c('NRO_IDENTIFICACION', columna, columna_valor)]
  setnames(data, c('NRO_IDENTIFICACION', columna, "VALOR"))
  data$VALOR <- numerize(data$VALOR)
  data <- data.table(data, key= 'NRO_IDENTIFICACION')
  data <- data[, list("VALOR" = sum(VALOR, na.rm = TRUE)),
               by = c('NRO_IDENTIFICACION', columna)]
  datapacientes <- data
  data <- data[, list(
    "Condicion1" = quantile(VALOR, probs = 0.75, na.rm = TRUE)+
      (IQR(VALOR, na.rm = TRUE)*multiplicativo), 
    "Condicion2" = quantile(VALOR, probs = 0.25, na.rm = TRUE)-
      (IQR(VALOR, na.rm = TRUE)*multiplicativo), 
    "Frec" = length(VALOR)), by = c(columna)]  
  data <- data[Frec >= frecuencia]
  
  data <- merge.data.table(
    x = datapacientes,
    y = data,
    by = columna
  )
  
  data[, "DIFERENCIA_1" := Condicion1 - VALOR]
  data[, "DIFERENCIA_2" := Condicion2 - VALOR]
  
  data <- rbind(
    data[DIFERENCIA_1 < 0],
    data[DIFERENCIA_2 > 0]
  )
  
  setorder(data, -VALOR)
  
  data <- data[, list(get(columna), NRO_IDENTIFICACION, VALOR)]
  
  setnames(data, c(columna, "NRO_IDENTIFICACION", "VALOR"))
  
  return(data)

}

