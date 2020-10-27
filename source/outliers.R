outliers_percentil <- function(data, columna, columna_valor, percentil,
                             frecuencia = 1) {
  
  data <- as.data.frame(data)
  data <- data[, c('NRO_IDENTIFICACION', columna, columna_valor)]
  colnames(data) <- c('NRO_IDENTIFICACION', columna, "VALOR")
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
  datafinal <- data.table()  
  for (i in unique(data[[columna]])) {
    datatemp <- data.table()
    datatemp <- datapacientes[get(columna) == i]
    condicion <- data[get(columna) == i, Condicion]
    datatemp <- datatemp[VALOR >= condicion]
    datafinal <- rbind(datafinal, datatemp)
  }

  return(datafinal)
  
}

outliers_iqr <- function(data, columna, columna_valor, multiplicativo,
                        frecuencia = 1) {

  multiplicativo <- numerize(multiplicativo)
  data <- as.data.frame(data)
  data <- data[, c('NRO_IDENTIFICACION', columna, columna_valor)]
  colnames(data) <- c('NRO_IDENTIFICACION', columna, "VALOR")
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
  datafinal <- data.table()  
  for (i in unique(data[[columna]])) {
    datatemp <- data.table()
    datatemp <- datapacientes[get(columna) == i]
    condicion1 <- data[get(columna) == i, Condicion1]
    condicion2 <- data[get(columna) == i, Condicion2]
    datatemp <- datatemp[!(VALOR <= condicion1 & VALOR >= condicion2)]
    datafinal <- rbind(datafinal, datatemp)
  }
  
  return(datafinal)

}

