outliers_percentil <- function(data, columna, columna_valor, percentil,
                             frecuencia = 1) {
  
  data <- as.data.frame(data)
  data <- data[, c('nro_identificacion', columna, columna_valor)]
  setnames(data, c('nro_identificacion', columna, "valor_calculos"))
  data$valor_calculos <- numerize(data$valor_calculos)
  data <- data.table(data, key = 'nro_identificacion')
  data <- data[, list("valor_calculos" = sum(valor_calculos)),
               by = c('nro_identificacion', columna)]
  datapacientes <- data
  data <- data[, list("condicion" = quantile(valor_calculos,
                                             probs = percentil,
                                             na.rm = TRUE),
                      "frec" = length(valor_calculos)), by = c(columna)]  
  data <- data[frec >= frecuencia]
  data <- merge.data.table(
    x = datapacientes,
    y = data,
    by = columna
  )
  data[, "diferencia" := condicion - valor_calculos]
  data <- data[diferencia < 0]
  setorder(data, -valor_calculos)
  
  data <- data[, list(get(columna), nro_identificacion, valor_calculos)]
  
  setnames(data, c(columna, "nro_identificacion", "valor_calculos"))
  
  return(data)
  
}

outliers_iqr <- function(data, columna, columna_valor, multiplicativo,
                        frecuencia = 1) {

  multiplicativo <- numerize(multiplicativo)
  data <- as.data.frame(data)
  data <- data[, c('nro_identificacion', columna, columna_valor)]
  setnames(data, c('nro_identificacion', columna, "valor_calculos"))
  data$valor_calculos <- numerize(data$valor_calculos)
  data <- data.table(data, key= 'nro_identificacion')
  data <- data[, list("valor_calculos" = sum(valor_calculos, na.rm = TRUE)),
               by = c('nro_identificacion', columna)]
  datapacientes <- data
  data <- data[, list(
    "condicion1" = quantile(valor_calculos, probs = 0.75, na.rm = TRUE)+
      (IQR(valor_calculos, na.rm = TRUE)*multiplicativo), 
    "condicion2" = quantile(valor_calculos, probs = 0.25, na.rm = TRUE)-
      (IQR(valor_calculos, na.rm = TRUE)*multiplicativo), 
    "frec" = length(valor_calculos)), by = c(columna)]  
  data <- data[frec >= frecuencia]
  
  data <- merge.data.table(
    x = datapacientes,
    y = data,
    by = columna
  )
  
  data[, "diferencia_1" := condicion1 - valor_calculos]
  data[, "diferencia_2" := condicion2 - valor_calculos]
  
  data <- rbind(
    data[diferencia_1 < 0],
    data[diferencia_2 > 0]
  )
  
  setorder(data, -valor_calculos)
  
  data <- data[, list(get(columna), nro_identificacion, valor_calculos)]
  
  setnames(data, c(columna, "nro_identificacion", "valor_calculos"))
  
  return(data)

}

