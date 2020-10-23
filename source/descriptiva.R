numerize <- function(x) {
  return(as.numeric(as.character(x)))
}
coe.variacion <- function(x) {
  return(sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))
}
rango <- function(x) {
  return(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

descriptiva <- function(data, columnas, columna_valor, prestaciones) {
  # data = as.data.frame(data)
  # data = data[, c('NRO_IDENTIFICACION',columnas, columna_valor)]
  # colnames(data) = c('NRO_IDENTIFICACION', columnas, "VALOR")
  # data$VALOR = numerize(data$VALOR)
  data[, "VALOR" := numerize(VALOR)]
  if (!prestaciones) {
    data <- data[, list("VALOR" = sum(VALOR)),
                by = c('NRO_IDENTIFICACION', 
                       columnas[columnas != 'NRO_IDENTIFICACION'])]
  }
  data <- data[, list(
    "Frecuencia" = length(VALOR),
     "Suma" = sum(VALOR, na.rm = TRUE),
     "Media" = round(mean(VALOR, na.rm = TRUE),2),
     "P50" = round(quantile(VALOR, probs = 0.5, na.rm = TRUE),2),
     "P75" = round(quantile(VALOR, probs = 0.75, na.rm = TRUE),2),
     "P90" = round(quantile(VALOR, probs = 0.9, na.rm = TRUE),2),
     "Media truncada 10%" = round(mean(VALOR, trim=0.1, na.rm = TRUE),2),
     "Media truncada 5%" = round(mean(VALOR, trim=0.05, na.rm = TRUE),2),
     "Desv.tipica" = round(sd(VALOR, na.rm = TRUE),2),
     "Coef.var" = round(coe.variacion(VALOR),2),
     "Min." = min(VALOR, na.rm = TRUE),
     "Max." = max(VALOR, na.rm = TRUE),
     "Rango" = rango(VALOR)
  ), by = c(columnas)]
  
  setnames(data,
           c(columnas, 
             "Frecuencia", 
             "Suma", "Media", 
             "P50", 
             "P75", 
             "P90",
             "Media truncada 10%",
             "Media truncada 5%",
             "Desv.tipica", "Coef.var",
             "Min.",
             "Max.",
             "Rango"))
  # colnames(data) = c(columnas, "Frecuencia", "Suma", "Media", "P50", "P75", "P90", "Media truncada 10%", "Media truncada 5%", "Desv.tipica", "Coef.var", "Min.", "Max.", "Rango")
  return(data)
  data <- NULL
}

extractCol <- function(x, col) {
    x <- as.data.table(x)
    vector <- x[, col, with = FALSE]
    return(unlist(vector)) 
    x <- NULL
}




