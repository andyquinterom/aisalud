numerize <- function(x) {
  return(as.numeric(as.character(x)))
}
coe.variacion <- function(x) {
  return(sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))
}
rango <- function(x) {
  return(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

descriptiva <- function(data, columnas, columna_valor, columna_suma,
                        prestaciones) {
  data <- copy(data)
  setnames(data, columna_valor, "valor_calculos")
  data[, "valor_calculos" := numerize(valor_calculos)]
  if (!prestaciones) {
    data <- data[, list("valor_calculos" = sum(valor_calculos)),
                by = c(columna_suma, 
                       columnas[columnas != columna_suma])]
  }
  data_descriptiva <- data[, list(
    "Frecuencia" = length(valor_calculos),
     "Suma" = sum(valor_calculos, na.rm = TRUE),
     "Media" = round(mean(valor_calculos, na.rm = TRUE),2),
     "P50" = round(quantile(valor_calculos, probs = 0.5, na.rm = TRUE),2),
     "P75" = round(quantile(valor_calculos, probs = 0.75, na.rm = TRUE),2),
     "P90" = round(quantile(valor_calculos, probs = 0.9, na.rm = TRUE),2),
     "Media truncada 10%" = round(mean(valor_calculos, trim=0.1, na.rm = TRUE),2),
     "Media truncada 5%" = round(mean(valor_calculos, trim=0.05, na.rm = TRUE),2),
     "Desv.tipica" = round(sd(valor_calculos, na.rm = TRUE),2),
     "Coef.var" = round(coe.variacion(valor_calculos),2),
     "Min." = min(valor_calculos, na.rm = TRUE),
     "Max." = max(valor_calculos, na.rm = TRUE),
     "Rango" = rango(valor_calculos)
  ), by = c(columnas)]
  
  setnames(data_descriptiva,
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

  return(list(
    "descriptiva" = data_descriptiva,
    "data" = data
  ))
  data <- NULL
}

extractCol <- function(x, col) {
    x <- as.data.table(x)
    vector <- x[, col, with = FALSE]
    return(unlist(vector)) 
    x <- NULL
}




