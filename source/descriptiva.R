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
  setnames(data, columna_valor, "VALOR_CALCULOS")
  data[, "VALOR_CALCULOS" := numerize(VALOR_CALCULOS)]
  if (!prestaciones) {
    data <- data[, list("VALOR_CALCULOS" = sum(VALOR_CALCULOS)),
                by = c(columna_suma, 
                       columnas[columnas != columna_suma])]
  }
  data_descriptiva <- data[, list(
    "Frecuencia" = length(VALOR_CALCULOS),
     "Suma" = sum(VALOR_CALCULOS, na.rm = TRUE),
     "Media" = round(mean(VALOR_CALCULOS, na.rm = TRUE),2),
     "P50" = round(quantile(VALOR_CALCULOS, probs = 0.5, na.rm = TRUE),2),
     "P75" = round(quantile(VALOR_CALCULOS, probs = 0.75, na.rm = TRUE),2),
     "P90" = round(quantile(VALOR_CALCULOS, probs = 0.9, na.rm = TRUE),2),
     "Media truncada 10%" = round(mean(VALOR_CALCULOS, trim=0.1, na.rm = TRUE),2),
     "Media truncada 5%" = round(mean(VALOR_CALCULOS, trim=0.05, na.rm = TRUE),2),
     "Desv.tipica" = round(sd(VALOR_CALCULOS, na.rm = TRUE),2),
     "Coef.var" = round(coe.variacion(VALOR_CALCULOS),2),
     "Min." = min(VALOR_CALCULOS, na.rm = TRUE),
     "Max." = max(VALOR_CALCULOS, na.rm = TRUE),
     "Rango" = rango(VALOR_CALCULOS)
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




