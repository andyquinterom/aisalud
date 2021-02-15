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
  
  print("Descriptiva: convirtiendo valor a numérico.")
  
  data <- data %>%
    mutate(valor_calculos = as.numeric(!!as.name(columna_valor)))
  
  if (!prestaciones) {
    print("Descriptiva: calculando valor por paciente.")
    data <- data %>%
      group_by(!!!rlang::syms(c(columna_suma, columnas))) %>%
      summarise(valor_calculos = sum(valor_calculos, na.rm = TRUE))
  }
  
  print("Descriptiva: resumiendo los datos.")
  data_descriptiva <- data %>%
    group_by(!!!rlang::syms(columnas)) %>%
    arrange(valor_calculos) %>%
    summarise(
      "Frecuencia" = n(),
      "Suma" = sum(valor_calculos, na.rm = TRUE),
      "Media" = round(mean(valor_calculos, na.rm = TRUE),2),
      "P25" = round(quantile(valor_calculos, probs = 0.25, na.rm = TRUE),2),
      "P50" = round(quantile(valor_calculos, probs = 0.5, na.rm = TRUE),2),
      "P75" = round(quantile(valor_calculos, probs = 0.75, na.rm = TRUE),2),
      "P90" = round(quantile(valor_calculos, probs = 0.9, na.rm = TRUE),2),
      "Desv.tipica" = round(sd(valor_calculos, na.rm = TRUE),2),
      "Coef.var" = round(na_if(sd(valor_calculos, na.rm = TRUE), 0)/
                           na_if(mean(valor_calculos, na.rm = TRUE), 0), 2),
      "Min." = min(valor_calculos, na.rm = TRUE),
      "Max." = max(valor_calculos, na.rm = TRUE),
      "Rango" = max(valor_calculos, na.rm = TRUE) - 
        min(valor_calculos, na.rm = TRUE)
    )
  
  print("Descriptiva: datos resumidos.")
  
  fields <- data_descriptiva %>% colnames()
  
  if (".add" %in% fields) {
    data_descriptiva <- data_descriptiva %>%
      select(-`.add`)
  }
  
  print("Descriptiva: descargando los datos.")
  
  data_descriptiva <- data_descriptiva %>%
    collect()
  
  print("Descriptiva: datos descargados.")
  
  setDT(data_descriptiva)
  
  setnames(
    data_descriptiva,
    c(columnas,
      "Frecuencia",
      "Suma", "Media",
      "P25",
      "P50",
      "P75",
      "P90",
      "Desv.tipica",
      "Coef.var",
      "Min.",
      "Max.",
      "Rango")
  )

  return(list(
    "descriptiva" = data_descriptiva,
    "data" = data
  ))
}

extractCol <- function(x, col) {
    x <- as.data.table(x)
    vector <- x[, col, with = FALSE]
    return(unlist(vector)) 
    x <- NULL
}




