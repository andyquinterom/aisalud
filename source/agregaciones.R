agregar <- function(data, columna_valor, columnas, columna_suma,
                    prestaciones) {
  data <- copy(data)
  setnames(data, columna_valor, "VALOR_CALCULOS")
  data[, "VALOR_CALCULOS" := numerize(VALOR)]
  if (!prestaciones) {
    data <- data[, list("VALOR_CALCULOS" = sum(VALOR)),
                 by = c(columna_suma,
                        columnas[columnas != columna_suma])]
  }
  data <- data[, list("Frecuencia" = length(VALOR),
                      "columna_valor" = sum(VALOR, na.rm = TRUE),
                      "Promedio" = mean(VALOR, na.rm = TRUE)),
               by = c(columnas)]
  setnames(data, "columna_valor", columna_valor)
  return(data)
  data <- NULL
  
}

`%notin%` = Negate(`%in%`)