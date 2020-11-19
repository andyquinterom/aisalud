agregar <- function(data, columna_valor, columnas, columna_suma,
                    prestaciones) {
  data <- copy(data)
  setnames(data, columna_valor, "VALOR_CALCULOS")
  data[, "VALOR_CALCULOS" := numerize(VALOR_CALCULOS)]
  if (!prestaciones) {
    data <- data[, list("VALOR_CALCULOS" = sum(VALOR_CALCULOS)),
                 by = c(columna_suma,
                        columnas[columnas != columna_suma])]
  }
  data <- data[, list("Frecuencia" = length(VALOR_CALCULOS),
                      "columna_valor" = sum(VALOR_CALCULOS, na.rm = TRUE),
                      "Promedio" = mean(VALOR_CALCULOS, na.rm = TRUE)),
               by = c(columnas)]
  setnames(data, "columna_valor", columna_valor)
  return(data)
  data <- NULL
  
}

`%notin%` = Negate(`%in%`)