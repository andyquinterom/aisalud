agregar <- function(data, columna_valor, columnas, columna_suma,
                    prestaciones) {
  data <- copy(data)
  setnames(data, columna_valor, "valor_calculos")
  data[, "valor_calculos" := numerize(valor_calculos)]
  if (!prestaciones) {
    data <- data[, list("valor_calculos" = sum(valor_calculos)),
                 by = c(columna_suma,
                        columnas[columnas != columna_suma])]
  }
  data <- data[, list("Frecuencia" = length(valor_calculos),
                      "columna_valor" = sum(valor_calculos, na.rm = TRUE),
                      "Promedio" = mean(valor_calculos, na.rm = TRUE)),
               by = c(columnas)]
  setnames(data, "columna_valor", columna_valor)
  return(data)
  data <- NULL
  
}

`%notin%` = Negate(`%in%`)