episodios <- function(data, columnas, columna_valor, columna_suma,
                               columna_sep) {
  setnames(data, columna_valor, "VALOR")
  data[, "VALOR" := numerize(VALOR)]
  data <- data[, list("FREC_PACIENTES" = uniqueN(NRO_IDENTIFICACION),
                      "Suma" = sum(VALOR, na.rm = TRUE),
                      "VAR_PLACEHOLDER" = unique(get(columnas))),
                      by = c(columna_suma, "NRO_IDENTIFICACION", columna_sep)]
  setnames(data, "VAR_PLACEHOLDER", columnas)
  data <- descriptiva(data,
                      columnas = c(columnas, columna_sep),
                      columna_valor = "Suma", prestaciones = FALSE)
  return(data)
  data <- NULL
}