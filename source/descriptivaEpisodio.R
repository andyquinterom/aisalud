episodios_descriptiva <- function(data, columnas, columna_valor, columna_suma,
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
                      columna_suma = "NRO_IDENTIFICACION",
                      columna_valor = "Suma", prestaciones = FALSE)
  return(data)
  data <- NULL
}

episodios_jerarquia <- function(data, columnas, columna_valor, columna_suma,
                                columna_sep, nivel_1, nivel_2, nivel_3) {
  
  data[, "ASIGNACION_NIVEL" := 3]
  
  episodios_nivel_1 <- data.table()
  episodios_nivel_2 <- data.table()
  episodios_nivel_3 <- data.table()
  
  if (!is.null(nivel_1)) {
    episodios_nivel_1 <- episodios_descriptiva(
      data = data,
      columnas = columnas,
      columna_valor = columna_valor,
      columna_suma = columna_suma,
      columna_sep = columna_sep
    )[get(columnas) %in% nivel_1]
    datos_nivel_1 <- unique(
      data[get(columnas) %in% nivel_1][[columna_suma]])
    data[get(columna_suma) %in% datos_nivel_1, "ASIGNACION_NIVEL" := 1]
  }
  
  if (!is.null(nivel_2)) {
    episodios_nivel_2 <- episodios_descriptiva(
      data = data[ASIGNACION_NIVEL != 1],
      columnas = columnas,
      columna_valor = columna_valor,
      columna_suma = columna_suma,
      columna_sep = columna_sep
    )[get(columnas) %in% nivel_2]
    datos_nivel_2 <- unique(
      data[get(columnas) %in% nivel_2][[columna_suma]])
    data[get(columna_suma) %in% datos_nivel_2, "ASIGNACION_NIVEL" := 2]
  }
  
  if (!is.null(nivel_3)) {
    episodios_nivel_3 <- descriptiva(
      data = data[ASIGNACION_NIVEL == 3],
      columnas = c(columnas, columna_sep),
      columna_valor = columna_valor,
      columna_suma = columna_suma,
      prestaciones = FALSE
    )[get(columnas) %in% nivel_3]
  }
  
  return(
    rbind(
      episodios_nivel_1,
      episodios_nivel_2,
      episodios_nivel_3
    )
  )
  
}