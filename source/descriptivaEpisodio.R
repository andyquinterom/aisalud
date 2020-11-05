episodios_descriptiva <- function(data, columnas, columna_valor, columna_suma,
                                  columna_sep) {
  setnames(data, columna_valor, "VALOR")
  data[, "VALOR" := numerize(VALOR)]
  data <- data[, list("FREC_PACIENTES" = uniqueN(get(columna_suma)),
                      "Suma" = sum(VALOR, na.rm = TRUE),
                      "VAR_COLUMNAS" = unique(get(columnas))),
                      by = c(columna_suma, columna_sep)]
  setnames(data, "VAR_COLUMNAS", columnas)
  data <- descriptiva(data,
                      columnas = c(columnas, columna_sep),
                      columna_suma = columna_suma,
                      columna_valor = "Suma", prestaciones = FALSE)
  return(data)
  data <- NULL
}

episodios_jerarquia <- function(data, columnas, columna_valor, columna_suma,
                                columna_sep, nivel_1, nivel_2, nivel_3, 
                                nivel_4) {
  
  data[, "ASIGNACION_NIVEL" := ""]
  
  episodios_nivel_1 <- data.table()
  episodios_nivel_2 <- data.table()
  episodios_nivel_3 <- data.table()
  episodios_nivel_4 <- data.table()
  
  if (!is.null(nivel_1)) {
    episodios_nivel_1 <- list()
    lapply(
      X = nivel_1,
      FUN = function(i) {
        episodios_nivel_1[[i]] <<- episodios_descriptiva(
          data = data,
          columnas = columnas,
          columna_valor = columna_valor,
          columna_suma = columna_suma,
          columna_sep = columna_sep
        )[get(columnas) %in% i]
        registros_procesados <- unique(data[
          get(columnas) %in% i][[columna_suma]])
        data[get(columna_suma) %in% registros_procesados,
             "ASIGNACION_NIVEL" := i]
        data <<- data[ASIGNACION_NIVEL != i]
        registros_procesados <- NULL
      }
    )
    episodios_nivel_1 <- rbindlist(episodios_nivel_1)
  }
  
  
  if (!is.null(nivel_2)) {
    episodios_nivel_2 <- descriptiva(
      data = data,
      columnas = c(columnas, columna_sep),
      columna_valor = columna_valor,
      columna_suma = "NRO_FACTURA",
      prestaciones = FALSE
    )[get(columnas) %in% nivel_2]
    registros_procesados <- unique(data[
      get(columnas) %in% i][["NRO_FACTURA"]])
    data[get(columna_suma) %in% registros_procesados,
         "ASIGNACION_NIVEL" := "nivel_2"]
    data <- data[ASIGNACION_NIVEL != "nivel_2"]
    registros_procesados <- NULL
  }
  
  if (!is.null(nivel_3)) {
    episodios_nivel_3 <- descriptiva(
      data = data,
      columnas = c(columnas, columna_sep),
      columna_valor = columna_valor,
      columna_suma = "NRO_IDENTIFICACION",
      prestaciones = FALSE
    )[get(columnas) %in% nivel_3]
    registros_procesados <- unique(data[
      get(columnas) %in% i][["NRO_IDENTIFICACION"]])
    data[get(columna_suma) %in% registros_procesados,
         "ASIGNACION_NIVEL" := "nivel_3"]
    data <- data[ASIGNACION_NIVEL != "nivel_3"]
  }
  
  if (!is.null(nivel_4)) {
    episodios_nivel_4 <- descriptiva(
      data = data,
      columnas = c(columnas, columna_sep),
      columna_valor = columna_valor,
      columna_suma = "",
      prestaciones = TRUE
    )[get(columnas) %in% nivel_4]
  }
  
  return(
    rbind(
      episodios_nivel_1,
      episodios_nivel_2,
      episodios_nivel_3,
      episodios_nivel_4
    )
  )
  
}