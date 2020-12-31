episodios_descriptiva <- function(data, columnas, columna_valor, columna_suma,
                                  columna_sep) {
  data <- data %>%
    mutate(valor_calculos = as.numeric(!!as.name(columna_valor)))
  
  if (!is.null(columna_sep)) {
    data <- data %>%
      group_by(!!as.name(columna_suma), 
               !!as.name(columna_sep))
  } else {
    data <- data %>%
      group_by(!!as.name(columna_suma))
  }
  
  facturas_columna <- data %>%
    select(!!as.name(columnas), columna_suma) %>%
    distinct()
  
  data <- data %>%
    summarise(
      frec_pacientes = n_distinct(!!as.name(columna_suma)),
      Suma = sum(valor_calculos, na.rm = TRUE)) %>%
    merge(facturas_columna, all = TRUE)

  datos_descriptiva <- descriptiva(
    data,
    columnas = c(columnas, columna_sep),
    columna_suma = columna_suma,
    columna_valor = "Suma", prestaciones = FALSE)
  return(datos_descriptiva)
  # data <- NULL
}

episodios_jerarquia <- function(data, columnas, columna_valor, columna_suma,
                                columna_sep, nivel_1, nivel_2, nivel_3, 
                                nivel_4, return_list = FALSE) {
  
  # data[, "ASIGNACION_NIVEL" := ""]
  data <- data %>% mutate(ASIGNACION_NIVEL = "")
  
  episodios_nivel_1 <- data.table()
  episodios_nivel_2 <- data.table()
  episodios_nivel_3 <- data.table()
  episodios_nivel_4 <- data.table()
  
  if (!is.null(nivel_1)) {
    episodios_nivel_1 <- list()
    episodios_nivel_1_data <- list()
    lapply(
      X = nivel_1,
      FUN = function(i) {
        data_temp <- episodios_descriptiva(
          data = data,
          columnas = columnas,
          columna_valor = columna_valor,
          columna_suma = columna_suma,
          columna_sep = columna_sep)[["descriptiva"]]
        episodios_nivel_1[[i]] <<- as.data.table(data_temp)[
          get(columnas) %in% i]
        data_temp <- NULL
        registros_no_procesados <- data %>%
          filter(!(!!as.name(columnas) %in% i)) %>%
          select(!!as.name(columna_suma)) %>%
          distinct() %>%
          collect %>%
          as.list()
        registros_no_procesados <- registros_no_procesados[[1]]
        data <<- data %>%
          filter(!!as.name(columna_suma) %in% registros_no_procesados)
        registros_no_procesados <- NULL
      }
    )
    # episodios_nivel_1_data <- rbindlist(episodios_nivel_1_data)
    episodios_nivel_1 <- rbindlist(episodios_nivel_1)
  }
  
  
  # if (!is.null(nivel_2)) {
  #   data_temp <- descriptiva(
  #     data = data,
  #     columnas = c(columnas, columna_sep),
  #     columna_valor = columna_valor,
  #     columna_suma = "nro_factura",
  #     prestaciones = FALSE
  #   )
  #   episodios_nivel_2 <- data_temp[["descriptiva"]][
  #     get(columnas) %in% nivel_2]
  #   episodios_nivel_2_data <- data_temp[["data"]][
  #     get(columnas) %in% nivel_2]
  #   data_temp <- NULL
  #   registros_procesados <- unique(data[
  #     get(columnas) %in% i][["nro_factura"]])
  #   data[get(columna_suma) %in% registros_procesados,
  #        "ASIGNACION_NIVEL" := "nivel_2"]
  #   data <- data[ASIGNACION_NIVEL != "nivel_2"]
  #   registros_procesados <- NULL
  # }
  # 
  # if (!is.null(nivel_3)) {
  #   data_temp <- descriptiva(
  #     data = data,
  #     columnas = c(columnas, columna_sep),
  #     columna_valor = columna_valor,
  #     columna_suma = "nro_identificacion",
  #     prestaciones = FALSE
  #   )
  #   episodios_nivel_3 <- data_temp[["descriptiva"]][
  #     get(columnas) %in% nivel_3]
  #   episodios_nivel_3_data <- data_temp[["data"]][
  #     get(columnas) %in% nivel_3]
  #   data_temp <- NULL
  #   registros_procesados <- unique(data[
  #     get(columnas) %in% i][["nro_identificacion"]])
  #   data[get(columna_suma) %in% registros_procesados,
  #        "ASIGNACION_NIVEL" := "nivel_3"]
  #   data <- data[ASIGNACION_NIVEL != "nivel_3"]
  # }
  # 
  # if (!is.null(nivel_4)) {
  #   data_temp <- descriptiva(
  #     data = data,
  #     columnas = c(columnas, columna_sep),
  #     columna_valor = columna_valor,
  #     columna_suma = "",
  #     prestaciones = TRUE
  #   )
  #   episodios_nivel_4 <- data_temp[["descriptiva"]][get(columnas) %in% nivel_4]
  #   columnas_select <- c(columnas, columna_sep)
  #   episodios_nivel_4_data <- data_temp[["data"]][
  #     get(columnas) %in% nivel_4][, c(columnas_select, "valor_calculos"), with = FALSE]
  #   data_temp <- NULL
  # }
  
  return(
    if (return_list) {
      list(
        "descriptiva" = list(
          "episodio" = episodios_nivel_1,
          "factura"   = episodios_nivel_2,
          "paciente"  = episodios_nivel_3,
          "prestacion"= episodios_nivel_4
        )
      )
    } else {
      list(
        "descriptiva" = rbind(
          episodios_nivel_1,
          episodios_nivel_2,
          episodios_nivel_3,
          episodios_nivel_4
        ),
        "data" = data.table()
        #   rbind(
        #   fill = TRUE,
        #   episodios_nivel_1_data,
        #   episodios_nivel_2_data,
        #   episodios_nivel_3_data,
        #   episodios_nivel_4_data
        # )
      )
    }
  )
  
}
