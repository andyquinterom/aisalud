
episodios_jerarquia <- function(data, columnas, columna_valor, columna_suma,
                                columna_sep, nivel_1, nivel_2, nivel_3, 
                                nivel_4, return_list = FALSE) {
  
  # data[, "ASIGNACION_NIVEL" := ""]
  data <- data %>% mutate(ASIGNACION_NIVEL = "")
  
  episodios_nivel_1 <- data.table()
  episodios_nivel_2 <- data.table()
  episodios_nivel_3 <- data.table()
  episodios_nivel_4 <- data.table()
  
  data_episodios <- NULL
  if (!is.null(nivel_1)) {
    
    index_episodios <- data.frame(
      index = 1:length(nivel_1),
      agrupador = nivel_1
    )
    colnames(index_episodios) <- c("index", columnas)
    
    episodios <- data %>%
      select(!!as.name(columna_suma), !!as.name(columnas)) %>%
      filter(!!as.name(columnas) %in% nivel_1) %>%
      distinct() %>%
      right_join(index_episodios, copy = TRUE) %>%
      arrange(index) %>%
      group_by(!!as.name(columna_suma)) %>%
      mutate(!!columnas := first(!!as.name(columnas))) %>%
      ungroup() %>%
      distinct(!!as.name(columna_suma), !!as.name(columnas))
    
    if (!is.null(columna_sep)) {
      data_episodios <- data %>%
        group_by(!!!rlang::syms(unique(c(columna_suma, columna_sep))))
    } else {
      data_episodios <- data %>%
        group_by(!!as.name(columna_suma))
    }
    data_episodios <- data_episodios %>%
      summarise(valor_calculos = sum(!!as.name(columna_valor), na.rm = TRUE)) %>%
      right_join(episodios)
    data_temp <- descriptiva(
      data = data_episodios,
      columnas = c(columnas, columna_sep),
      columna_valor = "valor_calculos",
      columna_suma = columna_suma,
      prestaciones = FALSE
    )
    episodios_nivel_1 <- data_temp[["descriptiva"]]
    episodios <- episodios %>%
      select(!!as.name(columna_suma))
    data <- data %>%
      anti_join(episodios)
    data_temp <- NULL
    print("Nivel 1: Completo.")
  }
  
  episodios_nivel_2_data <- NULL
  if (!is.null(nivel_2)) {
    data_temp <- descriptiva(
      data = data,
      columnas = c(columnas, columna_sep),
      columna_valor = columna_valor,
      columna_suma = "nro_factura",
      prestaciones = FALSE
    )
    episodios_nivel_2 <- data_temp[["descriptiva"]] %>%
      filter(!!as.name(columnas) %in% nivel_2)
    episodios_nivel_2_data <- data_temp[["data"]] %>%
      filter(!!as.name(columnas) %in% nivel_2)
    data_temp <- NULL
    print("Nivel 2: Completo.")
    
  }

  episodios_nivel_3_data <- NULL
  if (!is.null(nivel_3)) {
    data_temp <- descriptiva(
      data = data,
      columnas = c(columnas, columna_sep),
      columna_valor = columna_valor,
      columna_suma = "nro_identificacion",
      prestaciones = FALSE
    )
    episodios_nivel_3 <- data_temp[["descriptiva"]] %>%
      filter(!!as.name(columnas) %in% nivel_3)
    episodios_nivel_3_data <- data_temp[["data"]] %>%
      filter(!!as.name(columnas) %in% nivel_3)
    data_temp <- NULL
    print("Nivel 3: Completo.")
    
  }

  episodios_nivel_4_data <- NULL
  if (!is.null(nivel_4)) {
    print("Nivel 4: Generando descriptiva.")
    data_temp <- descriptiva(
      data = data,
      columnas = c(columnas, columna_sep),
      columna_valor = columna_valor,
      columna_suma = "",
      prestaciones = TRUE
    )
    print("Nivel 4: Descriptiva generada.")
    episodios_nivel_4 <- data_temp[["descriptiva"]] %>%
      filter(!!as.name(columnas) %in% nivel_4)
    episodios_nivel_4_data <- data_temp[["data"]] %>%
      filter(!!as.name(columnas) %in% nivel_4)
    data_temp <- NULL
    print("Nivel 4: Completo.")
    
  }
  
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
        "data" = list(
          "episodios" = data_episodios,
          "facturas" = episodios_nivel_2_data,
          "pacientes" = episodios_nivel_3_data,
          "prestaciones" = episodios_nivel_4_data
        )
      )
    }
  )
  
}
