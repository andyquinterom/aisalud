frecuencias <- function(
  data, agrupador, prestaciones, columna_fecha, columna_suma, 
  intervalo = "mes") {
  
  if (intervalo == "mes") {
    data <- data %>%
      mutate(mes_num = month(!!as.name(columna_fecha)),
             anio_num = year(!!as.name(columna_fecha))) %>%
      mutate(mes_anio_num = anio_num*100 + mes_num)
  } else if (intervalo == "dia") {
    data <- data %>%
      mutate(mes_anio_num = !!as.name(columna_fecha))
  } else if (intervalo == "semana") {
    data <- data %>%
      mutate(mes_num = round((yday(!!as.name(columna_fecha))  - 1)%/%7 + 1),
             anio_num = year(!!as.name(columna_fecha))) %>%
      mutate(mes_anio_num = anio_num*100 + mes_num)
  }
  
  if (!prestaciones) {
    
    data <- data %>%
      group_by(!!as.name(columna_suma), !!!rlang::syms(agrupador)) %>%
      summarise(mes_anio_num = max(mes_anio_num))
    
  }
  
  data <- data %>%
    group_by(!!!rlang::syms(agrupador), mes_anio_num) %>%
    summarise(Frecuencia = n()) %>%
    arrange(mes_anio_num) %>%
    collect() %>%
    pivot_wider(names_from = mes_anio_num, values_from = Frecuencia) %>%
    replace(is.na(.), 0)
  
  if (intervalo == "mes") {
    data <- data %>%
      rename_with(mes_spanish_juntos, .cols = -c(1:length(agrupador)))
  } else if (intervalo == "semana") {
    data <- data %>%
      rename_with(function(x) {
        paste(substr(x, 1, 4), substr(x, 5, 6), sep = " - ")
      }, .cols = -c(1:length(agrupador)))
  }
  
  return(data)
}


frecuencias_jerarquia <- function(data, columnas, columna_suma, columna_fecha,
                                columna_sep, nivel_1, nivel_2, nivel_3, 
                                nivel_4, intervalo = "mes", return_list = FALSE) {
  
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
        distinct(!!as.name(columna_suma), !!!rlang::syms(columna_sep))
    } else {
      data_episodios <- data %>%
        distinct(!!as.name(columna_suma))
    }
    
    data_episodios <- data_episodios %>%
      right_join(episodios)
    
    data_temp <- frecuencias(
      data = data_episodios,
      agrupador = c(columnas, columna_sep),
      columna_suma = columna_suma,
      prestaciones = FALSE,
      columna_fecha = columna_fecha,
      intervalo = intervalo
    )
    episodios_nivel_1 <- data_temp
    episodios <- episodios %>%
      select(!!as.name(columna_suma))
    data <- data %>%
      anti_join(episodios)
    data_temp <- NULL
    print("Nivel 1: Completo.")
  }
  
  episodios_nivel_2_data <- NULL
  if (!is.null(nivel_2)) {
    data_temp <- frecuencias(
      data = data,
      agrupador = c(columnas, columna_sep),
      columna_fecha = columna_fecha,
      columna_suma = "nro_factura",
      prestaciones = FALSE,
      intervalo = intervalo
    )
    episodios_nivel_2 <- data_temp %>%
      filter(!!as.name(columnas) %in% nivel_2)
    data_temp <- NULL
    print("Nivel 2: Completo.")
    
  }
  
  episodios_nivel_3_data <- NULL
  if (!is.null(nivel_3)) {
    data_temp <- frecuencias(
      data = data,
      agrupador = c(columnas, columna_sep),
      columna_fecha = columna_fecha,
      columna_suma = "nro_identificacion",
      prestaciones = FALSE,
      intervalo = intervalo
    )
    episodios_nivel_3 <- data_temp %>%
      filter(!!as.name(columnas) %in% nivel_3)
    data_temp <- NULL
    print("Nivel 3: Completo.")
    
  }
  
  episodios_nivel_4_data <- NULL
  if (!is.null(nivel_4)) {
    print("Nivel 4: Generando descriptiva.")
    data_temp <- frecuencias(
      data = data,
      agrupador = c(columnas, columna_sep),
      columna_fecha = columna_fecha,
      columna_suma = "",
      prestaciones = TRUE,
      intervalo = intervalo
    )
    print("Nivel 4: Descriptiva generada.")
    episodios_nivel_4 <- data_temp %>%
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
        )
      )
    }
  )
  
}
