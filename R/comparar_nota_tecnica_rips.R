cbind.fill <- function(nm, agrupador){
  if (length(nm) == 1) {
    return(nm)
  }
  if (length(nm) > 1) {
    merged <- merge.data.table(nm[[1]], nm[[2]], by = agrupador, all = TRUE)
    nm <- append(list(merged), nm[-c(1:2)])
    cbind.fill(nm, agrupador = agrupador)
  }
}

mes_spanish <- function(x) {
  meses <- c(
    "Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "Septiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
  )
  return(
    meses[x]
  )
  
}

mes_spanish_juntos <- function(x) {
  meses <- c(
    "Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "Septiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
  )
  anio <- substr(x, 1, 4)
  mes <- meses[as.numeric(substr(x, 5, 6))]
  return(
    paste(anio, mes, sep = " - ")
  )
  
}

comparar_nt_frecuencias <- function(frecuencias, nota_tecnica, agrupador,
                                    indicador = "diff") {
  
  frec_y_nt <- frecuencias %>%
    rename(agrupador = !!as.name(agrupador)) %>%
    right_join(nota_tecnica %>%
                 select(agrupador, frec_mes, cm)) %>%
    group_by(agrupador, frec_mes, cm) %>%
    mutate(valor_mes = frec_mes * cm) %>%
    group_by(agrupador, frec_mes, cm, valor_mes)
  
  frec_y_nt %>%
    {if (indicador == "diff") {
      mutate(., across(.fns = ~ .x - frec_mes)) %>%
      mutate(total = rowSums(across(), na.rm = TRUE))
    } else if (indicador == "perc") {
      mutate(., across(.fns = ~ .x / na_if(frec_mes, 0))) %>%
      mutate(total = rowMeans(across(), na.rm = TRUE))
    } else if (indicador == "diff_cm") {
      mutate(., across(.fns = ~ (.x - frec_mes) * cm)) %>%
      mutate(total = rowSums(across(), na.rm = TRUE))
    }} %>%
    relocate(agrupador, frec_mes, cm, valor_mes) %>%
    return()
  
}

descriptiva_basica <- function(
  data, agrupador, columna_valor, prestaciones, columna_fecha, columna_suma) {
  
  data <- data %>%
    mutate(valor_calculos = as.numeric(!!as.name(columna_valor)))
  
  if (!prestaciones) {
    
    data <- data %>%
      mutate(mes_num = month(!!as.name(columna_fecha)),
             anio_num = year(!!as.name(columna_fecha))) %>%
      group_by(!!as.name(columna_suma), !!as.name(agrupador)) %>%
      summarise(valor_calculos = sum(valor_calculos, na.rm = TRUE),
                mes_anio_num = max(anio_num*100 + mes_num))
    
  } else {
    data <- data %>%
      mutate(mes_num = month(!!as.name(columna_fecha)),
             anio_num = year(!!as.name(columna_fecha))) %>%
      mutate(mes_anio_num = anio_num*100 + mes_num)
      
  }
  data <- data %>%
    group_by(!!as.name(agrupador), mes_anio_num) %>%
    summarise(Frecuencia = n(), Suma = sum(valor_calculos, na.rm = TRUE)) %>%
    arrange(mes_anio_num) %>%
    collect() %>%
    mutate(mes_anio = mes_spanish_juntos(mes_anio_num)) %>%
    as.data.table()
  
  return(data)
  data <- NULL
}

descriptiva_basica_trans <- function(data, agrupador, frec = TRUE, suma = TRUE) {
  data <- copy(data)
  meses <- as.list(unique(data$mes_anio))
  agrupCompletos <- data.table(agrupador = unique(data[[agrupador]]))
  setnames(agrupCompletos, agrupador)
  data_sep <- lapply(
    meses,
    data = data,
    agrupador = agrupador,
    frec = frec,
    suma = suma,
    FUN = function(x, data, agrupador, frec, suma) {
      data <- data[mes_anio == x,
                  c(agrupador,
                    "mes_anio", c("Frecuencia", "Suma")[c(frec, suma)]), 
                  with = FALSE]
      setnames(data,
               c(agrupador,
                 "mes_anio",
                 paste(unique(data[["mes_anio"]]),
                       c("Frecuencia", "Suma")[c(frec, suma)]))) 

           return(data[,-c("mes_anio")])
      
  })
  
  return(cbind.fill(data_sep, agrupador = agrupador)[[1]])
  
}

multiplicar_cme <- function(frecs, nota_tecnica) {
  frecs <- copy(frecs)
  nota_tecnica <- copy(nota_tecnica)
  setnames(frecs, 1, "agrupador")
  agrupadores_compartidos <- intersect(nota_tecnica[["agrupador"]], 
                                frecs[["agrupador"]])
  nota_tecnica <- nota_tecnica[agrupador %in% agrupadores_compartidos]
  frecs <- frecs[agrupador %in% agrupadores_compartidos]
  
  nota_tecnica <- nota_tecnica[order(agrupador)]
  frecs <- frecs[order(agrupador)]
  
  return(
    as.matrix(apply(frecs[, -c("agrupador")], 2, as.numeric)) * 
      numerize(nota_tecnica[["cm"]])
    )
  
}

diferencia_valor_rips <- function(sumas, nota_tecnica, porcentaje = FALSE) {
  sumas <- copy(sumas)
  nota_tecnica <- copy(nota_tecnica)
  setnames(sumas, 1, "agrupador")
  agrupadores_compartidos <- intersect(nota_tecnica[["agrupador"]],
                                sumas[["agrupador"]])
  nota_tecnica <- nota_tecnica[
    agrupador %in% agrupadores_compartidos][order(agrupador)]
  sumas <- sumas[agrupador %in% agrupadores_compartidos][order(agrupador)]
  
  nota_tecnica[, "cm" := numerize(cm)]
  nota_tecnica[, "valor_mes" := numerize(valor_mes)]
  
  numero_meses <- 2:ncol(sumas)
  
  sumas[, (numero_meses) := lapply(.SD, numerize), .SDcols = numero_meses]
  
  if (porcentaje) {
    return(
      as.data.table(
        append(
          list(agrupador = sumas$agrupador),
          as.data.frame(
            sumas[, c(numero_meses) , with = FALSE] / 
              nota_tecnica[["valor_mes"]])))
    )
  } else {
    return(
      as.data.table(
        append(
          list(agrupador = sumas$agrupador),
          as.data.frame(
            sumas[, c(numero_meses), with = FALSE] -
              nota_tecnica[["valor_mes"]])))
    )
  }
}

diferencia_valor_cme <- function(frecs, nota_tecnica, porcentaje = FALSE) {
  frecs <- copy(frecs)
  nota_tecnica <- copy(nota_tecnica)
  setnames(frecs, 1, "agrupador")
  agrupadores_compartidos <- intersect(nota_tecnica[["agrupador"]],
                                frecs[["agrupador"]])
  nota_tecnica <- nota_tecnica[
    agrupador %in% agrupadores_compartidos][order(agrupador)]
  frecs <- frecs[agrupador %in% agrupadores_compartidos][order(agrupador)]
  
  nota_tecnica[["cm"]] <- numerize(nota_tecnica[["cm"]])
  nota_tecnica[["valor_mes"]] <- numerize(nota_tecnica[["valor_mes"]])
  
  numero_meses <- 2:ncol(frecs)
  frecs[, (numero_meses) := lapply(.SD, numerize), .SDcols = numero_meses]
  
  if (porcentaje) {
    return(
      as.data.table(
        append(list(agrupador = frecs$agrupador),
               as.data.frame((
                 frecs[, c(numero_meses) , with = FALSE] * 
                   numerize(nota_tecnica[["cm"]])/
                   nota_tecnica[["valor_mes"]]))))
    )
  } else {
     return(
      as.data.table(
        append(list(agrupador = frecs$agrupador), 
               as.data.frame((
                 frecs[, c(numero_meses) , with = FALSE] * 
                   numerize(nota_tecnica[["cm"]]) - 
                   nota_tecnica[["valor_mes"]]))))
    )
  }
}

diferencias_totales <- function(frecs, sumas, nota_tecnica) {
  frecs <- copy(frecs)
  sumas <- copy(sumas)
  nota_tecnica <- copy(nota_tecnica)
  
  setnames(frecs, 1, "agrupador")
  setnames(sumas, 1, "agrupador")
  agrupadores_compartidos <- intersect(nota_tecnica[["agrupador"]],
                                frecs[["agrupador"]])
  valor_ejecutar_mes <- sum(nota_tecnica[["valor_mes"]], na.rm = TRUE)
  nota_tecnica <- nota_tecnica[
    agrupador %in% agrupadores_compartidos][order(agrupador)]
  frecs <- frecs[agrupador %in% agrupadores_compartidos][order(agrupador)]
  sumas <- sumas[agrupador %in% agrupadores_compartidos][order(agrupador)]
  
  nota_tecnica[, "cm" := numerize(cm)]
  nota_tecnica[, "valor_mes" := numerize(valor_mes)]
  
  total_mes_rips <- data.table(
    "Mes" = c(colnames(sumas[, -c(1)])),
    "Total" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE)) -
                       valor_ejecutar_mes),
    "%" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE)) /
              valor_ejecutar_mes)
  )
  
  total_agrupador_rips <- data.table(
    "Agrupador" = c(sumas[["agrupador"]]),
    "Total" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE)) -
                       (nota_tecnica[["valor_mes"]] * ncol(sumas[, -c(1)]))),
    "%" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE)) /
              (nota_tecnica[["valor_mes"]] * ncol(sumas[, -c(1)])))
  )
  
  numero_meses <- 2:ncol(frecs)
  frecs[, (numero_meses) := lapply(.SD, numerize), .SDcols = numero_meses]
  
  sumas_cme <- frecs[, c(numero_meses) , with = FALSE] * 
    numerize(nota_tecnica[["cm"]])
  
  total_mes_cme <- data.table(
    "Mes" = c(colnames(sumas[, -c(1)])),
    "Total" = c(as.vector(apply(sumas_cme, 2, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas_cme, 2, sum, na.rm = TRUE)) - 
                       valor_ejecutar_mes),
    "%" = c(as.vector(apply(sumas_cme, 2, sum, na.rm = TRUE)) /
              valor_ejecutar_mes)
  )
  
  total_agrupador_cme <- data.table(
    "Mes" = c(sumas[["agrupador"]]),
    "Total" = c(as.vector(apply(sumas_cme, 1, sum, na.rm = TRUE))),
    "Diferencia" = c(as.vector(apply(sumas_cme, 1, sum, na.rm = TRUE)) -
                       (nota_tecnica[["valor_mes"]] * ncol(sumas[, -c(1)]))),
    "%" = c(as.vector(apply(sumas_cme, 1, sum, na.rm = TRUE)) /
              (nota_tecnica[["valor_mes"]] * ncol(sumas[, -c(1)])))
  )
  
  valor_ejecutar <- valor_ejecutar_mes * ncol(sumas[, -c(1)])
  valor_ejecutado_rips <- sum(sumas[, -c(1)], na.rm = TRUE)
  valor_ejecutado_cme <- sum(sumas_cme, na.rm = TRUE)
  
  totales <- data.table(
    "Detalle" = c("Valor a ejecutar", "Ejecutado RIPS", "Ejecutado CM"),
    "Valor" = c(valor_ejecutar, valor_ejecutado_rips, valor_ejecutado_cme),
    "Diferencias" = c(NA,
                      valor_ejecutado_rips - valor_ejecutar,
                      valor_ejecutado_cme  - valor_ejecutar),
    "%" = c(NA,
                      valor_ejecutado_rips/valor_ejecutar,
                      valor_ejecutado_cme/valor_ejecutar)
  )
  
  
  
  return(
    list(
      total_mes_rips = total_mes_rips,
      total_mes_cme  = total_mes_cme,
      total_agrupador_rips = total_agrupador_rips,
      total_agrupador_cme  = total_agrupador_cme,
      totales = totales
    )
  )
  
}


descriptiva_basica_jerarquia <- function(
  data, columnas, columna_valor, columna_suma, nivel_1, nivel_2, 
  nivel_3, nivel_4, return_list = FALSE) {
  
  episodios_nivel_1 <- data.table()
  episodios_nivel_2 <- data.table()
  episodios_nivel_3 <- data.table()
  episodios_nivel_4 <- data.table()
  
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
    
    data_episodios <- data %>%
      group_by(!!as.name(columna_suma)) %>%
      summarise(valor_calculos = sum(!!as.name(columna_valor), na.rm = TRUE),
                fecha_episodio = max(fecha_prestacion)) %>%
      right_join(episodios)
    
    episodios_nivel_1 <- descriptiva_basica(
      data = data_episodios,
      agrupador = columnas,  
      columna_valor = "valor_calculos",
      columna_suma = columna_suma,
      columna_fecha = "fecha_episodio",
      prestaciones = FALSE
    )
    episodios <- episodios %>%
      select(!!as.name(columna_suma))
    data <- data %>%
      anti_join(episodios)
    data_temp <- NULL
  }
  
  
  if (!is.null(nivel_2)) {
    episodios_nivel_2 <- descriptiva_basica(
      data = data,
      agrupador = columnas,  
      columna_valor = columna_valor,
      columna_suma = "nro_factura",
      columna_fecha = "fecha_prestacion",
      prestaciones = FALSE
    ) %>%
      filter(!!as.name(columnas) %in% nivel_2)
    registros_procesados <- NULL
  }
  
  if (!is.null(nivel_3)) {
    episodios_nivel_3 <- descriptiva_basica(
      data = data,
      agrupador = columnas,  
      columna_valor = columna_valor,
      columna_suma = "nro_identificacion",
      columna_fecha = "fecha_prestacion",
      prestaciones = FALSE
    ) %>%
      filter(!!as.name(columnas) %in% nivel_3)
  }
  
  if (!is.null(nivel_4)) {
    episodios_nivel_4 <- descriptiva_basica(
      data = data,
      agrupador = columnas,  
      columna_valor = columna_valor,
      columna_suma = "",
      columna_fecha = "fecha_prestacion",
      prestaciones = TRUE
    ) %>%
      filter(!!as.name(columnas) %in% nivel_4)
  }
  
  data_final <- data.table::rbindlist(
    fill = TRUE,
    list(
      "episodio" = episodios_nivel_1,
      "factura"   = episodios_nivel_2,
      "paciente"  = episodios_nivel_3,
      "prestacion"= episodios_nivel_4
    )
  )
  print(data_final)
  
  return(
      data_final
  )
  
}