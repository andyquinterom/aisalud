frecuencias <- function(
  data, agrupador, columna_valor, prestaciones, columna_fecha, columna_suma, 
  intervalo = "mes") {
  
  data <- data %>%
    mutate(valor_calculos = as.numeric(!!as.name(columna_valor)))
  
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
      summarise(valor_calculos = sum(valor_calculos, na.rm = TRUE),
                mes_anio_num = max(mes_anio_num))
    
  }
  
  data <- data %>%
    group_by(!!!rlang::syms(agrupador), mes_anio_num) %>%
    summarise(Frecuencia = n(), Suma = sum(valor_calculos, na.rm = TRUE)) %>%
    arrange(mes_anio_num) %>%
    collect() %>%
    # mutate(mes_anio = mes_spanish_juntos(mes_anio_num)) %>%
    select(-c(Suma)) %>%
    pivot_wider(names_from = mes_anio_num, values_from = Frecuencia)
  
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
