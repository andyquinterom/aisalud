datos_composicion <- function(data, columna_episodios, columna_valor,
                              columna_suma, columna_explorar, prioridad) {
  
  index_episodios <- data.frame(
    index = 1:length(prioridad),
    agrupador = prioridad
  )
  colnames(index_episodios) <- c("index", columna_episodios)
  
  episodios <- data %>%
    select(!!as.name(columna_suma), !!as.name(columna_episodios)) %>%
    filter(!!as.name(columna_episodios) %in% prioridad) %>%
    distinct() %>%
    right_join(index_episodios, copy = TRUE) %>%
    arrange(index) %>%
    group_by(!!as.name(columna_suma)) %>%
    mutate(!!columna_episodios := first(!!as.name(columna_episodios))) %>%
    ungroup() %>%
    distinct(!!as.name(columna_suma), !!as.name(columna_episodios)) %>%
    group_by(!!as.name(columna_episodios)) %>%
    mutate(n_episodios = n())
  
  sumas_episodios <- data %>%
    group_by(!!as.name(columna_suma)) %>%
    summarise(valor_calculos = sum(!!as.name(columna_valor), na.rm = TRUE)) %>%
    right_join(episodios) %>%
    ungroup() %>%
    group_by(!!as.name(columna_episodios)) %>%
    summarise(valor_calculos = sum(valor_calculos))
  
  datos_explorar <- data %>%
    group_by(!!as.name(columna_suma), !!as.name(columna_explorar)) %>%
    summarise(valor_explorar = sum(!!as.name(columna_valor), na.rm = TRUE)) %>%
    right_join(episodios) %>%
    group_by(!!as.name(columna_episodios), !!as.name(columna_explorar)) %>%
    summarise(count = n(), n_episodios = max(n_episodios),
              valor_explorar = sum(valor_explorar)) %>%
    right_join(sumas_episodios) %>%
    mutate(participacion = round(100*count / n_episodios),
           participacion_valor = round(100*valor_explorar / valor_calculos)) %>%
    select(!!as.name(columna_episodios), !!as.name(columna_explorar),
           count, n_episodios, participacion,
           valor_explorar, valor_calculos, participacion_valor)
  
  return(datos_explorar)
    
  
}
