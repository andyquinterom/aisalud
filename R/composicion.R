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
  
  datos_explorar <- data %>%
    select(!!as.name(columna_suma), !!as.name(columna_explorar)) %>%
    distinct() %>%
    right_join(episodios) %>%
    group_by(!!as.name(columna_episodios), !!as.name(columna_explorar)) %>%
    summarise(count = n(), n_episodios = max(n_episodios)) %>%
    mutate(participacion = round(100*count / n_episodios))
  
  return(datos_explorar)
    
  
}
