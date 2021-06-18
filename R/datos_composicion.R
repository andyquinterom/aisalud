datos_composicion <- function(data, columna_episodios, columna_valor,
                              columna_suma, columna_explorar, prioridad,
                              frec_cantidad = FALSE) {

  index_episodios <- data.frame(
    index = seq_len(length(prioridad)),
    agrupador = prioridad
  )
  colnames(index_episodios) <- c("index", columna_episodios)

  if (columna_explorar == columna_episodios) {
    columna_explorar_nuevo <- paste(columna_explorar, "(Explorar)")
    data <- data %>%
      mutate(!!columna_explorar_nuevo := !!rlang::sym(columna_explorar))
    columna_explorar <- columna_explorar_nuevo
  }

  episodios <- data %>%
    select(!!!rlang::syms(unique(c(columna_suma, columna_episodios)))) %>%
    filter(!!as.name(columna_episodios) %in% prioridad) %>%
    group_by(!!as.name(columna_suma)) %>%
    distinct() %>%
    right_join(index_episodios, copy = TRUE) %>%
    window_order(index) %>%
    mutate(!!columna_episodios := first(!!as.name(columna_episodios))) %>%
    ungroup() %>%
    distinct(!!!rlang::syms(unique(c(columna_suma, columna_episodios)))) %>%
    group_by(!!as.name(columna_episodios)) %>%
    mutate(n_episodios = n())

  sumas_episodios <- data %>%
    group_by(!!as.name(columna_suma)) %>%
    summarise(valor_calculos = sum(!!as.name(columna_valor), na.rm = TRUE)) %>%
    right_join(episodios) %>%
    ungroup() %>%
    group_by(!!as.name(columna_episodios)) %>%
    summarise(valor_episodios = sum(valor_calculos))

  datos_explorar <- data %>%
    group_by(!!!rlang::syms(unique(c(columna_suma, columna_explorar)))) %>%
    summarise(
      valor_explorar = sum(!!as.name(columna_valor), na.rm = TRUE),
      registros_explorar = ifelse(
        test = frec_cantidad,
        yes = sum(cantidad, na.rm = TRUE),
        no = n())) %>%
    right_join(episodios) %>%
    group_by(!!!rlang::syms(unique(c(columna_episodios, columna_explorar)))) %>%
    summarise(incluida_n_episodios = n(),
              n_registros = sum(registros_explorar),
              n_episodios = max(n_episodios),
              valor_explorar = sum(valor_explorar)) %>%
    right_join(sumas_episodios) %>%
    mutate(media_explorar_episodio = valor_explorar /
             na_if(incluida_n_episodios, 0),
           media_explorar_registro = valor_explorar /
             na_if(n_registros, 0),
           registros_por_episodios = n_registros /
             na_if(incluida_n_episodios, 0),
           media_episodio = valor_episodios /
             na_if(n_episodios, 0)) %>%
    mutate(participacion_en_episodios =
             round(100 * incluida_n_episodios / na_if(n_episodios, 0)),
           participacion_valor =
             round(100 * valor_explorar / na_if(valor_episodios, 0))) %>%
    select(!!as.name(columna_episodios),
           valor_episodios, media_episodio, n_episodios,
           !!as.name(columna_explorar),
           participacion_en_episodios, participacion_valor,
           n_registros, incluida_n_episodios, valor_explorar,
           registros_por_episodios,
           media_explorar_registro, media_explorar_episodio)

  return(datos_explorar)
}
