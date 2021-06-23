frec_x_cm <- function(timeseries, nota_tecnica, agrupador) {

  by_vector <- "agrupador"
  names(by_vector) <- agrupador
  nota_tecnica <- nota_tecnica %>%
    select(agrupador, cm)

  frec <- timeseries %>%
    inner_join(nota_tecnica, by = by_vector) %>%
    mutate(Suma = Frecuencia * cm) %>%
    ungroup()

  return(frec)

}
