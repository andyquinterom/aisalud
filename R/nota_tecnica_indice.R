parse_nt <- function(x) {
  
  purrr::map(x, function(y) {
    poblacion <- y[["poblacion"]]
    agrupadores_names <- names(y[["agrupadores"]])

    purrr::map2(y[["agrupadores"]], agrupadores_names, function(i, w) {
      if (is.null(names(i))) i <- list("n" = i[1], "cm" = i[2])
      temp <- data.frame("agrupador" = w, "frec_mes" = as.double(i[["n"]]),
                         "cm" = as.double(i[["cm"]]), 
                         "frecuencia_pc" = as.double(i[["n"]]/poblacion),
                         "valor_mes" = as.double(i[["n"]]*i[["cm"]]))
      return(temp)
    }) %>% rbindlist()
    
  }) %>% 
    rbindlist(idcol = "nt")
  
}

parse_nt_indice <- function(x, tabla_agrupadores) {
  
  nombres_nt <- names(x)
  
  indice_datos <- purrr::map2(x, nombres_nt,  function(y, cod_nt) {
    prestador <- y[["prestador"]]
    asegurador <- y[["asegurador"]]
    
    tibble(
      cod_nt = cod_nt,
      nom_prestador = NA,
      nom_asegurador = NA,
      poblacion = as.double(y[["poblacion"]]),
      departamento = y[["departamento"]],
      ciudades = y[["ciudades"]],
      cod_departamento = y[["cod_departamento"]],
      vigente = ifelse(is.null(y[["vigente"]]),
                               yes = FALSE, 
                               no = as.logical(y[["vigente"]]))
    ) %>%
      {if (!is.null(prestador)) {
        mutate(., nom_prestador = y[["prestador"]])
      } else {.}} %>% 
      {if (!is.null(asegurador)) {
        mutate(., nom_asegurador = y[["asegurador"]])
      } else {.}}
  }) %>% 
    rbindlist(fill = TRUE, use.names = TRUE) %>% 
    select(where(~!all(is.na(.x))))
  
  valores_mes <- tabla_agrupadores %>%
    group_by(nt) %>%
    summarise(valor_mes = sum(valor_mes, na.rm = TRUE))
  
  indice <- indice_datos %>%
    inner_join(valores_mes, by = c("cod_nt" = "nt")) %>%
    mutate(cod_departamento = as.character(cod_departamento)) %>%
    return()
  
}
