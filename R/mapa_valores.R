mapa_valores <- function(indice, departamentos, ...) {
  
  # Se resume en indice por departamento
  indice_summarise <- indice %>%
    mutate(cod_departamento = as.numeric(cod_departamento)) %>%
    group_by(cod_departamento)
  
  prestadores <- NULL
  aseguradores <- NULL
  
  # Si hay prestadores se suman
  if ("nom_prestador" %in% colnames(indice_summarise)) {
    prestadores <- indice_summarise %>%
      summarise(
        prestadores = paste(unique(nom_prestador), collapse = ", "))
  }
  
  # Si hay aseguradores se suman
  if ("nom_asegurador" %in% colnames(indice_summarise)) {
    aseguradores <- indice_summarise %>% 
      summarise(
        aseguradores = paste(unique(nom_asegurador), collapse = ", "))
  }
  
  # Resumen completo
  valores <- indice_summarise %>% 
    summarise(valor_mes = sum(valor_mes, na.rm = TRUE),
              ciudades = paste(unique(ciudades), collapse = ", ")) %>% 
    {if (!is.null(prestadores)) {
      left_join(., prestadores)
    } else {.}} %>% 
    {if (!is.null(aseguradores)) {
      left_join(., aseguradores)
    } else {.}} %>% 
    ungroup()
  
  # Se juntan datos geograficos con datos de notas tecnicas
  departamentos@data <- departamentos@data %>%
    left_join(valores) %>%
    mutate(valor_mes = replace_na(valor_mes, 0))
  
  p <- departamentos %>%
    mapView(zcol = "valor_mes", legend = FALSE)
  
  return(p)
  
}