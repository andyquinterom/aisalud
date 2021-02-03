leer_perfil <- function(json) {
  perfil <- parse_json(json, simplifyVector = TRUE)
  
  perfil_transform <- lapply(perfil, as.data.frame) %>%
    rbindlist(fill = TRUE, idcol = "perfil")
    
  
  return(perfil_transform)
  
}
