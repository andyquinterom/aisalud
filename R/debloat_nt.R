debloat_nt <- function(nota_tecnica) {

  nt2 <- list()

  agrupadores <- nota_tecnica$agrupadores

  agrupadores <- purrr::map(agrupadores, function(agrup) {
    return(list(n = agrup$n, cm = agrup$cm))
  })

  if (!is.null(nota_tecnica$perfil)) nt2$perfil <- nota_tecnica$perfil
  nt2$poblacion <- nota_tecnica$poblacion
  if (is.null(nota_tecnica$poblacion)) nt2$poblacion <- 1
  nt2$departamento <- "Bogotá D.C"
  nt2$cod_departamento <- 11
  nt2$vigente <- FALSE
  nt2$ciudades <- "Bogotá D.C"
  nt2$agrupadores <- agrupadores

  return(nt2)

}
