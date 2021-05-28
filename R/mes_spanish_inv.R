mes_spanish_inv <- function(x) {
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

  divididos <- str_split(x, " - ")

  divididos_numerico <- purrr::map(
    .x = divididos,
    .f = function(x) {
      as.numeric(x[1]) * 100 + which(x[2] == meses)
    })

  return(unlist(divididos_numerico))


}
