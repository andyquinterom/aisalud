mes_spanish_juntos <- function(x) {
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
  anio <- substr(x, 1, 4)
  mes <- meses[as.numeric(substr(x, 5, 6))]
  return(
    paste(anio, mes, sep = " - ")
  )
}
