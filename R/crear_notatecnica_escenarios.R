crear_notatecnica_escenarios <- function(
  data = NULL, columnas, poblacion = 1, meses = 1, escenario = 1) {
  if (escenario == 1) {
    nota_tecnica_tabla <- data[
      , list(
        "CM" = round0(Media),
        "Frecuencia a mes" = round0(Frecuencia / meses),
        "Valor a mes" = round0(Media * Frecuencia / meses),
        "Frecuencia per capita" = Frecuencia / poblacion,
        "Coe" = get("Coef.var")
      ),
      by = c(columnas)
    ]

    return(nota_tecnica_tabla)
  }

  if (escenario == 2) {
    nota_tecnica_tabla <- data[
      , list(
        "CM" = round0(P75),
        "Frecuencia a mes" = round0(Frecuencia / meses),
        "Valor a mes" = round0(P75 * Frecuencia / meses),
        "Frecuencia per capita" = Frecuencia / poblacion,
        "Coe" = get("Coef.var")
      ),
      by = c(columnas)
    ]

    return(nota_tecnica_tabla)
  }

}
