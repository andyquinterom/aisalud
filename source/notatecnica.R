round0 <- function(x) {
  return(round(x, 0))
}

crear_notatecnica_escenarios <- function(data = NULL, columnas, poblacion = 1, meses = 1,
                              escenario = 1) {
  if (escenario == 1) {
    nota_tecnica_tabla <- data[
      , list(
        "CME" = round0(Media),
        "Frecuencia a mes" = round0(Frecuencia/meses),
        "Valor a mes" = round0(Media*Frecuencia/meses),
        "Frecuencia per capita" = Frecuencia/poblacion,
        "Coe" = get('Coef.var')
      ),
      by = c(columnas)
    ]
    
    return(nota_tecnica_tabla)
  }
  
  if (escenario == 2) {
    nota_tecnica_tabla <- data[
      , list(
        "CME" = round0(P75),
        "Frecuencia a mes" = round0(Frecuencia/meses),
        "Valor a mes" = round0(P75*Frecuencia/meses),
        "Frecuencia per capita" = Frecuencia/poblacion,
        "Coe" = get('Coef.var')
      ),
      by = c(columnas)
    ]
    
    return(nota_tecnica_tabla)
  }
  
  if (escenario == 3) {
    nota_tecnica_tabla <- data[
      , list(
        "CME" = round0(get('Media truncada 10%')),
        "Frecuencia a mes" = round0(Frecuencia/meses),
        "Valor a mes" = round0(get('Media truncada 10%')*Frecuencia/meses),
        "Frecuencia per capita" = Frecuencia/poblacion,
        "Coe" = get('Coef.var')
      ),
      by = c(columnas)
    ]
    
    return(nota_tecnica_tabla)
  }
  
  if (escenario == 4) {
    nota_tecnica_tabla <- data[
      , list(
        "CME" = round0(get('Media truncada 5%')),
        "Frecuencia a mes" = round0(Frecuencia/meses),
        "Valor a mes" = round0(get('Media truncada 5%')*Frecuencia/meses),
        "Frecuencia per capita" = Frecuencia/poblacion,
        "Coe" = get('Coef.var')
      ),
      by = c(columnas)
    ]
    
    return(nota_tecnica_tabla)
  }
}

crear_notatecnica <- function(x = NULL, columnas, poblacion = 1, meses = 1) {
  x <- as.data.table(x)
  frecP25 <- quantile(x$Frecuencia, probs = 0.25)/meses
  notatecnica <- x[, list(
    "Primer escenario P75" = round0(P75*Frecuencia),
    "Segundo escenario media" = round0(Media*Frecuencia),
    "Tercer escenario media truncada 10%" = round0(
      get('Media truncada 10%')*Frecuencia),
    "Cuarto escenario media truncada 5%" = round0(
      get('Media truncada 5%')*Frecuencia),
    "Escenario combinado mayor" = max(
      c(round0(P75*Frecuencia),
      round0(Media*Frecuencia),
      round0(get('Media truncada 10%')*Frecuencia),
      round0(get('Media truncada 5%')*Frecuencia))),
    "Escenario por variabilidad y frecuencia" = round0(
      ifelse(
        !is.na(Coef.var),
        no = 0, 
        yes = (
          ifelse(
            Coef.var > 1 && round0(Frecuencia/meses) < 1,
            0,
            ifelse(
              Coef.var <= 1 && round0(Frecuencia/meses) >= 1,
              Media*Frecuencia,
              ifelse(
                round0(Frecuencia/meses) == 0,
                0,
                max(c(round0(P75*Frecuencia), round0(Media*Frecuencia), 
                      round0(get('Media truncada 10%')*Frecuencia),
                      round0(get('Media truncada 5%')*Frecuencia))))))))),
    "Frecuencia a mes" = round0(Frecuencia/meses),
    "Primer escenario P75 a mes" = round0(P75*Frecuencia/meses),
    "Segundo escenario media a mes" = round0(Media*Frecuencia/meses),
    "Tercer escenario media truncada 10% a mes" = round0(
      get('Media truncada 10%')*Frecuencia/meses),
    "Cuarto escenario media truncada 5% a mes" = round0(
      get('Media truncada 5%')*Frecuencia/meses),
    "Escenario combinado mayor a mes" = max(
      c(round0(P75*Frecuencia/meses),
        round0(Media*Frecuencia/meses), 
        round0(get('Media truncada 10%')*Frecuencia/meses),
        round0(get('Media truncada 5%')*Frecuencia/meses))),
    "Escenario por variabilidad y frecuencia a mes" = round0(
      ifelse(
        !is.na(Coef.var),
        no = 0, 
        yes = (
          ifelse(
            Coef.var > 1 && round0(Frecuencia/meses) < 1,
            0,
            ifelse(
              Coef.var <= 1 && round0(Frecuencia/meses) >= 1,
              Media*Frecuencia,
              ifelse(
                round0(Frecuencia/meses) == 0,
                0,
                max(c(round0(P75*Frecuencia),
                      round0(Media*Frecuencia), 
                      round0(get('Media truncada 10%')*Frecuencia),
                      round0(get('Media truncada 5%')*Frecuencia))))))))/meses),
    "Valor episodio" = round0(
      ifelse(
        P75*Frecuencia > Media*Frecuencia, 
        P75*Frecuencia,
        Media*Frecuencia)/Frecuencia),
    "Frecuencia per capita" = Frecuencia/poblacion,
    "CME" = round0(Media)),
    by = c(columnas)]
  
  return(as.data.table(notatecnica))

}