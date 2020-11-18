mapaValoresNT <- function(INDICE, ...) {
  
  INDICE_SUM <- NULL
  tablaDep <- NULL
  p <- NULL
  
  INDICE_SUM <- INDICE[, list(
    "Valor" = sum(VALOR_MES, na.rm = TRUE),
    "Contratos" = length(COD_NT)),
    by = c("COD_DEPARTAMENTO")]
  INDICE_SUM$id = numerize(INDICE_SUM$COD_DEPARTAMENTO)
  INDICE_SUM$Contratos = numerize(INDICE_SUM$Contratos)
  INDICE_SUM <- INDICE_SUM[order(COD_DEPARTAMENTO)]
  
  tablaDep <- data.table(
    id = numerize(colmaps::departamentos@data[["id"]]),
    depto = colmaps::departamentos@data[["depto"]],
    ID = str_pad(colmaps::departamentos@data[["id"]], pad = "0", width = 2)
  )
  
  tablaDep <- merge(tablaDep, INDICE_SUM, all.x = TRUE, sort = FALSE)
  tablaDep[is.na(Valor), Valor:=0]
  tablaDep[is.na(Contratos), Contratos:=0]
  tablaDep[, c("id", "COD_DEPARTAMENTO") := NULL]

  tablaDep$ValorFormato <- paste0(
    "Valor: ",
    "$",
    format(tablaDep$Valor,
           scientific = FALSE,
           big.mark = ".",
           decimal.mark = ","))
  tablaDep$Departamento <- tablaDep$depto

  p <- ggplotly(
    p = colmaps2::colmap(
      departamentos,
      data = tablaDep,
      data_id = "ID",
      var = "Valor",
      legend = TRUE,
      text = "ValorFormato",
      nombre = "Departamento") +
      scale_fill_gradient(low = "grey",
                          high = "dark green",
                          name = "Valor acumulado\npor departamento\na mes", 
                          labels = dollar_format()),
    tooltip = c("text", "nombre"), ...) %>%
    config(locale = "es") 
  
  return(p)
  
}