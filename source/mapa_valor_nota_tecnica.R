mapaValoresNT <- function(indice, ...) {
  
  indice_sum <- NULL
  tabla_dep <- NULL
  p <- NULL
  
  indice_sum <- indice[, list(
    "Valor" = sum(valor_mes, na.rm = TRUE),
    "Contratos" = length(cod_nt)),
    by = c("cod_departamento")]
  indice_sum$id = numerize(indice_sum$cod_departamento)
  indice_sum$Contratos = numerize(indice_sum$Contratos)
  indice_sum <- indice_sum[order(cod_departamento)]
  
  tabla_dep <- data.table(
    id = numerize(colmaps::departamentos@data[["id"]]),
    depto = colmaps::departamentos@data[["depto"]],
    ID = str_pad(colmaps::departamentos@data[["id"]], pad = "0", width = 2)
  )
  
  tabla_dep <- merge(tabla_dep, indice_sum, all.x = TRUE, sort = FALSE)
  tabla_dep[is.na(Valor), Valor:=0]
  tabla_dep[is.na(Contratos), Contratos:=0]
  tabla_dep[, c("id", "cod_departamento") := NULL]

  tabla_dep$ValorFormato <- paste0(
    "Valor: ",
    "$",
    format(tabla_dep$Valor,
           scientific = FALSE,
           big.mark = ".",
           decimal.mark = ","))
  tabla_dep$Departamento <- tabla_dep$depto

  p <- ggplotly(
    p = colmaps2::colmap(
      departamentos,
      data = tabla_dep,
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