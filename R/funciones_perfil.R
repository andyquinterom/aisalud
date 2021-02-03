perfil_jerarquia <- function(perfiles, perfil_select, items, 
                             funcion_jerarquia, ns) {
  
  perfiles_seleccionado <- perfiles[[perfil_select]][["jerarquia"]]
  
  items_nivel_1 <- perfiles_seleccionado[["episodio"]]
  items_nivel_1 <- items_nivel_1[items_nivel_1 %in% items]
  
  items_nivel_2 <- perfiles_seleccionado[["factura"]]
  items_nivel_2 <- items_nivel_2[items_nivel_2 %in% items]
  
  items_nivel_3 <- perfiles_seleccionado[["paciente"]]
  items_nivel_3 <- items_nivel_3[items_nivel_3 %in% items]
  
  items_nivel_4 <- setdiff(
    x = items,
    y = c(items_nivel_1, items_nivel_2, items_nivel_3))
  
  if (identical(items_nivel_4, character(0))) {
    items_nivel_4 <- NULL
  }
  
  funcion_jerarquia(
    ns = ns,
    items_nivel_1 = items_nivel_1,
    items_nivel_2 = items_nivel_2,
    items_nivel_3 = items_nivel_3,
    items_nivel_4 = items_nivel_4
  )
  
}