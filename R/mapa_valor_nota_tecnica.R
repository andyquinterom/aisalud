mapa_valores <- function(indice, ...) {
  
  indice <- copy(indice)
  setDT(indice)
  
  departamentos <- data.table::data.table(
    "id" = as.character(c(5,8,11,13,15,17,18,19,20,23,25,27,41,44,47,50,
                          52,54,63,66,68,70,73,76,81,85,86,88,91,94,95,97,99)),
    "nombre" = c("ANTIOQUIA","ATLÁNTICO","BOGOTÁ, D.C.","BOLÍVAR","BOYACÁ",
                 "CALDAS","CAQUETÁ","CAUCA","CESAR","CÓRDOBA","CUNDINAMARCA",
                 "CHOCÓ","HUILA","LA GUAJIRA","MAGDALENA","META","NARIÑO",
                 "NORTE DE SANTANDER","QUINDÍO","RISARALDA","SANTANDER","SUCRE",
                 "TOLIMA","VALLE DEL CAUCA","ARAUCA","CASANARE","PUTUMAYO",
                 "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA",
                 "AMAZONAS","GUAINÍA","GUAVIARE","VAUPÉS","VICHADA"),
    "lat" = c(6.702032125,10.67700953,4.316107698,8.079796863,5.891672889,
              5.280139978,0.798556195,2.396833887,9.53665993,8.358549754,
              4.771120716,5.397581542,2.570143029,11.47687008,10.24738355,
              3.345562732,1.571094987,8.09513751,4.455241567,5.240757239,
              6.693633184,9.064941448,4.03477252,3.569858693,6.569577215,
              5.404064237,0.3673031,12.54311512,-1.54622768,2.727842865,
              1.924531973,0.64624561,4.713557125),
    "long" = c(-75.50455704,-74.96521949,-74.1810727,-74.23514814,-72.62788054,
               -75.27498304,-73.95946756,-76.82423283,-73.51783154,-75.79200872,
               -74.43111092,-76.942811,-75.58434836,-72.42951072,-74.26175733,
               -72.95645988,-77.87020496,-72.88188297,-75.68962853,-76.00244469,
               -73.48600894,-75.10981755,-75.2558271,-76.62850427,-70.96732394,
               -71.60188073,-75.51406183,-81.71762382,-71.50212858,-68.81661272,
               -72.12859569,-70.56140566,-69.41400011)
  )
  
  datos <- merge.data.table(indice, departamentos, by.x = "cod_departamento",
                            by.y = "id")
  
  datos <- datos[, list("valor_mes" = sum(valor_mes, na.rm = TRUE),
                        "prestadores" = paste(nom_prestador, collapse = ", "),
                        "ciudades" = paste(unique(ciudades), collapse = ", ")),
                 by = c("cod_departamento", "lat", "long", "nombre")]
  
  datos[, "valor_total" := sum(valor_mes, na.rm = TRUE)]
  
  p <- leaflet(datos) %>%
    addTiles() %>%
    addCircleMarkers(
      clusterOptions = markerClusterOptions(),
      color = "green",
      fillOpacity = ~(valor_mes/valor_total),
      radius = 16,
      popup = ~paste(
        paste0("<b><a>", nombre, "</a></b>"),
        paste0("Prestadores: ", prestadores),
        paste0("Ciudades: ", ciudades),
        paste0("Valor total: <b><a>", formatAsCurrency(valor_mes), "</a></b>"),
        sep = "<br/>"
      )
    )
  
  return(p)
  
}
