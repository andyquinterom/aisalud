require(data.table)

resumenComp = function(tabla, columna, colsum) {
			tabla = as.data.table(tabla)
			total = sum(tabla[, list(get(colsum))], na.rm = TRUE)
			totales = data.table("x" = "TOTAL", "SUMA" = formatAsCurrency(total), "PARTICIPACIÓN (%)" = "100%")
			colnames(totales) = c(columna, "SUMA", "PARTICIPACIÓN (%)")
			
			return(
				rbind(use.names=FALSE,
							 tabla[, list("SUMA" = formatAsCurrency(sum(get(colsum), na.rm = T)), "PARTICIPACIÓN (%)" = formatAsPerc(sum(get(colsum), na.rm = T)/total)), by = columna]
							,totales
				)
			)
			tabla = NULL
}