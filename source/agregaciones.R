require(data.table)

agregar = function(data, columna_valor, columnas, prestaciones) {

		data = as.data.frame(data)
		data = data[, c('NRO_IDENTIFICACION',columnas, columna_valor)]
		colnames(data) = c('NRO_IDENTIFICACION', columnas, "VALOR")
		data$VALOR = numerize(data$VALOR)
		data = data.table(data)
		if (isFALSE(prestaciones)) {
			data = data[, list("VALOR" = sum(VALOR)), by = c('NRO_IDENTIFICACION', columnas[columnas != 'NRO_IDENTIFICACION'])]
		}
		data = data[, list("Frecuencia" = length(VALOR),columna_valor = sum(VALOR, na.rm = TRUE), "Promedio" = mean(VALOR, na.rm = TRUE)), by = c(columnas)]
		colnames(data) = c(columnas, "Frecuencia", columna_valor, "Promedio")
		return(data)
		data = NULL
}

`%notin%` = Negate(`%in%`)