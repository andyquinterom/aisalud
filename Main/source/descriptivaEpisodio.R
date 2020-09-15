require(data.table)


descriptvaEpisodio = function(data, columnas, columna_valor, columna_suma, columna_sep) {
	data = as.data.frame(data)
	data = data[, unique(c('NRO_IDENTIFICACION', columnas, columna_valor, columna_suma, columna_sep))]
	colnames(data) = unique(c('NRO_IDENTIFICACION', columnas, "VALOR", columna_suma, columna_sep))
	data$VALOR = numerize(data$VALOR)
	data = data.table(data)
	data = data[, list("FREC_PACIENTES" = length(unique(NRO_IDENTIFICACION)),
										 "Suma" = sum(VALOR, na.rm = TRUE),
										 "VAR_PLACEHOLDER" = unique(get(columnas))
	), by = c(columna_suma, "NRO_IDENTIFICACION", columna_sep)]
	colnames(data)[which(colnames(data) == "VAR_PLACEHOLDER")] = columnas
	data = descriptiva(data, columnas = c(columnas, columna_sep), columna_valor = "Suma", prestaciones = FALSE)
	return(data)
	data = NULL
}