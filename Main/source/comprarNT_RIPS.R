require(data.table)
require(lubridate)
require(dplyr)

cbind.fill<-function(nm, agrupador){
if (length(nm) == 1) {
	return(nm)
}
if (length(nm) > 1) {
	merged = merge.data.table(nm[[1]], nm[[2]], by = agrupador, all = TRUE)
	nm = append(list(merged), nm[-c(1:2)])
	cbind.fill(nm, agrupador = agrupador)
}
}

mesSpanish = function(x) {
	meses = c(
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
	return(
		meses[x]
	)
	
}

descriptivaBasica = function(data, agrupador, columna_valor, prestaciones, columna_fecha) {
	data = as.data.frame(data)
	columnas = c(agrupador, columna_fecha)
	data = data[, c('NRO_IDENTIFICACION',columnas, columna_valor)]
	colnames(data) = c('NRO_IDENTIFICACION', columnas, "VALOR")
	data$VALOR = numerize(data$VALOR)
	data$MES_ANIO_NUM = lubridate::year(data[[columna_fecha]])*100 + lubridate::month(data[[columna_fecha]])
	data$MES_ANIO = paste(lubridate::year(data[[columna_fecha]]), mesSpanish(lubridate::month(data[[columna_fecha]])), sep = " - ")
	columnas = c(agrupador, "MES_ANIO_NUM", "MES_ANIO")
	data = data.table(data)
	if (isFALSE(prestaciones)) {
		data = data[, list("VALOR" = sum(VALOR)), by = c('NRO_IDENTIFICACION', columnas[columnas != 'NRO_IDENTIFICACION'])]
	}
	data = data[, list("Frecuencia" = length(VALOR),
										 "Suma" = sum(VALOR, na.rm = TRUE)
	), by = c(columnas)]
	colnames(data) = c(columnas, "Frecuencia", "Suma")
	return(data[order(MES_ANIO_NUM)])
	data = NULL
}

descriptivaBasicaTrans = function(data, agrupador, frec = TRUE, suma = TRUE) {
	meses = as.list(unique(data$MES_ANIO))
	agrupCompletos = data.table(agrupador = unique(data[[agrupador]]))
		colnames(agrupCompletos) = agrupador

	dataSep = lapply(meses, data = data, agrupador = agrupador, frec = frec, suma = suma
				 , FUN = function(x, data, agrupador, frec, suma) {
				 	data = data[MES_ANIO == x, c(agrupador, "MES_ANIO", c("Frecuencia", "Suma")[c(frec, suma)]), with = FALSE]
				 	colnames(data) = c(agrupador, "MES_ANIO", paste(unique(data[["MES_ANIO"]]), c("Frecuencia", "Suma")[c(frec, suma)]))
				 	return(data[,-c("MES_ANIO")])
				 })
	# return(dataSep)
	return(cbind.fill(dataSep, agrupador = agrupador)[[1]])
	
}

multiplicarCME = function(frecs, NT) {
	colnames(frecs)[1] = "AGRUPADOR"
	agrupCompartidos = intersect(NT[["AGRUPADOR"]], frecs[["AGRUPADOR"]])
			NT = NT[AGRUPADOR %in% agrupCompartidos]
			frecs = frecs[AGRUPADOR %in% agrupCompartidos]
	
	NT = NT[order(AGRUPADOR)]
	frecs = frecs[order(AGRUPADOR)]
	
	return(as.matrix(apply(frecs[, -c("AGRUPADOR")], 2, as.numeric)) * numerize(NT[["CME"]]))
	
}

diferenciaValorRIPS = function(sumas, NT, porcentaje = FALSE) {
	colnames(sumas)[1] = "AGRUPADOR"
	agrupCompartidos = intersect(NT[["AGRUPADOR"]], sumas[["AGRUPADOR"]])
			NT = NT[AGRUPADOR %in% agrupCompartidos]
			sumas = sumas[AGRUPADOR %in% agrupCompartidos]
	
	NT = NT[order(AGRUPADOR)]
	sumas = sumas[order(AGRUPADOR)]
	
	NT[["CME"]] = numerize(NT[["CME"]])
	NT[["VALOR_MES"]] = numerize(NT[["VALOR_MES"]])
	
	if (porcentaje)
		return(
			as.data.table(append(list(AGRUPADOR = sumas$AGRUPADOR), as.data.frame(as.matrix(apply(sumas[, -c("AGRUPADOR")], 2, as.numeric))/NT[["VALOR_MES"]])))
		)
	else
		return(
			as.data.table(append(list(AGRUPADOR = sumas$AGRUPADOR), as.data.frame(as.matrix(apply(sumas[, -c("AGRUPADOR")], 2, as.numeric)) - NT[["VALOR_MES"]])))
		)
}

diferenciaValorCME = function(frecs, NT, porcentaje = FALSE) {
	colnames(frecs)[1] = "AGRUPADOR"
	agrupCompartidos = intersect(NT[["AGRUPADOR"]], frecs[["AGRUPADOR"]])
	NT = NT[AGRUPADOR %in% agrupCompartidos]
	frecs = frecs[AGRUPADOR %in% agrupCompartidos]
	
	NT = NT[order(AGRUPADOR)]
	frecs = frecs[order(AGRUPADOR)]
	
	NT[["CME"]] = numerize(NT[["CME"]])
	NT[["VALOR_MES"]] = numerize(NT[["VALOR_MES"]])
	
	if (porcentaje)
		return(
			as.data.table(append(list(AGRUPADOR = frecs$AGRUPADOR), as.data.frame((as.matrix(apply(frecs[, -c("AGRUPADOR")], 2, as.numeric)) * numerize(NT[["CME"]]))/NT[["VALOR_MES"]])))
		)
	else
		return(
			as.data.table(append(list(AGRUPADOR = frecs$AGRUPADOR), as.data.frame((as.matrix(apply(frecs[, -c("AGRUPADOR")], 2, as.numeric)) * numerize(NT[["CME"]])) - NT[["VALOR_MES"]])))
		)
}

diferenciasTotales = function(frecs, sumas, NT) {

	colnames(frecs)[1] = "AGRUPADOR"
	colnames(sumas)[1] = "AGRUPADOR"
	agrupCompartidos = intersect(NT[["AGRUPADOR"]], frecs[["AGRUPADOR"]])
		NT = NT[AGRUPADOR %in% agrupCompartidos]
		frecs = frecs[AGRUPADOR %in% agrupCompartidos]
		sumas = sumas[AGRUPADOR %in% agrupCompartidos]
	
		NT = NT[order(AGRUPADOR)]
		frecs = frecs[order(AGRUPADOR)]
		sumas = sumas[order(AGRUPADOR)]
	
	NT[["CME"]] = numerize(NT[["CME"]])
	NT[["VALOR_MES"]] = numerize(NT[["VALOR_MES"]])
	
	totalMesRIPS = data.table(
		"Mes" = c(colnames(sumas[, -c(1)])),
		"Total" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE))),
		"Diferencia" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE)) - sum(NT[["VALOR_MES"]], na.rm = TRUE)),
		"%" = c(as.vector(apply(sumas[, -c(1)], 2, sum, na.rm = TRUE))/sum(NT[["VALOR_MES"]], na.rm = TRUE))
	)
	
	totalAgrupadorRIPS = data.table(
		"Agrupador" = c(sumas[["AGRUPADOR"]]),
		"Total" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE))),
		"Diferencia" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE)) - (NT[["VALOR_MES"]] * ncol(sumas[, -c(1)]))),
		"%" = c(as.vector(apply(sumas[, -c(1)], 1, sum, na.rm = TRUE))/(NT[["VALOR_MES"]] * ncol(sumas[, -c(1)])))
	)
	
	sumasCME = as.matrix(apply(frecs[, -c("AGRUPADOR")], 2, as.numeric)) * numerize(NT[["CME"]])
	
	totalMesCME = data.table(
		"Mes" = c(colnames(sumas[, -c(1)])),
		"Total" = c(as.vector(apply(sumasCME, 2, sum, na.rm = TRUE))),
		"Diferencia" = c(as.vector(apply(sumasCME, 2, sum, na.rm = TRUE)) - sum(NT[["VALOR_MES"]], na.rm = TRUE)),
		"%" = c(as.vector(apply(sumasCME, 2, sum, na.rm = TRUE))/sum(NT[["VALOR_MES"]], na.rm = TRUE))
	)
	
	totalAgrupadorCME = data.table(
		"Mes" = c(sumas[["AGRUPADOR"]]),
		"Total" = c(as.vector(apply(sumasCME, 1, sum, na.rm = TRUE))),
		"Diferencia" = c(as.vector(apply(sumasCME, 1, sum, na.rm = TRUE)) - (NT[["VALOR_MES"]] * ncol(sumas[, -c(1)]))),
		"%" = c(as.vector(apply(sumasCME, 1, sum, na.rm = TRUE))/(NT[["VALOR_MES"]] * ncol(sumas[, -c(1)])))
	)
	
	valorEjecutar = sum(NT[["VALOR_MES"]]*ncol(sumas[, -c(1)]), na.rm = TRUE)
	valorEjecutadoRips = sum(sumas[, -c(1)], na.rm = TRUE)
	valorEjecutadoCME = sum(sumasCME, na.rm = TRUE)
	
	totales = data.table(
		"Detalle" = c("Valor a ejecutar", "Ejecutado RIPS", "Ejecutado CME"),
		"Valor" = c(valorEjecutar, valorEjecutadoRips, valorEjecutadoCME),
		"Diferencias" = c(NA,
											valorEjecutadoRips - valorEjecutar,
											valorEjecutadoCME  - valorEjecutar
											),
		"%"           = c(NA,
											valorEjecutadoRips/valorEjecutar,
											valorEjecutadoCME/valorEjecutar
											)
	)
	
	
	
	return(
		list(
			totalMesRIPS = totalMesRIPS,
			totalMesCME  = totalMesCME,
			totalAgrupadorRIPS = totalAgrupadorRIPS,
			totalAgrupadorCME  = totalAgrupadorCME,
			totales = totales
		)
	)
	
}


