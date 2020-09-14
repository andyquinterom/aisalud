library(data.table)
library(shiny)
outlierspercentil = function(data, columna, columna_valor, percentil, frecuencia = 1,session = NULL) {
	if (is.null(session)) {
		data = as.data.frame(data)
		data = data[, c('NRO_IDENTIFICACION', columna, columna_valor)]
		colnames(data) = c('NRO_IDENTIFICACION', columna, "VALOR")
		data$VALOR = numerize(data$VALOR)
			data = data.table(data, key= 'NRO_IDENTIFICACION')
			data = data[, list("VALOR" = sum(VALOR)), by = c('NRO_IDENTIFICACION', columna)]
			datapacientes = data
			data = data[, list("Condicion" = quantile(VALOR, probs = percentil, na.rm = TRUE), "Frec" = length(VALOR)), by = c(columna)]	
			data = data[Frec >= frecuencia]
		
		datafinal = data.table()	
		for (i in unique(data[[columna]])) {
				datatemp = data.table()
				datatemp = datapacientes[get(columna) == i]
				condicion = data[get(columna) == i, Condicion]
				datatemp = datatemp[VALOR >= condicion]
				datafinal = rbind(datafinal, datatemp)
		}
		
	}
	if (!is.null(session)) {
		lengthProgreso = length(data[[columna]])
		withProgress(min = 0, max = 3, message = "Calculando Outliers", {
				
				data = as.data.frame(data)
				data = data[, c('NRO_IDENTIFICACION', columna, columna_valor)]
				colnames(data) = c('NRO_IDENTIFICACION', columna, "VALOR")
				data$VALOR = numerize(data$VALOR)
				data = data.table(data, key= 'NRO_IDENTIFICACION')
				data = data[, list("VALOR" = sum(VALOR)), by = c('NRO_IDENTIFICACION', columna)]
					incProgress(amount = 1)
				datapacientes = data
				data = data[, list("Condicion" = quantile(VALOR, probs = percentil, na.rm = TRUE), "Frec" = length(VALOR)), by = c(columna)]
					incProgress(amount = 1)
				data = data[Frec >= frecuencia]
				
				datafinal = data.table()
				for (i in unique(data[[columna]])) {
					datatemp = data.table()
					datatemp = datapacientes[get(columna) == i]
					condicion = data[get(columna) == i, Condicion]
					datatemp = datatemp[VALOR >= condicion]
					datafinal = rbind(datafinal, datatemp)
					incProgress(amount = 1/lengthProgreso)
				}
			
		})
		
	}
		datatemp = NULL
		data = NULL
		return(datafinal)
}

outliersIQR = function(data, columna, columna_valor, multiplicativo, frecuencia = 1,session = NULL) {
	if (is.null(session)) {
		data = as.data.frame(data)
		data = data[, c('NRO_IDENTIFICACION', columna, columna_valor)]
		colnames(data) = c('NRO_IDENTIFICACION', columna, "VALOR")
		data$VALOR = numerize(data$VALOR)
			data = data.table(data, key= 'NRO_IDENTIFICACION')
			data = data[, list("VALOR" = sum(VALOR, na.rm = TRUE)), by = c('NRO_IDENTIFICACION', columna)]
			datapacientes = data
			data = data[, list("Condicion1" = quantile(VALOR, probs = 0.75, na.rm = TRUE)+((quantile(VALOR, probs = 0.75, na.rm = TRUE)-quantile(VALOR, probs = 0.25, na.rm = TRUE))*multiplicativo)
												 , "Condicion2" = quantile(VALOR, probs = 0.25, na.rm = TRUE)-((quantile(VALOR, probs = 0.75, na.rm = TRUE)-quantile(VALOR, probs = 0.25, na.rm = TRUE))*multiplicativo)
												 , "Frec" = length(VALOR)), by = c(columna)]	
			data = data[Frec >= frecuencia]
		
		datafinal = data.table()	
		for (i in unique(data[[columna]])) {
			datatemp = data.table()
			datatemp = datapacientes[get(columna) == i]
			condicion1 = data[get(columna) == i, Condicion1]
			condicion2 = data[get(columna) == i, Condicion2]
			datatemp = datatemp[!(VALOR <= condicion1 & VALOR >= condicion2)]
			datafinal = rbind(datafinal, datatemp)
		}
		
	}
	if (!is.null(session)) {
		lengthProgreso = length(data[[columna]])
		withProgress(min = 0, max = 3, message = "Calculando Outliers", {
			
			data = as.data.frame(data)
			data = data[, c('NRO_IDENTIFICACION', columna, columna_valor)]
			colnames(data) = c('NRO_IDENTIFICACION', columna, "VALOR")
			data$VALOR = numerize(data$VALOR)
			data = data.table(data, key= 'NRO_IDENTIFICACION')
			data = data[, list("VALOR" = sum(VALOR)), by = c('NRO_IDENTIFICACION', columna)]
				incProgress(amount = 1)
			datapacientes = data
			data = data[, list("Condicion1" = quantile(VALOR, probs = 0.75, na.rm = TRUE)+(quantile(VALOR, probs = 0.75, na.rm = TRUE)-quantile(VALOR, probs = 0.25, na.rm = TRUE))*multiplicativo
												 , "Condicion2" = quantile(VALOR, probs = 0.25, na.rm = TRUE)-(quantile(VALOR, probs = 0.75, na.rm = TRUE)-quantile(VALOR, probs = 0.25, na.rm = TRUE))*multiplicativo
												 , "Frec" = length(VALOR)), by = c(columna)]	
				incProgress(amount = 1)
			data = data[Frec >= frecuencia]
			
			datafinal = data.table()
			for (i in unique(data[[columna]])) {
				datatemp = data.table()
					datatemp = datapacientes[get(columna) == i]
						condicion1 = data[get(columna) == i, Condicion1]
						condicion2 = data[get(columna) == i, Condicion2]
				datatemp = datatemp[!(VALOR <= condicion1 & VALOR >= condicion2)]
				datafinal = rbind(datafinal, datatemp)
					incProgress(amount = 1/lengthProgreso)
			}
			
		})
		
	}
	datatemp = NULL
	data = NULL
	return(datafinal)
	datafinal = NULL
}

