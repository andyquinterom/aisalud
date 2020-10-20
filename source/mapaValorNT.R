require("maps")
require(scales)
require(plotly)
require(colmaps2)
require(withr)
require(data.table)
require(stringr)

mapaValoresNT = function(INDICE, ...) {
	
	INDICE_SUM = NULL
	tablaDep = NULL
	p = NULL
	
	INDICE_SUM = INDICE[, list("Valor" = sum(VALOR_MES, na.rm = TRUE), "Contratos" = length(COD_NT)), by = c("COD_DEPARTAMENTO")]
	INDICE_SUM$COD_DEPARTAMENTO = numerize(INDICE_SUM$COD_DEPARTAMENTO)
	INDICE_SUM$Contratos = numerize(INDICE_SUM$Contratos)
	INDICE_SUM = INDICE_SUM[order(COD_DEPARTAMENTO)]
	
	tablaDep = data.table(
		id = numerize(departamentos@data[["id"]]),
		depto = departamentos@data[["depto"]]
	)
	
	tablaDep = tablaDep[, list("Valor" = AGRUPADOR_CUSTOM_NUM(COD = id, index = INDICE_SUM, INDEX_COL_COD = "COD_DEPARTAMENTO", INDEX_COL_GROUP = "Valor"),
							 "Contratos" = AGRUPADOR_CUSTOM_NUM(COD = id, index = INDICE_SUM, INDEX_COL_COD = "COD_DEPARTAMENTO", INDEX_COL_GROUP = "Contratos"),
							 "ID" = str_pad(id, pad = "0", width = 2)
	), by = c("id", "depto")]
	
	ValorFormato <<- paste0("Valor: ","$", format(tablaDep$Valor, scientific = FALSE, big.mark = ".", decimal.mark = ","))
	`Departamento` <<- tablaDep$depto

	p = ggplotly(
		p = colmaps2::colmap(departamentos, data = tablaDep, data_id = "ID", var = "Valor", legend = TRUE, text = "ValorFormato", nombre = "Departamento") +
			scale_fill_gradient(low = "grey", high = "dark green", name = "Valor acumulado\npor departamento\na mes", labels = dollar_format())
		, tooltip = c("text", "nombre"), ...) %>% config(locale = "es") 
	
	return(p)
	
}