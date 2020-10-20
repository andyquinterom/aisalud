require(bsearchtools)

AGRUPADOR_CUSTOM = function(COD, index, INDEX_COL_COD, INDEX_COL_GROUP) {
	
	
	tryCatch(
		expr = {
			if (!is.null(COD)) {
				p = bsearchtools::indexesEqualToCharacter(index[[INDEX_COL_COD]], COD[1])
				# p = which(index[[INDEX_COL_COD]] == COD[1])
				
				
				if (!identical(p, integer(0))) {
					p = min(p, na.rm = TRUE)
					return(
						as.character(index[[INDEX_COL_GROUP]][p])
					)
				}
				else {
					return(
						"No agrupado"
					)
				}
				
			}
		}
	)
	
	
}
AGRUPADOR_CUSTOM_NUM = function(COD, index, INDEX_COL_COD, INDEX_COL_GROUP) {
	
	
	tryCatch(
		expr = {
			if (!is.na(COD)) {
				p = bsearchtools::indexesEqualTo(index[[INDEX_COL_COD]], COD[1])
				# p = which(index[[INDEX_COL_COD]] == COD[1])
				
				
				if (!identical(p, integer(0))) {
					p = min(p, na.rm = TRUE)
					return(
						as.double(index[[INDEX_COL_GROUP]][p])
					)
				}
				else {
					return(
						as.double(0)
					)
				}
				
			}
		}
	)
	
	
}
