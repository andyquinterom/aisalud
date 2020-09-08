formatAsCurrency = function(x) {
	x = as.numeric(as.character(x))
	return(paste0("$", format(x, digits = 0, big.mark = ",", scientific = F)))
}

formatAsPerc = function(x) {
	x = as.numeric(as.character(x))
	return(
		paste0(format(x*100, digits = 2, scientific = F), "%")
	)
}