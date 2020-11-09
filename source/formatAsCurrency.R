formatAsCurrency <- function(x) {
  x <- as.numeric(as.character(x))
  return(
    paste0(
      "$",
      format(
        x,
        digits = 0,
        big.mark = ".", 
        decimal.mark = ",",
        scientific = F))
  )
}

formatAsPerc <- function(x) {
  x <- as.numeric(as.character(x))
  return(
    paste0(
      format(
        x,
        digits = 0,
        big.mark = ".", 
        decimal.mark = ",",
        scientific = F),
      "%")
  )
}