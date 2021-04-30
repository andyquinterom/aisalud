# La función counter permite crear un counter independiente y sandboxed.
# La funcion devuelve una función la cual al correrse incrementa el valor
# por 1. 
counter <- function() {
  starting_val <- 0
  counter_fn <- function() {
    starting_val <<- starting_val + 1
  }
}
