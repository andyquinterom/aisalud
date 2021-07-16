# Función para extraer los valores únicos de una columna en un vector
pull_distinct <- function(data, col) {
  data %>%
    select(!!rlang::sym(col)) %>%
    distinct() %>%
    pull(!!rlang::sym(col))
}
