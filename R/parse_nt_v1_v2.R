parse_nt_v1_v2 <- function(x) {
  notas_tecnicas_v2 <- purrr::map(x, function(y) {
    if (!is.null(y[["vigente"]]))  y[["vigente"]] <- as.logical(y[["vigente"]])
    agrupadores_names <- names(y[["agrupadores"]])
    agrupadores <- purrr::map2(
      y[["agrupadores"]],
      agrupadores_names,
      function(i, w) {
        if (is.null(names(i))) i <- list("n" = i[1], "cm" = i[2])
        return(i)
      }
    )
    y[["agrupadores"]] <- agrupadores
    return(y)
  })
  return(
    as.character(
      toJSON(notas_tecnicas_v2, pretty = TRUE, auto_unbox = TRUE)
    )
  )
}
