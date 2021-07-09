parse_nt <- function(x) {

  purrr::map(x, function(y) {
    poblacion <- y[["poblacion"]]
    agrupadores_names <- names(y[["agrupadores"]])

    purrr::map2(y[["agrupadores"]], agrupadores_names, function(i, w) {
      if (is.null(names(i))) i <- list("n" = i[1], "cm" = i[2])
      i[["cm"]] <- as.double(i[["cm"]])
      i[["n"]]  <- as.double(i[["n"]])
      temp <- data.frame("agrupador" = w, "frec_mes" = as.double(i[["n"]]),
                         "cm" = as.double(i[["cm"]]),
                         "frecuencia_pc" = as.double(i[["n"]] / poblacion),
                         "valor_mes" = as.double(i[["n"]] * i[["cm"]]))
      return(temp)
    }) %>% rbindlist()

  }) %>%
    rbindlist(idcol = "nt")

}
