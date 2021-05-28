parse_nt_indice <- function(x, tabla_agrupadores) {

  nombres_nt <- names(x)

  indice_datos <- purrr::map2(x, nombres_nt,  function(y, cod_nt) {
    prestador <- y[["prestador"]]
    asegurador <- y[["asegurador"]]

    tibble(
      cod_nt = cod_nt,
      nom_prestador = NA,
      nom_asegurador = NA,
      poblacion = as.double(y[["poblacion"]]),
      departamento = y[["departamento"]],
      ciudades = y[["ciudades"]],
      cod_departamento = y[["cod_departamento"]],
      vigente = ifelse(is.null(y[["vigente"]]),
                               yes = FALSE,
                               no = as.logical(y[["vigente"]]))
    ) %>%
      {if (!is.null(prestador)) {
        mutate(., nom_prestador = y[["prestador"]])
      } else {.}} %>%
      {if (!is.null(asegurador)) {
        mutate(., nom_asegurador = y[["asegurador"]])
      } else {.}}
  }) %>%
    rbindlist(fill = TRUE, use.names = TRUE) %>%
    select(where(~!all(is.na(.x))))

  valores_mes <- tabla_agrupadores %>%
    group_by(nt) %>%
    summarise(valor_mes = sum(valor_mes, na.rm = TRUE))

  indice <- indice_datos %>%
    inner_join(valores_mes, by = c("cod_nt" = "nt")) %>%
    mutate(cod_departamento = as.character(cod_departamento)) %>%
    return()

}
