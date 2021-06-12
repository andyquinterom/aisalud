comparacion_valor_facturado <- function(
  descriptiva_tabla, nota_tecnica, agrupador, col_mes = "ais_mes",
  col_anio = "ais_anio") {

  descriptiva_tabla <- descriptiva_tabla %>%
    mutate(ais_mes_anio = !!rlang::sym(col_anio) * 100 + !!rlang::sym(col_mes))

  style_interval <- ifelse(
    test = (Sys.getenv("NT_MODO_IPS") != "") %>% rep(2),
    yes = c("rgb(145, 255, 145)", "rgb(255, 190, 100)"),
    no = c("rgb(255, 190, 100)", "rgb(145, 255, 145)"))

  comparaciones <- purrr::map(
    .x = list("suma" = "suma", "diff" = "diff", "perc" = "perc"),
    .f = comparar_nt_valor_factura,
    descriptiva_tabla = descriptiva_tabla,
    nota_tecnica = nota_tecnica,
    agrupador = agrupador,
    col_anio = col_anio,
    col_mes = col_mes
  )

  comparacion_suma_dt <- datatable(
    data = comparaciones[["suma"]],
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Suma valor total" = "total",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = "none",
    extensions = c("FixedColumns"),
    options = list(
      dom = "t",
      pageLength = nrow(comparaciones[["suma"]]),
      fixedColumns = list(leftColumns = 2),
      scrollX = TRUE,
      scrollY = "300px",
      scrollCollapse = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatCurrency(
      columns = 2:ncol(comparaciones[["suma"]]),
      dec.mark = ",", mark = ".", digits = 0)

  comparacion_diff_dt <- datatable(
    data = comparaciones[["diff"]],
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Diferencia de valor total" = "total",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = "none",
    extensions = c("FixedColumns"),
    options = list(
      dom = "t",
      pageLength = nrow(comparaciones[["suma"]]),
      fixedColumns = list(leftColumns = 2),
      scrollX = TRUE,
      scrollY = "300px",
      scrollCollapse = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatCurrency(
      columns = 2:ncol(comparaciones[["diff"]]),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = c(3:ncol(comparaciones[["diff"]]),
                  ncol(comparaciones[["diff"]])),
      backgroundColor = styleInterval(
        cuts = 0,
        values = style_interval
      ))

  comparacion_perc_dt <- datatable(
    data = comparaciones[["perc"]],
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Porcentaje de ejecución medio" = "media",
      "Ejecución media a mes" = "media_valor",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = "none",
    extensions = c("FixedColumns"),
    options = list(
      dom = "t",
      pageLength = nrow(comparaciones[["suma"]]),
      fixedColumns = list(leftColumns = 1, rightColumns = 1),
      scrollX = TRUE,
      scrollY = "300px",
      scrollCollapse = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatPercentage(
      columns = 3:(ncol(comparaciones[["perc"]]) - 1),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = 3:(ncol(comparaciones[["perc"]]) - 1),
      backgroundColor = styleInterval(
        cuts = 1,
        values = style_interval
      )) %>%
    formatCurrency(
      columns = c(2, ncol(comparaciones[["perc"]])),
      dec.mark = ",", mark = ".", digits = 0)

  minimo_mes <- min(pull(descriptiva_tabla, ais_mes_anio))
  maximo_mes <- max(pull(descriptiva_tabla, ais_mes_anio))
  valor_mes <- sum(nota_tecnica$valor_mes)

  meses_completos <- tibble("ais_mes_anio" = seq(minimo_mes, maximo_mes)) %>%
    mutate(ais_anio = substr(ais_mes_anio, 1, 4) %>% as.numeric(),
           ais_mes  = substr(ais_mes_anio, 5, 6) %>% as.numeric()) %>%
    filter(ais_mes >= 1 & ais_mes <= 12) %>%
    select(-ais_mes_anio)

  valor_acumulado <- meses_completos %>%
    left_join(
      descriptiva_tabla %>%
        rename(agrupador = !!as.name(agrupador)) %>%
        left_join(nota_tecnica %>%
                    select(agrupador)) %>%
        arrange(!!rlang::sym(col_anio), !!rlang::sym(col_mes)) %>%
        group_by(!!rlang::sym(col_anio), !!rlang::sym(col_mes)) %>%
        summarise(Suma = sum(Suma, na.rm = TRUE)) %>%
        select(!!!rlang::syms(c(col_anio, col_mes)), Suma),
      by = c("ais_mes" = col_mes, "ais_anio" = col_anio)) %>%
    mutate(numero_meses = 1:nrow(.),
           mes_anio_num = ais_anio * 100 + ais_mes,
           mes_anio = mes_spanish_juntos(mes_anio_num),
           mes_anio_num = do.call(purrr::map(
             .x = as.Date(paste(ais_anio, ais_mes, "01", sep = "-")),
             .f = function(x) last(seq(x, length = 2, by = "months") - 1)),
             what = "c")) %>%
    mutate(Suma = replace_na(Suma, 0),
           valor_acumulado = cumsum(Suma), valor_mes_esperado = valor_mes,
           valor_a_ejecutar = valor_mes * as.double(numero_meses))

  plot_valor_acumulado <- valor_acumulado %>%
    plot_ly(
      x = ~mes_anio_num,
      y = ~valor_acumulado,
      name = "Valor ejecutado",
      type = "scatter", mode = "lines+markers"
    ) %>%
    add_trace(
      y = ~valor_a_ejecutar,
      name = "Valor a ejecutar",
      mode = "lines",
      line = list(color = "rgb(205, 12, 24)", dash = "dash"),
      fill = "tonexty", fillcolor="rgba(0,100,80,0.2)"
    ) %>%
    config(locale = "es") %>%
    layout(
      legend = list(x = 0.1, y = 0.9),
      xaxis = list(title = "Mes"),
      yaxis = list(title = "Suma",
                   tickformat = ",.2f")
    )

  numero_meses <- nrow(valor_acumulado)
  valor_a_ejecutar <- numero_meses * as.double(valor_mes)
  valor_ejecutado <- sum(valor_acumulado$Suma, na.rm = TRUE)

  totales <- list(
    "Valor a ejecutar:" = formatAsCurrency(valor_a_ejecutar),
    "Valor facturado:" = formatAsCurrency(valor_ejecutado),
    "Diferencia de valor total:" = formatAsCurrency(valor_a_ejecutar -
                                                      valor_ejecutado),
    "Porcentaje del valor ejecutado:" = formatAsPerc(
      100 * valor_ejecutado / na_if(valor_a_ejecutar, 0)))

  totales_ui <- purrr::map2(
    .x = totales, .y = names(totales),
    .f = function(x, y) {
      tagList(
        tags$b(y),
        tags$p(x)
      )
    }) %>%
    tagList()

  ui_list <- list(
    "comparacion_suma" = comparacion_suma_dt,
    "comparacion_diff" = comparacion_diff_dt,
    "comparacion_porcentaje" = comparacion_perc_dt,
    "plot_valor_acumulado" = plot_valor_acumulado,
    "totales" = totales_ui
  )

  data_list <- list(
    "Totales de valor facturado" = comparaciones[["suma"]],
    "Diferencia con valor facturado" = comparaciones[["diff"]],
    "Ejecución de valor en %" = comparaciones[["perc"]]
  )

  return(
    list(
      ui = ui_list,
      data = data_list
    )
  )

}
