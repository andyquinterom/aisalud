#
# Analítica Integrada Salud
#
# Derechos de autor 2021 por MD&CO Consulting Group (NIT 901.119.781-5)
# Copyright (C) 2021 by MD&CO Consulting Group
#
# Este programa es software libre: puede redistribuirlo o modificarlo bajo
# los términos de la licencia Affero General Public License tal cual
# publicada por la Free Software Foundation, sea la versión 3 de la licencia
# o cualquier versión posterior. Este programa se distribuye SIN GARANTÍA
# EXPERSA O IMPLÍCITA, INCLUIDAS LAS DE NO INFRACCIÓN, COMERCIABILIDAD O
# APTITUD PARA UN PROPÓSITO PARTICULAR. Referir a la
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) para más detalles.
#

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
      "Agrupador" = "agrupador",
      "Unidad de conteo" = "unidad_conteo"),
    rownames = FALSE,
    selection = "none",
    extensions = c("FixedColumns"),
    options = list(
      dom = "t",
      pageLength = nrow(comparaciones[["suma"]]),
      fixedColumns = list(leftColumns = 3),
      scrollX = TRUE,
      scrollY = "300px",
      scrollCollapse = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatCurrency(
      columns = 3:ncol(comparaciones[["suma"]]),
      dec.mark = ",", mark = ".", digits = 0)

  comparacion_diff_dt <- datatable(
    data = comparaciones[["diff"]],
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Diferencia de valor total" = "total",
      "Agrupador" = "agrupador",
      "Unidad de conteo" = "unidad_conteo"),
    rownames = FALSE,
    selection = "none",
    extensions = c("FixedColumns"),
    options = list(
      dom = "t",
      pageLength = nrow(comparaciones[["suma"]]),
      fixedColumns = list(leftColumns = 3),
      scrollX = TRUE,
      scrollY = "300px",
      scrollCollapse = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatCurrency(
      columns = 3:ncol(comparaciones[["diff"]]),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = c(4:ncol(comparaciones[["diff"]]),
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
      "Agrupador" = "agrupador",
      "Unidad de conteo" = "unidad_conteo"),
    rownames = FALSE,
    selection = "none",
    extensions = c("FixedColumns"),
    options = list(
      dom = "t",
      pageLength = nrow(comparaciones[["suma"]]),
      fixedColumns = list(leftColumns = 2, rightColumns = 1),
      scrollX = TRUE,
      scrollY = "300px",
      scrollCollapse = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatPercentage(
      columns = 4:(ncol(comparaciones[["perc"]]) - 1),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = 4:(ncol(comparaciones[["perc"]]) - 1),
      backgroundColor = styleInterval(
        cuts = 1,
        values = style_interval
      )) %>%
    formatCurrency(
      columns = c(3, ncol(comparaciones[["perc"]])),
      dec.mark = ",", mark = ".", digits = 0)

  valor_mes <- sum(nota_tecnica$valor_mes)

  valor_acumulado <- descriptiva_tabla %>%
    completar_meses(
      agrupador = agrupador,
      col_anio = col_anio,
      col_mes = col_mes) %>%
    rename(agrupador = !!rlang::sym(agrupador)) %>%
    right_join(nota_tecnica) %>%
    arrange(mes_anio_num) %>%
    group_by(ais_anio, ais_mes, mes_anio_num) %>%
    summarise(Suma = sum(Suma, na.rm = TRUE)) %>%
    filter(!is.na(ais_mes) || !is.na(ais_anio)) %>%
    ungroup() %>%
    mutate(
      numero_meses = 1:n(),
      mes_anio = mes_spanish_juntos(mes_anio_num),
      fecha_ejec = ym(mes_anio_num) + months(1) - days(1)) %>%
    mutate(Suma = replace_na(Suma, 0),
           valor_acumulado = cumsum(Suma), valor_mes_esperado = valor_mes,
           valor_a_ejecutar = valor_mes * as.double(numero_meses))

  plot_valor_acumulado <- valor_acumulado %>%
    plot_ly(
      x = ~fecha_ejec,
      y = ~valor_acumulado,
      name = "Valor ejecutado",
      type = "scatter", mode = "lines+markers"
    ) %>%
    add_trace(
      y = ~valor_a_ejecutar,
      name = "Valor a ejecutar",
      mode = "lines",
      line = list(color = "rgb(205, 12, 24)", dash = "dash"),
      fill = "tonexty", fillcolor = "rgba(0,100,80,0.2)"
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
