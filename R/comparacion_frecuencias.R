comparacion_frecuencias <- function(frecuencias_tabla, nota_tecnica,
                                    agrupador) {

  style_interval <-ifelse(
    test = (Sys.getenv("NT_MODO_IPS") != "") %>% rep(2),
    yes = c("rgb(145, 255, 145)", "rgb(255, 190, 100)"),
    no = c("rgb(255, 190, 100)", "rgb(145, 255, 145)"))

  comparacion_frecs <- comparar_nt_frecuencias(
    frecuencias = frecuencias_tabla,
    nota_tecnica = nota_tecnica,
    agrupador = agrupador
  )

  comparacion_frecs_dt <- datatable(
    data = comparacion_frecs,
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Diferencia valor total" = "total_valor",
      "Diferencia de frecuencia total" = "total",
      "Costo medio" = "cm",
      "Frecuencia a mes" = "frec_mes",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = "none",
    options = list(
      dom = "t",
      pageLength = nrow(comparacion_frecs),
      scrollX = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatStyle(columns = 1:ncol(comparacion_frecs), backgroundColor = "white") %>%
    formatCurrency(
      table = .,
      columns = c("Valor a mes", "Diferencia valor total", "Costo medio"),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatRound(2, dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = 5:ncol(comparacion_frecs),
      backgroundColor = styleInterval(
        cuts = 0,
        values = style_interval
      ))

  comparacion_x_cme <- comparar_nt_frecuencias(
    frecuencias = frecuencias_tabla,
    nota_tecnica = nota_tecnica,
    agrupador = agrupador,
    indicador = "diff_cm")

  comparacion_x_cme_dt <- datatable(
    data = comparacion_x_cme,
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Diferencia valor total" = "total",
      "Costo medio" = "cm",
      "Frecuencia a mes" = "frec_mes",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = "none",
    options = list(
      dom = "t",
      pageLength = nrow(comparacion_x_cme),
      scrollX = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatStyle(columns = 1:ncol(comparacion_x_cme), backgroundColor = "white") %>%
    formatCurrency(
      table = .,
      columns = 3:ncol(comparacion_x_cme),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatRound(2, dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = 5:ncol(comparacion_x_cme),
      backgroundColor = styleInterval(
        cuts = 0,
        values = style_interval
      ))

  comparacion_porcentaje <- comparar_nt_frecuencias(
    frecuencias = frecuencias_tabla,
    nota_tecnica = nota_tecnica,
    agrupador = agrupador,
    indicador = "perc")

  comparacion_porcentaje_dt <- datatable(
    data = comparacion_porcentaje,
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Costo medio" = "cm",
      "Frecuencia a mes" = "frec_mes",
      "Porcentaje de ejecución medio" = "media",
      "Ejecución media a mes" = "media_valor",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = "none",
    options = list(
      dom = "t",
      pageLength = nrow(comparacion_porcentaje),
      scrollX = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatStyle(columns = 1:ncol(comparacion_porcentaje),
                backgroundColor = "white") %>%
    formatPercentage(
      table = .,
      columns = c(5:(ncol(comparacion_porcentaje) - 1)),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatCurrency(
      table = .,
      columns = c(3, 4, ncol(comparacion_porcentaje)),
      dec.mark = ",", mark = ".", digits = 0)  %>%
    formatRound(2, dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = 5:(ncol(comparacion_porcentaje) - 1),
      backgroundColor = styleInterval(
        cuts = 1,
        values = style_interval
      ))

  frecuencias_original <- frecuencias_tabla %>%
    rename(agrupador = !!as.name(agrupador)) %>%
    inner_join(nota_tecnica %>%
                 select(agrupador, frec_mes, cm)) %>%
    group_by(agrupador, frec_mes, cm) %>%
    mutate(valor_mes = frec_mes * cm) %>%
    group_by(agrupador, frec_mes, cm, valor_mes) %>%
    mutate(across(.fns = replace_na, replace = 0)) %>%
    mutate(., total = rowSums(across(), na.rm = TRUE)) %>%
    mutate(., total_valor = total * cm) %>%
    relocate(agrupador, frec_mes, cm, valor_mes)

  frecuencias_original_dt <- datatable(
    data = frecuencias_original,
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Valor total" = "total_valor",
      "Frecuencia total" = "total",
      "Costo medio" = "cm",
      "Frecuencia a mes" = "frec_mes",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = "none",
    options = list(
      dom = "t",
      pageLength = nrow(frecuencias_original),
      scrollX = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatStyle(columns = 1:ncol(frecuencias_original), backgroundColor = "white") %>%
    formatCurrency(
      table = .,
      columns = c("Valor a mes", "Valor total", "Costo medio"),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatRound(c(2, ncol(frecuencias_original) - 1),
                dec.mark = ",", mark = ".", digits = 0)

  valor_acumulado <- comparar_nt_frecuencias(
    frecuencias = frecuencias_tabla,
    nota_tecnica = nota_tecnica,
    agrupador = agrupador,
    indicador = "cm")

  print(valor_acumulado)

  valor_acumulado <- valor_acumulado %>%
    ungroup() %>%
    select(-c(cm, frec_mes, valor_mes, total, media)) %>%
    pivot_longer(
      cols = -c(agrupador),
      names_to = "mes_anio",
      values_to = "valor") %>%
    group_by(mes_anio) %>%
    summarise(suma = sum(valor, na.rm = TRUE)) %>%
    mutate(
      mes_anio_num = mes_spanish_inv(mes_anio),
      ais_mes = mes_anio_num %% 100,
      ais_anio = mes_anio_num %/% 100) %>%
    mutate(numero_meses = 1:nrow(.), suma = replace_na(suma, 0),
           mes_anio = mes_spanish_juntos(mes_anio_num),
           mes_anio_num = do.call(purrr::map(
             .x = as.Date(paste(ais_anio, ais_mes, "01", sep = "-")),
             .f = function(x) last(seq(x, length = 2, by = "months") - 1)),
             what = "c")) %>%
    arrange(mes_anio_num) %>%
    ungroup() %>%
    mutate(valor_acumulado = cumsum(suma),
           valor_mes_esperado = as.double(sum(nota_tecnica$valor_mes)),
           numero_meses = 1:nrow(.),
           valor_a_ejecutar = valor_mes_esperado * numero_meses)

  numero_meses <- nrow(valor_acumulado)
  valor_a_ejecutar <- sum(nota_tecnica$valor_mes) * numero_meses
  valor_ejecutado <- sum(frecuencias_original$total_valor)

  totales <- list(
    "Valor a ejecutar:" = formatAsCurrency(valor_a_ejecutar),
    "Valor ejecutado con costo medio:" = formatAsCurrency(valor_ejecutado),
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
      fill = "tonexty", fillcolor = "rgba(0,100,80,0.2)"
    ) %>%
    layout(
      legend = list(x = 0.1, y = 0.9),
      xaxis = list(title = "Mes"),
      yaxis = list(title = "Suma",
                   tickformat = ",.2f")
    ) %>%
    config(locale = "es")


  return(list(comparacion_frecs = comparacion_frecs,
              comparacion_frecs_dt = comparacion_frecs_dt,
              comparacion_x_cme = comparacion_x_cme,
              comparacion_x_cme_dt = comparacion_x_cme_dt,
              comparacion_porcentaje = comparacion_porcentaje,
              comparacion_porcentaje_dt = comparacion_porcentaje_dt,
              frecuencias_original = frecuencias_original,
              frecuencias_original_dt = frecuencias_original_dt,
              totales = totales_ui,
              valor_acumulado = valor_acumulado,
              plot_valor_acumulado = plot_valor_acumulado
  ))

}
