comparacion_frecuencias <- function(frecuencias_tabla, nota_tecnica,
                                    agrupador) {

  style_interval <-ifelse(
    test = (Sys.getenv("NT_MODO_IPS") != "") %>% rep(2),
    yes = c("rgb(145, 255, 145)", "rgb(255, 190, 100)"),
    no = c("rgb(255, 190, 100)", "rgb(145, 255, 145)"))

  ejecucion_base <- comparar_nt_frecuencias(
    frecuencias = frecuencias_tabla,
    nota_tecnica = nota_tecnica,
    agrupador = agrupador
  )

  ejecucion_base_dt <- datatable(
    data = ejecucion_base,
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Ejecución total x CM" = "frec_suma_por_cm",
      "Ejecución media x CM" = "frec_media_por_cm",
      "Costo medio" = "cm",
      "Frecuencia a mes" = "frec_mes",
      "Ejecución media" = "frec_media",
      "Ejecución total" = "frec_suma",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = "none",
    extensions = c("FixedColumns"),
    options = list(
      dom = "t",
      scrollCollapse = TRUE,
      fixedColumns = list(leftColumns = 1),
      scrollY = "300px",
      pageLength = nrow(ejecucion_base),
      scrollX = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatStyle(columns = seq_len(ncol(ejecucion_base)),
      backgroundColor = "white") %>%
    formatCurrency(
      table = .,
      columns = c("Valor a mes", "Ejecución total x CM", "Costo medio",
        "Ejecución media x CM"),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatRound(2, dec.mark = ",", mark = ".", digits = 3)

  valor_acumulado <- ejecucion_base %>%
  mutate(across(.fns = ~.x * cm))

  diferencias_por_cm <- valor_acumulado %>%
    mutate(across(.fns = ~.x - valor_mes)) %>%
    ungroup() %>%
    select(-c(cm, frec_mes, frec_suma, frec_media, frec_media_por_cm,
      frec_suma_por_cm)) %>%
    group_by(agrupador, valor_mes) %>%
    mutate(diferencia_total_por_cm = rowSums(across(), na.rm = TRUE)) %>%
    relocate(agrupador, valor_mes, diferencia_total_por_cm)

  diferencias_por_cm_dt <- datatable(
    data = diferencias_por_cm,
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Agrupador" = "agrupador",
      "Diferencia total" = "diferencia_total_por_cm"),
    rownames = FALSE,
    selection = "none",
    extensions = c("FixedColumns"),
    options = list(
      dom = "t",
      scrollCollapse = TRUE,
      fixedColumns = list(leftColumns = 2),
      scrollY = "300px",
      pageLength = nrow(diferencias_por_cm),
      scrollX = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatStyle(columns = seq_len(ncol(diferencias_por_cm)),
      backgroundColor = "white") %>%
    formatCurrency(
      table = .,
      columns = 2:ncol(diferencias_por_cm),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatStyle(
      columns = 3:ncol(diferencias_por_cm),
      backgroundColor = styleInterval(
        cuts = 0,
        values = style_interval
      ))

  ejecucion_base_por_cm <- valor_acumulado %>%
    ungroup() %>%
    select(-c(cm, frec_mes, frec_suma, frec_media, frec_media_por_cm,
        frec_suma_por_cm))

  ejecucion_base_por_cm_dt <- datatable(
    data = ejecucion_base_por_cm,
    colnames = c(
      "Valor a mes" = "valor_mes",
      "Agrupador" = "agrupador"),
    rownames = FALSE,
    selection = "none",
    extensions = c("FixedColumns"),
    options = list(
      dom = "t",
      scrollCollapse = TRUE,
      fixedColumns = list(leftColumns = 2),
      scrollY = "300px",
      pageLength = nrow(ejecucion_base_por_cm),
      scrollX = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatStyle(columns = seq_len(ncol(ejecucion_base_por_cm)),
      backgroundColor = "white") %>%
    formatCurrency(
      table = .,
      columns = 2:ncol(ejecucion_base_por_cm),
      dec.mark = ",", mark = ".", digits = 0)

  valor_acumulado <- valor_acumulado %>%
    ungroup() %>%
    select(-c(cm, frec_mes, valor_mes, frec_suma, frec_media,
        frec_media_por_cm, frec_suma_por_cm)) %>%
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
    mutate(numero_meses = seq_len(nrow(.)), suma = replace_na(suma, 0),
           mes_anio = mes_spanish_juntos(mes_anio_num),
           mes_anio_num = do.call(purrr::map(
             .x = as.Date(paste(ais_anio, ais_mes, "01", sep = "-")),
             .f = function(x) last(seq(x, length = 2, by = "months") - 1)),
             what = "c")) %>%
    arrange(mes_anio_num) %>%
    ungroup() %>%
    mutate(valor_acumulado = cumsum(suma),
           valor_mes_esperado = as.double(sum(nota_tecnica$valor_mes)),
           numero_meses = seq_len(nrow(.)),
           valor_a_ejecutar = valor_mes_esperado * numero_meses)

  numero_meses <- nrow(valor_acumulado)
  valor_a_ejecutar <- sum(nota_tecnica$valor_mes) * numero_meses
  valor_ejecutado <- sum(ejecucion_base$frec_suma_por_cm)

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

  data_list <- list(
    "Ejecución de frecuencias" = ejecucion_base,
    "Dif de frecuencias por CM" = diferencias_por_cm,
    "Ejecución de frecuencias por CM" = ejecucion_base_por_cm)

  ui_list <- list(
    ejecucion_base = ejecucion_base_dt,
    ejecucion_base_por_cm = ejecucion_base_por_cm_dt,
    diferencias_por_cm = diferencias_por_cm_dt,
    totales = totales_ui,
    plot_valor_acumulado = plot_valor_acumulado)

  return(
    list(
      ui = ui_list,
      data = data_list
    )
  )

}
