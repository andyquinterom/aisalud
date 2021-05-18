cbind.fill <- function(nm, agrupador){
  if (length(nm) == 1) {
    return(nm)
  }
  if (length(nm) > 1) {
    merged <- merge.data.table(nm[[1]], nm[[2]], by = agrupador, all = TRUE)
    nm <- append(list(merged), nm[-c(1:2)])
    cbind.fill(nm, agrupador = agrupador)
  }
}

mes_spanish <- function(x) {
  meses <- c(
    "Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "Septiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
  )
  return(
    meses[x]
  )
  
}

mes_spanish_juntos <- function(x) {
  meses <- c(
    "Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "Septiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
  )
  anio <- substr(x, 1, 4)
  mes <- meses[as.numeric(substr(x, 5, 6))]
  return(
    paste(anio, mes, sep = " - ")
  )
  
}

mes_spanish_inv <- function(x) {
  meses <- c(
    "Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "Septiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
  )
  
  divididos <- str_split(x, " - ")
  
  divididos_numerico <- purrr::map(
    .x = divididos,
    .f = function(x) {
      as.numeric(x[1]) * 100 + which(x[2] == meses)
    })
  
  return(unlist(divididos_numerico))
  
  
}

comparar_nt_frecuencias <- function(frecuencias, nota_tecnica, agrupador,
                                    indicador = "diff") {
  
  frec_y_nt <- frecuencias %>%
    rename(agrupador = !!as.name(agrupador)) %>%
    inner_join(nota_tecnica %>%
                 select(agrupador, frec_mes, cm)) %>%
    group_by(agrupador, frec_mes, cm) %>%
    mutate(valor_mes = frec_mes * cm) %>%
    group_by(agrupador, frec_mes, cm, valor_mes)
  
  frec_y_nt %>%
    mutate(across(.fns = replace_na, replace = 0)) %>%
    {if (indicador == "diff") {
      mutate(., across(.fns = ~ .x - frec_mes))
    } else if (indicador == "perc") {
      mutate(., across(.fns = ~ .x / na_if(frec_mes, 0))) %>%
      mutate(media = rowMeans(across(), na.rm = TRUE),
             media_valor = media * valor_mes)
    } else if (indicador == "diff_cm") {
      mutate(., across(.fns = ~ (.x - frec_mes) * cm))
    } else if (indicador == "cm") {
      mutate(., across(.fns = ~ .x * cm))
    } else {.}} %>%
    {if (indicador %in% c("diff", "diff_cm")) {
      mutate(., total = rowSums(across(), na.rm = TRUE))
    } else {.}} %>%
    {if (indicador == "diff") {
      mutate(., total_valor = total * cm)
    } else {.}} %>%
    relocate(agrupador, frec_mes, cm, valor_mes) %>%
    return()
  
}

comparar_nt_valor_factura <- function(
  indicador, descriptiva_tabla, nota_tecnica, agrupador, col_anio, col_mes) {
  
  descriptiva_tabla %>%
    select(!!!rlang::syms(c(agrupador, col_anio, col_mes)), Suma) %>%
    rename(agrupador = !!as.name(agrupador)) %>%
    arrange(!!rlang::sym(col_anio), !!rlang::sym(col_mes)) %>%
    mutate(ais_mes_nombre = mes_spanish(!!!rlang::syms(col_mes))) %>%
    pivot_wider(
      id_cols = c(agrupador),
      names_from = c(ais_anio, ais_mes_nombre),
      values_from = Suma,
      names_sep = " - ") %>%
    inner_join(nota_tecnica %>%
                 select(agrupador, valor_mes)) %>%
    mutate(across(.fns = replace_na, replace = 0)) %>%
    group_by(agrupador, valor_mes) %>%
    relocate(agrupador, valor_mes) %>%
    {if (indicador == "diff") {
      mutate(., across(.fns = ~ .x - valor_mes))
    } else if (indicador == "perc") {
      mutate(., across(.fns = ~ .x / na_if(valor_mes, 0)))
    } else {.}} %>%
    {if (indicador %in% c("diff", "suma")) {
      mutate(., total = rowSums(across(), na.rm = TRUE))
    } else if (indicador == "perc") {
      mutate(., media = rowMeans(across(), na.rm = TRUE),
             media_valor = media * valor_mes)
    } else {.}}
  
}

comparacion_frecuencias <- function(frecuencias_tabla, nota_tecnica, agrupador) {
  
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
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(comparacion_frecs),
      scrollX = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatStyle(columns = 1:ncol(comparacion_frecs), backgroundColor = 'white') %>%
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
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(comparacion_x_cme),
      scrollX = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatStyle(columns = 1:ncol(comparacion_x_cme), backgroundColor = 'white') %>%
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
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(comparacion_porcentaje),
      scrollX = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatStyle(columns = 1:ncol(comparacion_porcentaje),
                backgroundColor = 'white') %>%
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
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(frecuencias_original),
      scrollX = TRUE,
      language = list(
        url = dt_spanish))) %>%
    formatStyle(columns = 1:ncol(frecuencias_original), backgroundColor = 'white') %>%
    formatCurrency(
      table = .,
      columns = c("Valor a mes", "Valor total", "Costo medio"),
      dec.mark = ",", mark = ".", digits = 0) %>%
    formatRound(c(2, ncol(frecuencias_original) - 1),
                dec.mark = ",", mark = ".", digits = 0)
  
  meses <- frecuencias_tabla %>%
    select(-c(rlang::sym(agrupador))) %>%
    colnames() %>%
    mes_spanish_inv() %>%
    as.numeric()
  
  minimo_mes <- min(meses)
  maximo_mes <- max(meses)
  
  meses_completos <- tibble("ais_mes_anio" = seq(minimo_mes, maximo_mes)) %>%
    mutate(ais_anio = substr(ais_mes_anio, 1, 4) %>% as.numeric(),
           ais_mes  = substr(ais_mes_anio, 5, 6) %>% as.numeric()) %>%
    filter(ais_mes >= 1 & ais_mes <= 12) %>%
    rename(mes_anio_num = ais_mes_anio)
  
  valor_acumulado <- meses_completos %>%
    left_join(
      comparar_nt_frecuencias(
        frecuencias = frecuencias_tabla,
        nota_tecnica = nota_tecnica,
        agrupador = agrupador,
        indicador = "cm") %>%
        ungroup() %>%
        select(-c(cm, frec_mes, valor_mes)) %>%
        pivot_longer(
          cols = -c(agrupador),
          names_to = "mes_anio",
          values_to = "valor") %>%
        group_by(mes_anio) %>%
        summarise(suma = sum(valor, na.rm = TRUE)) %>%
        mutate(mes_anio_num = mes_spanish_inv(mes_anio))
    ) %>%
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
      line = list(color = 'rgb(205, 12, 24)', dash = 'dash'),
      fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)'
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

comparacion_valor_facturado <- function(
  descriptiva_tabla, nota_tecnica, agrupador, col_mes = "ais_mes",
  col_anio = "ais_anio") {
  
  descriptiva_tabla <- descriptiva_tabla %>%
    mutate(ais_mes_anio = !!rlang::sym(col_anio) * 100 + !!rlang::sym(col_mes))
  
  style_interval <-ifelse(
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
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(comparaciones[["suma"]]),
      scrollX = TRUE,
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
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(comparaciones[["diff"]]),
      scrollX = TRUE,
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
    selection = 'none',
    options = list(
      dom = 't',
      pageLength = nrow(comparaciones[["perc"]]),
      scrollX = TRUE,
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
      line = list(color = 'rgb(205, 12, 24)', dash = 'dash'),
      fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)'
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
  
  return(list(comparacion_suma = comparaciones[["suma"]],
              comparacion_suma_dt = comparacion_suma_dt,
              comparacion_diff = comparaciones[["diff"]],
              comparacion_diff_dt = comparacion_diff_dt,
              comparacion_porcentaje = comparaciones[["perc"]],
              comparacion_porcentaje_dt = comparacion_perc_dt,
              valor_acumulado = valor_acumulado,
              plot_valor_acumulado = plot_valor_acumulado,
              totales = totales_ui
  ))
  
  
}
