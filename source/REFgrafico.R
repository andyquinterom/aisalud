ref_plot <- function(paquetes, referente, cups, valor_costo) {
  
  referente.completo <- as.data.table(
    rbind(fill = TRUE,
          paquetes[, list("cums_cups" = cums_cups,
                          "p" = "I",
                          ref_valor = valor,
                          ref_costo = costo,
                          "TIPO" = "INSTITUCIONAL")],
          cbind(referente, data.frame(
            "TIPO" = rep("REFERENTE", nrow(referente))))))
  
  print(referente.completo)
      
  promedios <- referente.completo[cums_cups %in% cups, list(
    p,
    costomean = mean(ref_costo, na.rm = T), 
    costomin = min(ref_costo, na.rm = T),
    costomax = max(ref_costo, na.rm = T),
    valormean = mean(ref_valor, na.rm = T),
    valormin = min(ref_valor, na.rm = T),
    valormax = max(ref_valor, na.rm = T),
    VALORMEDIA = paste0("MEDIA: ",
                        formatAsCurrency(mean(ref_valor, na.rm = T))),
    VALORMIN = paste0("MÍNIMO: ",
                      formatAsCurrency(min(ref_valor, na.rm = T))),
    VALORMAX = paste0("MÁXIMO: ",
                      formatAsCurrency(max(ref_valor, na.rm = T))),
    COSTOMEDIA = paste0("MEDIA: ",
                        formatAsCurrency(mean(ref_costo, na.rm = T))),
    COSTOMIN = paste0("MÍNIMO: ",
                      formatAsCurrency(min(ref_costo, na.rm = T))),
    COSTOMAX = paste0("MÁXIMO: ",
                      formatAsCurrency(max(ref_costo, na.rm = T)))
  ),
  by = c("TIPO")]
  
  if (valor_costo == "valor") {
    REF <-   "ref_valor"
    MEDIA <- "VALORMEDIA"
    MAX <-   "VALORMAX"
    MIN <-   "VALORMIN"
    ymean <- "valormean"
    ymin <-  "valormin"
    ymax <-  "valormax"
  }
  else if (valor_costo == "costo") {
    REF <-   "ref_costo"
    MEDIA <- "COSTOMEDIA"
    MAX <-   "COSTOMAX"
    MIN <-   "COSTOMIN"
    ymean <- "costomean"
    ymin <-  "costomin"
    ymax <-  "costomax"
  }
  
  plot1 <-
    ggplot(
      data = referente.completo[cums_cups %in% cups],
      aes(x = p,
          y = get(REF),
          fill = TIPO,
          tooltip = format(get(REF),
                           digits = 0,
                           big.mark = ".",
                           decimal.mark = ",",
                           scientific = F),
          data_id = p)) +
    geom_col_interactive() +
    geom_errorbar_interactive(
      data = promedios[TIPO == "REFERENTE"],
      aes(p, ymax = get(ymean), 
          ymin = get(ymean),
          color = get(MEDIA), 
          tooltip = format(get(ymean),
                           scientific = F,
                           big.mark = ".",
                           decimal.mark = ",",
                           digits = 0)),
      size=0.5, 
      linetype = "longdash",
      inherit.aes = F,
      width = 1) +
    geom_errorbar_interactive(
      data = promedios[TIPO == "REFERENTE"],
      aes(
        p,
        ymax = get(ymax),
        ymin = get(ymax),
        color = get(MAX),
        tooltip = format(get(ymax),
                         scientific = F,
                         big.mark = ".",
                         decimal.mark = ",",
                         digits = 0)),
      size=0.5,
      linetype = "longdash",
      inherit.aes = F,
      width = 1) +
    geom_errorbar_interactive(
      data = promedios[TIPO == "REFERENTE"],
      aes(
        p,
        ymax = get(ymin),
        ymin = get(ymin),
        color = get(MIN),
        tooltip = format(get(ymin),
                         scientific = F,
                         big.mark = ".",
                         decimal.mark = ",",
                         digits = 0)),
      size=0.5,
      linetype = "longdash",
      inherit.aes = F,
      width = 1) +
    scale_y_continuous(labels = scales::comma, name = toupper(valor_costo)) +
    scale_x_discrete(name = "") + 
    scale_fill_manual(breaks = c("INSTITUCIONAL", "REFERENTE"),
                      values = c("blue", "dark grey")) +
    scale_color_manual("DATOS DEL MERCADO",
                       values = c("blue", "black", "blue")) +
   theme_minimal()
  
  return(
    girafe(ggobj = plot1,
           options = list(opts_selection(type = "none", only_shiny = FALSE)),
           width_svg = 8,
           height_svg = 6)
    )
}
