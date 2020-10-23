REFgrafico <- function(paquetes, referente, CUMSCUPS, valorcosto) {
  
  referente.completo <- as.data.table(  
    rbind(fill = TRUE,
          paquetes[, list(`CUMS/CUPS`, `CODIGO PAQUETE`,
                          "P" = "I",
                          `REF VALOR` = VALOR,
                          `REF COSTO` = COSTO,
                          "TIPO" = "INSTITUCIONAL")],
          cbind(referente, data.frame(
            "TIPO" = rep("REFERENTE", nrow(referente))))))
      
  promedios <- referente.completo[`CUMS/CUPS` %in% CUMSCUPS, list(
    P,
    costomean = mean(`REF COSTO`, na.rm = T), 
    costomin = min(`REF COSTO`, na.rm = T),
    costomax = max(`REF COSTO`, na.rm = T),
    valormean = mean(`REF VALOR`, na.rm = T),
    valormin = min(`REF VALOR`, na.rm = T),
    valormax = max(`REF VALOR`, na.rm = T),
    VALORMEDIA = paste0("MEDIA: ",
                        formatAsCurrency(mean(`REF VALOR`, na.rm = T))),
    VALORMIN = paste0("MÍNIMO: ",
                      formatAsCurrency(min(`REF VALOR`, na.rm = T))),
    VALORMAX = paste0("MÁXIMO: ",
                      formatAsCurrency(max(`REF VALOR`, na.rm = T))),
    COSTOMEDIA = paste0("MEDIA: ",
                        formatAsCurrency(mean(`REF COSTO`, na.rm = T))),
    COSTOMIN = paste0("MÍNIMO: ",
                      formatAsCurrency(min(`REF COSTO`, na.rm = T))),
    COSTOMAX = paste0("MÁXIMO: ",
                      formatAsCurrency(max(`REF COSTO`, na.rm = T)))
    ),
    by=c("TIPO")]
  
  if (valorcosto == "VALOR") {
    REF <-   "REF VALOR"
    MEDIA <- "VALORMEDIA"
    MAX <-   "VALORMAX"
    MIN <-   "VALORMIN"
    ymean <- "valormean"
    ymin <-  "valormin"
    ymax <-  "valormax"
  }
  else if (valorcosto == "COSTO") {
    REF <-   "REF COSTO"
    MEDIA <- "COSTOMEDIA"
    MAX <-   "COSTOMAX"
    MIN <-   "COSTOMIN"
    ymean <- "costomean"
    ymin <-  "costomin"
    ymax <-  "costomax"
  }
  
  plot1 <-
    ggplot(
      data = referente.completo[`CUMS/CUPS` %in% CUMSCUPS],
      aes(x = P,
          y = get(REF),
          fill = TIPO,
          tooltip = format(get(REF),
                           digits = 0,
                           big.mark = ",",
                           scientific = F),
          data_id = P)) +
    geom_col_interactive() +
    geom_errorbar_interactive(
      data = promedios[TIPO == "REFERENTE"],
      aes(P, ymax = get(ymean), 
          ymin = get(ymean),
          color = get(MEDIA), 
          tooltip = format(get(ymean),
                           scientific = F,
                           big.mark = ",",
                           digits = 0)),
      size=0.5, 
      linetype = "longdash",
      inherit.aes = F,
      width = 1) +
    geom_errorbar_interactive(
      data = promedios[TIPO == "REFERENTE"],
      aes(
        P,
        ymax = get(ymax),
        ymin = get(ymax),
        color = get(MAX),
        tooltip = format(get(ymax),
                         scientific = F,
                         big.mark = ",",
                         digits = 0)),
      size=0.5,
      linetype = "longdash",
      inherit.aes = F,
      width = 1) +
    geom_errorbar_interactive(
      data = promedios[TIPO == "REFERENTE"],
      aes(
        P,
        ymax = get(ymin),
        ymin = get(ymin),
        color = get(MIN),
        tooltip = format(get(ymin),
                         scientific = F,
                         big.mark = ",",
                         digits = 0)),
      size=0.5,
      linetype = "longdash",
      inherit.aes = F,
      width = 1) +
    scale_y_continuous(labels = scales::comma, name = valorcosto) +
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
