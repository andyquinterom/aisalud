pie_chart <- function(paquetes, columna, valor_costo, nombre_legend = "") {
  piechart <- ggplot(
    paquetes[, list(TOTAL = sum(get(valor_costo), na.rm = TRUE)), by = columna], 
    aes(x="", 
        y=TOTAL, 
        fill=str_wrap(get(columna), 20),  
        tooltip = paste(get(columna),formatAsCurrency(TOTAL), sep = " - "),
        data_id = get(columna)))+
    geom_bar_interactive(width = 1, stat = "identity", colour="black") +
    coord_polar("y", start=0) +
    scale_y_continuous(labels = scales::comma, name = valor_costo) +
    scale_x_discrete(name = "") +
    labs(fill = columna) +
    scale_fill_discrete(name = nombre_legend) + 
    theme_minimal()
  
  return(
    girafe(ggobj = piechart,
           width_svg = 10, 
           height_svg = 8, 
           options = list(opts_selection(type = "none", only_shiny = TRUE)))
  )
}