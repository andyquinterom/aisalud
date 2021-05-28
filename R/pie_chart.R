pie_chart <- function(paquetes, columna, valor_costo, nombre_legend = "") {
  data <- copy(paquetes)[, list(
    TOTAL = sum(get(valor_costo), na.rm = TRUE)), by = columna]

  plot_pie_chart <- plot_ly(
    data = data,
    labels = ~get(columna),
    values = ~TOTAL,
    type = "pie"
  )

  plot_pie_chart <- plot_pie_chart %>%
    config(locale = "es") %>%
    layout(
      xaxis = list(
        showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(
        showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      legend = list(
        orientation = "h", x = 0, y = 0)) %>%
    style(legendgroup = NULL)

  return(
    plot_pie_chart
  )
}
