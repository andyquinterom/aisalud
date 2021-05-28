jerarquia <- function(
  ns,
  items_nivel_1 = NULL,
  items_nivel_2 = NULL,
  items_nivel_3 = NULL,
  items_nivel_4 = NULL) {
  return(
    tagList(
      orderInput(
        inputId = ns("episodios_jerarquia_nivel_1"),
        label = actionLink(ns("seleccionar_episodio"), label = "Episodio"),
        items = items_nivel_1,
        width = "100%",
        height = "100%",
        connect = c(
          ns("episodios_jerarquia_nivel_2"),
          ns("episodios_jerarquia_nivel_3"),
          ns("episodios_jerarquia_nivel_4"))),
      orderInput(
        inputId = ns("episodios_jerarquia_nivel_2"),
        label = actionLink(ns("seleccionar_factura"), label = "Factura"),
        items = items_nivel_2,
        width = "100%",
        height = "100%",
        connect = c(
          ns("episodios_jerarquia_nivel_1"),
          ns("episodios_jerarquia_nivel_3"),
          ns("episodios_jerarquia_nivel_4"))),
      orderInput(
        inputId = ns("episodios_jerarquia_nivel_3"),
        label = actionLink(ns("seleccionar_paciente"), label = "Paciente"),
        items = items_nivel_3,
        width = "100%",
        height = "100%",
        connect = c(
          ns("episodios_jerarquia_nivel_1"),
          ns("episodios_jerarquia_nivel_2"),
          ns("episodios_jerarquia_nivel_4"))),
      orderInput(
        inputId = ns("episodios_jerarquia_nivel_4"),
        label = actionLink(ns("seleccionar_prestacion"), label = "Prestación"),
        items = items_nivel_4,
        width = "100%",
        height = "100%",
        connect = c(
          ns("episodios_jerarquia_nivel_1"),
          ns("episodios_jerarquia_nivel_2"),
          ns("episodios_jerarquia_nivel_3")))
    )
  )
}
