comparar_cajas_jerarquia <- function(
  ns,
  items_nivel_1 = NULL,
  items_nivel_2 = NULL,
  items_nivel_3 = NULL,
  items_nivel_4 = NULL) {
  return(
    tags$div(
      class = "comparar_jerarquia_row",
      box(
        width = 3,
        orderInput(
          inputId = ns("comparar_jerarquia_nivel_1"),
          label = actionLink(ns("seleccionar_episodio"), label = "Episodio"),
          items = items_nivel_1,
          width = "100%",
          height = "100%",
          connect = c(
            ns("comparar_jerarquia_nivel_2"),
            ns("comparar_jerarquia_nivel_3"),
            ns("comparar_jerarquia_nivel_4")))
      ),
      box(
        width = 3,
        orderInput(
          inputId = ns("comparar_jerarquia_nivel_2"),
          label = actionLink(ns("seleccionar_factura"), label = "Factura"),
          items = items_nivel_2,
          width = "100%",
          height = "100%",
          connect = c(
            ns("comparar_jerarquia_nivel_1"),
            ns("comparar_jerarquia_nivel_3"),
            ns("comparar_jerarquia_nivel_4")))
      ),
      box(
        width = 3,
        orderInput(
          inputId = ns("comparar_jerarquia_nivel_3"),
          label = actionLink(ns("seleccionar_paciente"), label = "Paciente"),
          items = items_nivel_3,
          width = "100%",
          height = "100%",
          connect = c(
            ns("comparar_jerarquia_nivel_1"),
            ns("comparar_jerarquia_nivel_2"),
            ns("comparar_jerarquia_nivel_4")))
      ),
      box(
        width = 3,
        orderInput(
          inputId = ns("comparar_jerarquia_nivel_4"),
          label = actionLink(ns("seleccionar_prestacion"),
            label = "Prestación"),
          items = items_nivel_4,
          width = "100%",
          height = "100%",
          connect = c(
            ns("comparar_jerarquia_nivel_1"),
            ns("comparar_jerarquia_nivel_2"),
            ns("comparar_jerarquia_nivel_3")))
      )
    )
  )
}
