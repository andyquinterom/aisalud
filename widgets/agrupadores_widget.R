agrupadores_widget <- function(id = NULL, separadores = FALSE, ...) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId = ns("agrupador"),
      label = "Agrupador principal:",
      choices = NULL,
      multiple = FALSE),
    if (separadores) {
      selectizeInput(
        inputId = ns("separadores"),
        label = "Separadores:",
        choices = NULL,
        multiple = TRUE)
    },
    ...,
    checkboxInput(
      inputId = ns("episodios"),
      label = "Agrupador por episodios",
      value = FALSE),
    uiOutput(outputId = ns("episodios_col_rel")),
    tags$div(
      style = "overflow-y: scroll; max-height: 300px;",
      uiOutput(outputId = ns("episodios_jerarquia")))
    )
}
