seguimiento_notas_dashboard_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        valueBoxOutput(outputId = ns("entidad"), width = 12))),
    fluidRow(
      column(
        width = 12,
        valueBoxOutput(
          outputId = ns("valor_mes"), width = 4),
        valueBoxOutput(
          outputId = ns("poblacion"),
          width = 4),
        valueBoxOutput(
          outputId = ns("departamento"),
          width = 4))),
    fluidRow(
      column(
        width = 12,
        box(
          width = 12,
          pickerInput(
            inputId = ns("board_select"), 
            width = "100%",
            choices = "NA",
            label = "Nota técnica")),
        box(
          width = 12,
          title = "Nota técnica:",
          fluidRow(
            column(
              width = 5,
              DT::dataTableOutput(outputId = ns("board_datos"))),
            column(
              width = 7,
              ggiraph::ggiraphOutput(
                outputId = ns("plot_agrupadores"),
                width = "100%",
                height = "100%")))),
        box(
          title = "Inclusiones:",
          width = 6,
          DT::dataTableOutput(outputId = ns("inclusiones"))),
        box(
          title = "Exclusiones:",
          width = 6,
          DT::dataTableOutput(outputId = ns("exclusiones")))))
  )
  
}