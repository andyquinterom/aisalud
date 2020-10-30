filtros_ui <- function(id, n_char, n_num) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 6,
        tags$h3("Variables")
      ),
      column(
        width = 6,
        tags$h3("Valores")
      )
    ),
    filtro_discreto_ui_insert(ns = ns, n = n_char),
    filtro_numerico_ui_insert(ns = ns, n = n_num)
  )
}

filtros_server <- function(input, output, session, datos, n) {

  
}


# Funciones ----------------

filtro_discreto_ui_fila <- function(ns, position = 1) {
  fluidRow(
    column(
      width = 6,
      pickerInput(
        inputId = ns(paste("filtro_char_columna", position, sep = "_")),
        label = NULL,
        choices = "NA",
        selected = "NA",
        multiple = TRUE
      )),
    column(
      width = 6,
      pickerInput(
        inputId = ns(paste("filtro_char_valor", position, sep = "_")),
        label = NULL,
        choices = "NA",
        selected = "NA",
        multiple = TRUE
      ))
  )
}

filtro_discreto_ui_insert <- function(ns, n) {
  
  filtros_filas <- list()
  
  for (i in 1:n) {
    filtros_filas[[i]] <- filtro_discreto_ui_fila(
      ns = ns, 
      position = i)
  }
  
  return(filtros_filas)
  
}

filtro_numerico_ui_fila <- function(ns, position = 1) {
  fluidRow(
    column(
      width = 6,
      pickerInput(
        inputId = ns(paste("filtro_num_columna", position, sep = "_")),
        choices = c("NA"),
        width = "100%")),
    column(
      width = 3,
      numericInput(
        inputId = ns(paste("filtro_num_min", position, sep = "_")),
        label = NULL,
        value = 0,
        min = 0,
        max = 0, 
        width = "100%")),
    column(
      width = 3,
      numericInput(
        inputId = ns(paste("filtro_num_max", position, sep = "_")),
        label = NULL, 
        value = 0,
        min = 0,
        max = 0,
        width = "100%")))
}

filtro_numerico_ui_insert <- function(ns, n) {
  
  filtros_filas <- list()
  
  for (i in 1:n) {
    filtros_filas[[i]] <- filtro_numerico_ui_fila(
      ns = ns, 
      position = i)
  }
  
  return(filtros_filas)
  
}