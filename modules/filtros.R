filtros_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "filtros",
      fluidRow(
        column(
          width = 5,
          tags$h3("Variables")
        ),
        column(
          width = 2
        ),
        column(
          width = 5,
          tags$h3("Valores")
        )
      ),
      tags$div(
        class = "filtros_char",
          filtro_discreto_ui_insert(ns = ns, n = 5)
        ),
      filtro_numerico_ui_insert(ns = ns, n = 3),
      actionButton(ns("aplicar_filtros"), "Aplicar")
    )
  )
}

filtros_server <- function(input, output, session, datos) {

  n_num = 2
  n_char = 5
  
  observeEvent(datos$colnames, {
    lapply(
      X = 1:n_char,
      FUN = function(x) {
        updatePickerInput(
          session = session,
          inputId = paste("filtro_char_columna", x, sep = "_"),
          choices = c("NA", datos$colnames)
        )
      }
    )
    lapply(
      X = 1:n_num,
      FUN = function(x) {
        updatePickerInput(
          session = session,
          inputId = paste("filtro_num_columna", x, sep = "_"),
          choices = c("NA", datos$colnames_num)
        )
      }
    )
    print(datos$colnames_num)
  })
  
  lapply(
    X = 1:n_char,
    FUN = function(i) {
      observeEvent(input[[paste0("filtro_char_columna_", i)]], {
        updateSelectizeInput(
          session = session,
          inputId = paste0("filtro_char_valor_", i),
          server = TRUE,
          choices = datos$valores_unicos[[
            input[[paste0("filtro_char_columna_", i)]]]]
        )
      })
    }
  )
  
  observeEvent(input$aplicar_filtros, {
    inputs_filtros_char <- c()
    for (i in 1:n_char) {
      if (input[[paste0("filtro_char_columna_", i)]] != "NA") {
        inputs_filtros_char <- c(inputs_filtros_char, TRUE)
      } else {
        inputs_filtros_char <- c(inputs_filtros_char, FALSE)
      }
    }
    
    inputs_filtros_num <- c()
    for (i in 1:n_num) {
      if (input[[paste0("filtro_num_columna_", i)]] != "NA") {
        inputs_filtros_num <- c(inputs_filtros_num, TRUE)
      } else {
        inputs_filtros_num <- c(inputs_filtros_num, FALSE)
      }
    }

    datos$data_table <- datos$data_original
    for (i in (1:n_char)[inputs_filtros_char]) {
      if (input[[paste0("filtro_char_incluir_", i)]]) {
        datos$data_table <- datos$data_table[
          get(input[[paste0("filtro_char_columna_", i)]]) %in%
            input[[paste0("filtro_char_valor_", i)]]
        ]
      } else {
        datos$data_table <- datos$data_table[
          get(input[[paste0("filtro_char_columna_", i)]]) %notin%
            input[[paste0("filtro_char_valor_", i)]]
        ]
      }
    }
  })
  
}


# Funciones ----------------

if_na_return <- function(x, y) {
  return(
    ifelse(
      test = is.na(x),
      yes = y,
      no = x
    )
  )
}

filtro_discreto_ui_fila <- function(ns, position = 1) {
  fluidRow(
    column(
      width = 5,
      pickerInput(
        inputId = ns(paste("filtro_char_columna", position, sep = "_")),
        label = NULL,
        choices = "NA",
        selected = "NA",
        multiple = FALSE
      )),
    column(
      width = 2,
      shinyWidgets::switchInput(
        inputId = ns(paste("filtro_char_incluir", position, sep = "_")),
        onLabel = "Incluir",
        offLabel = "Excluir",
        value = TRUE
      )
    ),
    column(
      width = 5,
      selectizeInput(
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
      width = 5,
      pickerInput(
        inputId = ns(paste("filtro_num_columna", position, sep = "_")),
        choices = c("NA"),
        width = "100%")),
    column(
      width = 2
    ),
    column(
      width = 5,
      fluidRow(
        column(
          width = 6,
          numericInput(
            inputId = ns(paste("filtro_num_min", position, sep = "_")),
            label = NULL,
            value = 0,
            min = 0,
            max = 0, 
            width = "100%")),
        column(
          width = 6,
          numericInput(
            inputId = ns(paste("filtro_num_max", position, sep = "_")),
            label = NULL, 
            value = 0,
            min = 0,
            max = 0,
            width = "100%"))
      )
    )
    )
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