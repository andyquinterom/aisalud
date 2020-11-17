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
          filtro_discreto_ui_insert(ns = ns, n = 5),
          filtros_pacientes_ui_fila(ns)
        ),
      filtro_numerico_ui_insert(ns = ns, n = 3),
      actionButton(ns("aplicar_filtros"), "Aplicar")
    )
  )
}

filtros_server <- function(input, output, session, datos) {

  n_num = 3
  n_char = 5
  
  observeEvent(datos$colnames, {
    lapply(
      X = 1:n_char,
      FUN = function(x) {
        updatePickerInput(
          session = session,
          inputId = paste("filtro_char_columna", x, sep = "_"),
          choices = c("Ninguno", datos$colnames)
        )
      }
    )
    lapply(
      X = 1:n_num,
      FUN = function(x) {
        updatePickerInput(
          session = session,
          inputId = paste("filtro_num_columna", x, sep = "_"),
          choices = c("Ninguno", datos$colnames_num)
        )
      }
    )
  })
  
  observeEvent(datos$pacientes_excluir_exe, {
    if (is.null(input$filtros_paciente_valor) && 
        length(datos$pacientes_excluir) > 1) {
      pacientes_excluir <- unique(datos$pacientes_excluir[-1])
    } else {
      pacientes_excluir <- unique(datos$pacientes_excluir)
    }
    updateSelectizeInput(
      session = session,
      inputId = "filtros_paciente_valor",
      choices = pacientes_excluir,
      selected = pacientes_excluir,
      server = TRUE
    )
  })
  
  observeEvent(input$filtros_paciente_valor,{
    if (!all(datos$pacientes_excluir %in% input$filtros_paciente_valor)) {
      datos$pacientes_excluir <- input$filtros_paciente_valor
    }
  })
  
  observeEvent(input$filtro_paciente_vaciar, {
    datos$pacientes_excluir <- datos$pacientes_excluir[1]
    updateSelectizeInput(
      session = session,
      inputId = "filtros_paciente_valor",
      choices = NULL,
      selected = NULL,
      server = TRUE
    )
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
  
  lapply(
    X = 1:n_num,
    FUN = function(i) {
      observeEvent(input[[paste0("filtro_num_columna_", i)]], {
        if (input[[paste0("filtro_num_columna_", i)]] != "Ninguno") {
          updateNumericInput(
            session = session,
            inputId = paste0("filtro_num_min_", i),
            value = min(
              datos$data_original[[
                input[[paste0("filtro_num_columna_", i)]]
                ]],
              na.rm = TRUE)
          )
          updateNumericInput(
            session = session,
            inputId = paste0("filtro_num_max_", i),
            value = max(
              datos$data_original[[
                input[[paste0("filtro_num_columna_", i)]]
              ]],
              na.rm = TRUE)
          )
        }
      })
    }
  )
  
  lapply(
    X = 1:n_num,
    FUN = function(i) {
      observeEvent(input[[paste0("filtro_num_min_", i)]], {
        if (is.na(input[[paste0("filtro_num_min_", i)]]) &
            input[[paste0("filtro_num_columna_", i)]] != "Ninguno") {
          updateNumericInput(
            session = session,
            inputId = paste0("filtro_num_min_", i),
            value = min(
              datos$data_original[[
                input[[paste0("filtro_num_columna_", i)]]
              ]],
              na.rm = TRUE)
          )
        }
      })
      observeEvent(input[[paste0("filtro_num_max_", i)]], {
        if (is.na(input[[paste0("filtro_num_max_", i)]]) &
            input[[paste0("filtro_num_columna_", i)]] != "Ninguno") {
          updateNumericInput(
            session = session,
            inputId = paste0("filtro_num_max_", i),
            value = max(
              datos$data_original[[
                input[[paste0("filtro_num_columna_", i)]]
              ]],
              na.rm = TRUE)
          )
        }
      })
    }
  )
  
  observeEvent(input$aplicar_filtros, {
    inputs_filtros_char <- c()
    
    inputs_filtros_char <- unlist(
      lapply(
        X = 1:n_char,
        FUN = function(i) {
          return(input[[paste0("filtro_char_columna_", i)]] != "Ninguno")
        }
      )
    )
    
    inputs_filtros_char_arguments <- paste(unlist(
      lapply(
        X = (1:n_char)[inputs_filtros_char],
        FUN = function(i) {
          return(
            paste0(
              "[get(",
              paste0("input$filtro_char_columna_", i),
              ifelse(
                test = input[[paste0("filtro_char_incluir_", i)]],
                yes = ") %in% ",
                no = ") %notin% "
              ),
              paste0("input$filtro_char_valor_", i),
              "]"
            )
          )
        }
      )
    ),
    collapse = "")
    
    inputs_filtros_num <- unlist(
      lapply(
        X = 1:n_char,
        FUN = function(i) {
          return(input[[paste0("filtro_num_columna_", i)]] != "Ninguno")
        }
      )
    )
    
    inputs_filtros_num_arguments <- paste(unlist(
      lapply(
        X = (1:n_num)[inputs_filtros_num],
        FUN = function(i) {
          return(
            paste0(
              "[get(",
              paste0("input$filtro_num_columna_", i),
              ") >= ",
              paste0("input$filtro_num_min_", i),
              " & get(",
              paste0("input$filtro_num_columna_", i),
              ") <= ",
              paste0("input$filtro_num_max_", i),
              "]"
            )
          )
        }
      )
    ),
    collapse = "")
    
    inputs_filtros_pacientes_arguments <- paste(unlist(
      if (!is.null(input$filtros_paciente_valor)) {
        paste0(
          "[NRO_IDENTIFICACION",
          ifelse(
            test = input[["filtro_paciente_incluir"]],
            yes = " %in% ",
            no = " %notin% "
          ),
          "input$filtros_paciente_valor]"
        )
      } else {
        "[]"
      }
      ),
      collapse = "")
    
    filtros_parse <- paste0(
      "datos$data_table",
      inputs_filtros_char_arguments,
      inputs_filtros_num_arguments,
      inputs_filtros_pacientes_arguments
    )


    datos$data_table <- datos$data_original
    datos$data_table <- eval(parse(
      text = filtros_parse
    ))
    
    showNotification(
      ui = "Filtros aplicados.",
      duration = 4
    )
    
  })
  
}


# Funciones ----------------

filtro_discreto_ui_fila <- function(ns, position = 1) {
  fluidRow(
    column(
      width = 5,
      pickerInput(
        inputId = ns(paste("filtro_char_columna", position, sep = "_")),
        label = NULL,
        choices = "Ninguno",
        selected = "Ninguno",
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
        choices = "Ninguno",
        selected = "Ninguno",
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
        choices = c("Ninguno"),
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
  
  filtros_filas[[1]] <-  fluidRow(
    column(width = 7),
    column(
      width = 5,
      fluidRow(
        column(
          width = 6,
          tags$p("Mínimo", style = "font-size: 100%;")),
        column(
          width = 6,
          tags$p("Máximo", style = "font-size: 100%;"))
      )
    )
  )
    
  
  for (i in 1:n + 1) {
    filtros_filas[[i]] <- filtro_numerico_ui_fila(
      ns = ns, 
      position = i - 1)
  }
  
  return(filtros_filas)
  
}

filtros_pacientes_ui_fila <- function(ns) {
  fluidRow(
    column(
      width = 3,
      pickerInput(
        inputId = ns("filtro_paciente"),
        label = NULL,
        choices = "Pacientes",
        selected = "Pacientes",
        multiple = FALSE
      )),
    column(
      width = 2,
      actionButton(
        inputId = ns("filtro_paciente_vaciar"),
        label = "Vaciar",
        width = "100%"
      )
    ),
    column(
      width = 2,
      shinyWidgets::switchInput(
        inputId = ns("filtro_paciente_incluir"),
        onLabel = "Incluir",
        offLabel = "Excluir",
        value = FALSE
      )
    ),
    column(
      width = 5,
      selectizeInput(
        inputId = ns("filtros_paciente_valor"),
        label = NULL,
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      )
    )
  )
}