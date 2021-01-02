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

filtros_server <- function(id, opciones) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      n_num = 3
      n_char = 5
      
      observeEvent(opciones$colnames, {
        lapply(
          X = 1:n_char,
          FUN = function(x) {
            updatePickerInput(
              session = session,
              inputId = paste("filtro_char_columna", x, sep = "_"),
              choices = c("Ninguno", opciones$colnames)
            )
          }
        )
        lapply(
          X = 1:n_num,
          FUN = function(x) {
            updatePickerInput(
              session = session,
              inputId = paste("filtro_num_columna", x, sep = "_"),
              choices = c("Ninguno", opciones$colnames_num)
            )
          }
        )
      })
      
      lapply(
        X = 1:n_char,
        FUN = function(i) {
          observeEvent(input[[paste0("filtro_char_columna_", i)]], {
            if (opciones$tabla_nombre != "Ninguno") {
              updateSelectizeInput(
                session = session,
                inputId = paste0("filtro_char_valor_", i),
                server = TRUE,
                selected = "Ninguno",
                choices = {
                  columna_seleccionada <- input[[paste0("filtro_char_columna_", i)]]
                  if (columna_seleccionada != "Ninguno") {
                    opciones$tabla_original %>%
                      select(!!as.name(columna_seleccionada)) %>%
                      distinct() %>%
                      collect() %>%
                      unlist() %>%
                      unname()
                  } else {
                    "Ninguno"
                  }
                }
              )
            }
          })
        }
      )
      
      lapply(
        X = 1:n_num,
        FUN = function(i) {
          observeEvent(input[[paste0("filtro_num_columna_", i)]], {
            if (opciones$tabla_nombre != "Ninguno") {
              if (opciones$tabla_nombre != "Ninguno") {
                updateNumericInput(
                  session = session,
                  inputId = paste0("filtro_num_min_", i),
                  value = {
                    columna_seleccionada <- input[[paste0("filtro_num_columna_", i)]]
                    if (columna_seleccionada != "Ninguno") {
                      opciones$tabla_original %>%
                        select(!!as.name(columna_seleccionada)) %>%
                        transmute(min = min(!!as.name(columna_seleccionada),
                                            na.rm = TRUE)) %>%
                        distinct() %>%
                        collect() %>%
                        unlist() %>%
                        unname()
                    } else {
                      0
                    }
                  }
                )
                updateNumericInput(
                  session = session,
                  inputId = paste0("filtro_num_max_", i),
                  value = {
                    columna_seleccionada <- input[[paste0("filtro_num_columna_", i)]]
                    if (columna_seleccionada != "Ninguno") {
                      opciones$tabla_original %>%
                        select(!!as.name(columna_seleccionada)) %>%
                        transmute(max = max(!!as.name(columna_seleccionada),
                                            na.rm = TRUE)) %>%
                        distinct() %>%
                        collect() %>%
                        unlist() %>%
                        unname()
                    } else {
                      0
                    }
                  }
                )
              }
            }
          })
        }
      )
      
      observeEvent(input$aplicar_filtros, {
        
        opciones$tabla <- opciones$tabla_original
        
        inputs_filtros_char <- c()
        
        inputs_filtros_char <- unlist(
          lapply(
            X = 1:n_char,
            FUN = function(i) {
              return(input[[paste0("filtro_char_columna_", i)]] != "Ninguno")
            }
          )
        )
        
        lapply(
          X = (1:n_char)[inputs_filtros_char],
          FUN = function(i) {
            valores_filtro <- input[[paste0("filtro_char_valor_", i)]]
            if (input[[paste0("filtro_char_incluir_", i)]]) {
              opciones$tabla <<- opciones$tabla %>%
                filter(!!as.name(input[[paste0("filtro_char_columna_", i)]]) %in%
                         valores_filtro)
            } else {
              opciones$tabla <<- opciones$tabla %>%
                filter(!(!!as.name(input[[paste0("filtro_char_columna_", i)]]) %in%
                           valores_filtro))
            }
          }
        )
        
        inputs_filtros_num <- unlist(
          lapply(
            X = 1:n_char,
            FUN = function(i) {
              return(input[[paste0("filtro_num_columna_", i)]] != "Ninguno")
            }
          )
        )
        
        lapply(
          X = (1:n_num)[inputs_filtros_num],
          FUN = function(i) {
            minimo <- input[[paste0("filtro_num_min_", i)]]
            maximo <- input[[paste0("filtro_num_max_", i)]]
            
            if (!is.na(minimo) && !is.na(maximo)) {
              columna <- input[[paste0("filtro_num_columna_", i)]]
              opciones$tabla <<- opciones$tabla %>%
                filter(!!as.name(columna) >= minimo) %>%
                filter(!!as.name(columna) <= maximo)
            } else {
              showNotification(
                "Uno de los filtros numéricos esta vacio.",
                type = "error"
              )
            }
            
          }
        )
        
        showNotification(
          ui = "Filtros aplicados.",
          duration = 4
        )
      })
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