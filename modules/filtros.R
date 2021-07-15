#
# Analítica Integrada Salud
#
# Derechos de autor 2021 por MD&CO Consulting Group (NIT 901.119.781-5)
# Copyright (C) 2021 by MD&CO Consulting Group
#
# Este programa es software libre: puede redistribuirlo o modificarlo bajo
# los términos de la licencia Affero General Public License tal cual
# publicada por la Free Software Foundation, sea la versión 3 de la licencia
# o cualquier versión posterior. Este programa se distribuye SIN GARANTÍA
# EXPERSA O IMPLÍCITA, INCLUIDAS LAS DE NO INFRACCIÓN, COMERCIABILIDAD O
# APTITUD PARA UN PROPÓSITO PARTICULAR. Referir a la
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) para más detalles.
#

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
          filtro_discreto_ui_insert(ns = ns, n = 6),
          filtros_pacientes_ui_fila(ns)
        ),
      filtro_numerico_ui_insert(ns = ns, n = 3),
      actionButton(ns("aplicar_filtros"), "Aplicar")
    )
  )
}

filtros_server <- function(id, opciones, cache) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      aplicar_count <- counter()
      # cantidad de filtros numericos
      n_num <- 3
      # cantidad de filtros de variables caracteres
      n_char <- 6

      observeEvent(opciones$colnames, {
        lapply(
          X = 1:n_char,
          FUN = function(x) {
            updateSelectizeInput(
              session = session,
              inputId = paste("filtro_char_columna", x, sep = "_"),
              choices = c("Ninguno", opciones$colnames)
            )
          }
        )
        lapply(
          X = 1:n_num,
          FUN = function(x) {
            updateSelectizeInput(
              session = session,
              inputId = paste("filtro_num_columna", x, sep = "_"),
              choices = c("Ninguno", opciones$colnames_num)
            )
          }
        )
      })

      observe({
        pacientes_excluir <- unique(opciones$pacientes_excluir)
        updateSelectizeInput(
          session = session,
          inputId = "filtros_paciente_valor",
          choices = pacientes_excluir,
          selected = pacientes_excluir,
          server = TRUE
        )
      }) %>%
      bindEvent(opciones$pacientes_excluir_exe)

      observeEvent(input$filtros_paciente_valor,{
        if (!all(opciones$pacientes_excluir %in%
                 input$filtros_paciente_valor)) {
          opciones$pacientes_excluir <- input$filtros_paciente_valor
        }
      })

      observeEvent(input$filtro_paciente_vaciar, {
        opciones$pacientes_excluir <- NULL
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
            if (opciones$datos_cargados) {
              updateSelectizeInput(
                session = session,
                inputId = paste0("filtro_char_valor_", i),
                server = TRUE,
                selected = "Ninguno",
                choices = {
                  columna_seleccionada <-
                    input[[paste0("filtro_char_columna_", i)]]
                  if (columna_seleccionada %notin% c("Ninguno", "")) {
                    cache_call(
                      fn = pull_distinct,
                      cache = cache,
                      cache_params = list(col = columna_seleccionada),
                      non_cache_params = list(data = opciones$tabla_original),
                      cache_depends = opciones$tabla_nombre,
                      prefix = "items-filtros"
                    )
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
            if (opciones$datos_cargados) {
              updateNumericInput(
                session = session,
                inputId = paste0("filtro_num_min_", i),
                value = {
                  columna_seleccionada <-
                    input[[paste0("filtro_num_columna_", i)]]
                  if (columna_seleccionada %notin% c("Ninguno", "")) {
                    opciones$tabla_original %>%
                      select(!!as.name(columna_seleccionada)) %>%
                      summarise(min = min(!!as.name(columna_seleccionada),
                                          na.rm = TRUE)) %>%
                      pull()
                  } else {
                    0
                  }
                }
              )
              updateNumericInput(
                session = session,
                inputId = paste0("filtro_num_max_", i),
                value = {
                  columna_seleccionada <-
                    input[[paste0("filtro_num_columna_", i)]]
                  if (columna_seleccionada %notin% c("Ninguno", "")) {
                    opciones$tabla_original %>%
                      select(!!as.name(columna_seleccionada)) %>%
                      summarise(max = max(!!as.name(columna_seleccionada),
                                          na.rm = TRUE)) %>%
                      pull()
                  } else {
                    0
                  }
                }
              )
            }
          })
        }
      )

      # Se observa el botón aplicar_filtros y la opcion global
      aplicar_filtros <- reactive({
        list(input$aplicar_filtros, opciones$aplicar_filtros)
      })

      observeEvent(aplicar_filtros(), {
        opciones$tabla <- opciones$tabla_original
        # Variable global que cambia cada vez que se aplican los filtros
        opciones$filtros_apply <- aplicar_count()
        inputs_filtros_char <- c()
        inputs_filtros_char <- unlist(
          lapply(
            X = 1:n_char,
            FUN = function(i) {
              return(input[[paste0("filtro_char_columna_", i)]] %notin%
                       c("Ninguno", ""))
            }
          )
        )

        n_filtros_char <- sum(inputs_filtros_char)

        if (!is.null(input$filtros_paciente_valor)) {
          n_filtros_char <- n_filtros_char + 1
          valores_filtro <- input$filtros_paciente_valor
          if (input$filtro_paciente_incluir) {
            opciones$tabla <<- opciones$tabla %>%
              filter(nro_identificacion %in% valores_filtro)
          } else {
            opciones$tabla <<- opciones$tabla %>%
              filter(!(nro_identificacion %in% valores_filtro))
          }
        }

        lapply(
          X = (1:n_char)[inputs_filtros_char],
          FUN = function(i) {
            valores_filtro <- input[[paste0("filtro_char_valor_", i)]]
            columna <- input[[paste0("filtro_char_columna_", i)]]
            if (input[[paste0("filtro_char_incluir_", i)]]) {
              opciones$tabla <<- opciones$tabla %>%
                filter(!!as.name(columna) %in% valores_filtro)
            } else {
              opciones$tabla <<- opciones$tabla %>%
                filter(!(!!as.name(columna) %in% valores_filtro))
            }
          }
        )

        inputs_filtros_num <- unlist(
          lapply(
            X = 1:n_char,
            FUN = function(i) {
              return(input[[paste0("filtro_num_columna_", i)]] %notin%
                       c("Ninguno", ""))
            }
          )
        )

        n_filtros_num <- sum(inputs_filtros_num)

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

        n_filtros_total <- n_filtros_char + n_filtros_num

        showNotification(
          ui = paste("Se aplicaron", n_filtros_total, "filtros."),
          duration = 4
        )
      })
  })
}


# Funciones ----------------

addPreserveSearch <- function(x) {
  preserve_search <- htmlDependency(
    "preserve_search", "1.0", "www",
    script = "preserve_search.js")
  attachDependencies(x, c(htmlDependencies(x), list(preserve_search)))
}

filtro_discreto_ui_fila <- function(ns, position = 1) {
  fluidRow(
    column(
      width = 5,
      selectizeInput(
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
        width = "100%",
        value = TRUE
      )
    ),
    column(
      width = 5,
      addPreserveSearch(selectizeInput(
        inputId = ns(paste("filtro_char_valor", position, sep = "_")),
        label = NULL,
        choices = "Ninguno",
        selected = "Ninguno",
        options = list(plugins = list('preserve_search')),
        multiple = TRUE
      )))
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
      selectizeInput(
        label = NULL,
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
