# El módulo de cargar_datos tiene como proposito alimentar los datos de
# toda la aplicación. La varaible principal en la cual se almacenan los datos
# es la opciones. Esta se declara en el server.R y se muta dentro de este
# módulo. Si algun otro modulo desea cambiar algun dato global se puede hacer
# un request a través de la misma variable opciones. Sin embargo, los datos
# no deben ser mutados.

cargar_datos_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        style = "min-height: 692px;",
        width = 4,
        dateRangeInput(
          inputId = ns("fecha_rango"),
          label = "Fechas:",
          min = "1970-01-01",
          max = NULL,
          format = "dd/mm/yyyy",
          language = "es"),
        tags$style(HTML(".datepicker {z-index:99999 !important;}")),
        selectizeInput(
          width = "100%",
          inputId = ns("tabla"),
          label = "Seleccionar datos",
          choices = "Ninguno"
        )
      ),
      box(
        width = 8,
        style = "min-height: 650px;",
        tabsetPanel(
          tabPanel(
            title = "Resumen",
            plotlyOutput(
              outputId = ns("valor_con_tiempo"), height = "630px") %>%
              withSpinner()
          ),
          tabPanel(
            title = "Perfiles",
            jsoneditOutput(
              outputId = ns("perfil_editor"),
              height = "630px",
              width = "100%"
            )
          ),
          tabPanel(
            title = "Notas técnicas",
            jsoneditOutput(
              outputId = ns("notas_tecnicas_editor"),
              height = "630px",
              width = "100%"
            )
          )
        )
      )
    )
  )
}

cargar_datos_server <- function(id, opciones, conn) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- NS(id)
      aplicar_filtros <- counter()

      # La varaible prepara opciones guarda cambios dentro del módulo.
      prepara_opciones <- reactiveValues()

      # Se obtienen las tablas dentro de la base de datos del usuario
      # y se actualiza el selectizeInput.
      observe({
        tables_query <- dbListTables(conn = conn) %>%
          unlist() %>%
          unname()
        # AIS solo puede leer tablas que tengan el prefix "ais_"
        tables_ais <- tables_query[str_starts(tables_query, "ais_")] %>%
          stringr::str_replace(pattern = "ais_", "")
        # Si no exsiten tablas validas
        if (identical(character(0), tables_ais)) tables_ais <- "Ninguno"
        updateSelectizeInput(
          session = session,
          inputId = "tabla",
          choices = unique(c("Ninguno", tables_ais))
        )
      })

      # Se observa el input del usuario al seleccionar una tabla
      observeEvent(input$tabla, {
        tabla <- paste0("ais_", input$tabla)
        # El input no puede estar vacio ni puede ser "Ninguno"
        if (input$tabla %notin% c("Ninguno", "")) {
          tryCatch(
            expr = {
              # Se obtienen columnas para el definir la columna de valor
              prepara_opciones$colnames <- dbListFields(
                conn,
                tabla)
            },
            error = function(e) {
              print(e)
              sendSweetAlert(
                session = session,
                title = "Error",
                text = "No se pudo leer la tabla seleccionada. Valide que aun
                exista esta tabla o contacte a un administrador.",
                type = "error"
              )
            }
          )
        }
      })

      # Se definen los rangos de fechas a utilizar
      observe({
        # Si la fecha es invalida, se utilizará la última selección
        opciones$fecha_rango[1] <- data.table::fifelse(
          test = !is.na(input$fecha_rango[1]),
          yes = input$fecha_rango[1], no = opciones$fecha_rango[1])
        opciones$fecha_rango[2] <- data.table::fifelse(
          test = !is.na(input$fecha_rango[2]),
          yes = input$fecha_rango[2], no = opciones$fecha_rango[2])
      })

      observe({
        if (input$tabla %notin% c("Ninguno", "")) {
          tryCatch(
            expr = {
              tabla <- paste0("ais_", input$tabla)
              # Lectura de la tabla seleccionada
              opciones$colnames <- dbListFields(
                conn,
                tabla)
              opciones$colnames_num <- dbListNumericFields(
                conn,
                tabla)
              fecha_min <- opciones$fecha_rango[1]
              fecha_max <- opciones$fecha_rango[2]
              opciones$tabla_nombre <- tabla
              # Se crea una imagen de la versión original de los datos
              # necesaria para los filtros
              opciones$tabla_original <- conn %>%
                tbl(tabla) %>%
                filter(fecha_prestacion >= fecha_min) %>%
                filter(fecha_prestacion <= fecha_max)
              opciones$tabla <- conn %>%
                tbl(tabla) %>%
                filter(fecha_prestacion >= fecha_min) %>%
                filter(fecha_prestacion <= fecha_max)
              opciones$aplicar_filtros <- aplicar_filtros()
            },
            error = function(e) {
              # Si la lectura de los datos da un error, se volverá  a leer
              # las tablas para evitar futuros errores
              tables_query <- dbListTables(conn = conn) %>%
                unlist() %>%
                unname()
              tables_ais <- tables_query[str_starts(tables_query, "ais_")] %>%
                stringr::str_replace(pattern = "ais_", "")
              if (identical(character(0), tables_ais)) tables_ais <- "Ninguno"
              updateSelectizeInput(
                session = session,
                inputId = "tabla",
                choices = unique(c("Ninguno", tables_ais))
              )
              print(e)
              sendSweetAlert(
                session = session,
                title = "Error",
                text = "No se pudo leer la tabla seleccionada. Valide que
                aun exista esta tabla o contacte a un administrador.",
                type = "error"
              )
            }
          )
        }
      }) %>%
      bindEvent(input$tabla, opciones$fecha_rango)

      # Resumen de valores en el tiempo seleccionado
      output$valor_con_tiempo <- renderPlotly({
        if (opciones$datos_cargados) {
          cache_id <- digest(
            object = list(
              "hist_val",
              opciones$tabla_query,
              opciones$valor_costo),
            algo = "xxhash32",
            seed = 1)
          check_cache <- cache_id %in% names(opciones$cache)
          if (!check_cache) {
            valor_costo <- opciones$valor_costo
            # Se genera gráfico de barras por mes y año
            opciones$cache[[cache_id]] <- opciones$tabla %>%
              mutate(
                # genera id de mes y año
                mes_temporal = year(fecha_prestacion) * 100 +
                    month(fecha_prestacion)) %>%
              group_by(mes_temporal) %>%
              summarise(suma = sum(
                as.numeric(!!as.name(valor_costo)), na.rm = TRUE)) %>%
              collect() %>%
              mutate(mes_label_temporal = paste(
                sep = " - ",
                substr(mes_temporal, 1, 4),
                mes_spanish(
                as.numeric(substr(mes_temporal, 5, 6))))) %>%
              arrange(mes_temporal) %>%
              plot_ly(
                x = ~mes_label_temporal,
                y = ~suma,
                type = "bar") %>%
              config(locale = "es") %>%
              layout(
                title = "Suma del valor a mes",
                xaxis = list(
                  title = "Mes",
                  categoryorder = "array",
                  categoryarray = ~mes_temporal),
                yaxis = list(
                  title = "Suma",
                  tickformat = ",.2f"))
          }
          opciones$cache[[cache_id]]
        }
      })

      observe({
        opciones$tabla_query <- opciones$tabla %>%
          sql_render()
      }) %>%
      bindEvent(opciones$tabla)

      # Validación de que los datos esten subido o seleccionados
      observe({
        opciones$datos_cargados <- FALSE
        if (input$tabla != "Ninguno" &&
            input$tabla != "") {
          # Si se seleccionan datos de la nube se
          # desactivarán los datos locales
          opciones$datos_cargados <- TRUE
        }
      })

      # Sección de Perfiles ---------------------------------------------------

      perfil_opciones <- reactiveValues()

      observe({
        # Se observan cambios a opciones$perfiles_updated
        # (cuando haya un cambio a los perfiles)
        opciones$perfil_updated
        # Pull los perfiles en formato json
        perfil_raw <- tbl(conn, "perfiles_usuario") %>%
          pull(perfiles)
        tryCatch(
          expr = {
            # Prettify y parsing del raw JSON
            opciones$perfil_raw <- perfil_raw %>%
              prettify()
            opciones$perfil_lista <- opciones$perfil_raw %>%
              parse_json(simplifyVector = TRUE)

          },
          error = function(e) {
            # Si se da un error, el raw json se cargará para que el usuario
            # lo pueda editar
            opciones$perfil_raw <- perfil_raw
            message(e)
            sendSweetAlert(
              session = session,
              title = "Error",
              text = e[1],
              type = "error"
            )
          }
        )
      })

      # Editor de JSON interactivo
      output$perfil_editor <- renderJsonedit({
        jsonedit(
          opciones$perfil_raw,
          language = "es",
          languages = "es",
          name = "Perfiles",
          enableTransform = FALSE,
          guardar = ns("perfil_editor_edit"),
          # Se lee JSONSchema para perfiles
          schema = read_json("json_schemas/perfiles.json"),
          # Se lee el template para perfiles
          templates = read_json("json_schemas/perfiles_template.json")
        )
      })

      # Guardar cambios a perfiles
      observeEvent(input$perfil_editor_edit, {
        showModal(
          modalDialog(
            # Se exige una contraseña en caso de encontrarse en las variables
            # de ambiente
            title = "Contraseña", size = "s", fade = TRUE, easyClose = TRUE,
            passwordInput(ns("perfil_actualizar_pw"), label = NULL),
            footer = shinyWidgets::actionGroupButtons(
              inputIds = ns(c("perfil_actualizar_close",
                "perfil_actualizar_conf")),
              labels = c("Cerrar", "Guardar"),
              fullwidth = TRUE
            )
          )
        )
      })

      observeEvent(input$perfil_actualizar_close, {
        removeModal()
      })

      observeEvent(input$perfil_actualizar_conf, {
        # Se valida que la contraseña ingresada por el usuario
        # sea identica a la que se encuentra en las variables de ambiente
        password_correct <- identical(
          input$perfil_actualizar_pw,
          Sys.getenv("CONF_PW"))
        if (password_correct) {
          perfil_nuevo <- data.frame("perfiles" = input$perfil_editor_edit$raw)
          removeModal()
          tryCatch(
            expr = {
              # Se valida el json con el JSONSchemad de perfiles
              validado <- json_validate(
                json = input$perfil_editor_edit$raw,
                schema = "json_schemas/perfiles.json"
              )
              # Si no se valida se generará error
              if (!validado) stop("El formato de los perfiles es invalido.")
              if (validado) {
                perfil_nuevo$perfiles %>%
                  prettify()
                dbWriteTable(
                  conn = conn,
                  name = "perfiles_usuario",
                  perfil_nuevo,
                  overwrite = TRUE
                )
                # Se envia mensaje de actualización de los perfiles
                opciones$perfil_updated <- FALSE
                opciones$perfil_updated <- TRUE
                showNotification(
                  ui = "El perfil se a guardado.",
                  type = "message"
                )
              }
            },
            error = function(e) {
              print(e)
              sendSweetAlert(
                session = session,
                title = "Error",
                text = e[1],
                type = "error"
              )
            }
          )
        }
        if (!password_correct) {
          # Mensaje de contraseña incorrecta
          showNotification(
            ui = "La contraseña no es correcta.",
            type = "error"
          )
        }
      })


      # Notas tecnicas editor
      # Este tipo de funcion se puede generalizar en un futuro
      # El código es practicamente igual al de perfiles
      observe({
        # Se observan cambios a opciones$notas_tecnicas_updated
        # (cuando hayan cambios a las notas técnicas)
        opciones$notas_tecnicas_updated
        # Pull las notas tecnicas en formato json
        opciones$notas_tecnicas_raw <-
          tbl(conn, "perfiles_notas_tecnicas_v2") %>%
            pull(notas_tecnicas)
        tryCatch(
          expr = {
            # parsing del raw JSON
            opciones$notas_tecnicas_lista <- opciones$notas_tecnicas_raw %>%
              parse_json(simplifyVector = TRUE)
            # Funcion para parse notas tecnicas
            opciones$notas_tecnicas <- opciones$notas_tecnicas_lista %>%
              parse_nt()
            # Funcion para convertir a un indice
            opciones$indice_todos <- parse_nt_indice(
              opciones$notas_tecnicas_lista,
              tabla_agrupadores = opciones$notas_tecnicas)
          },
          error = function(e) {
            print(e)
            sendSweetAlert(
              session = session,
              title = "Error",
              text = e[1],
              type = "error"
            )
          }
        )
      })

      # Output de JSON interactivo
      output$notas_tecnicas_editor <- renderJsonedit({
        jsonedit(
          opciones$notas_tecnicas_raw,
          language = "es",
          languages = "es",
          name = "Notas técnicas",
          enableTransform = FALSE,
          guardar = ns("notas_tecnicas_editor_edit"),
          # JSON Schema de notas tecnicas
          schema = read_json("json_schemas/nota_tecnica.json"),
          # Templates de Notas tecnicas
          templates = read_json("json_schemas/nota_tecnica_template.json")
        )
      })

      # Guardar cambios a notas técnicas
      observeEvent(input$notas_tecnicas_editor_edit, {
        showModal(
          modalDialog(
            # Se exige contraseña para poder guardar las notas tecnicas
            title = "Contraseña", size = "s", easyClose = TRUE,fade = TRUE,
            passwordInput(ns("notas_tecnicas_pw"), label = NULL),
            footer = shinyWidgets::actionGroupButtons(
              inputIds = ns(c("notas_tecnicas_close",
                "notas_tecnicas_conf")),
              labels = c("Cerrar", "Guardar"),
              fullwidth = TRUE
            )
          )
        )
      })

      observeEvent(input$notas_tecnicas_close, {
        removeModal()
      })

      # la sección de validacion se puede generalizar facilmente
      # Es exactamente igual a la seccion de los perfiles
      observeEvent(input$notas_tecnicas_conf, {
        password_correct <- identical(
          input$notas_tecnicas_pw,
          Sys.getenv("CONF_PW"))
        if (password_correct) {
          notas_tecnicas_nuevo <- data.frame(
            "notas_tecnicas" = input$notas_tecnicas_editor_edit$raw)
          removeModal()
          tryCatch(
            expr = {
              # Se valida contra un json_schema
              test_prettify <- input$notas_tecnicas_editor_raw %>%
                toJSON()
              validado <- json_validate(
                json = input$notas_tecnicas_editor_edit$raw,
                schema = "json_schemas/nota_tecnica.json")
              if (!validado) stop("El formato es invalido")
              if (validado) {
                dbWriteTable(
                  conn = conn,
                  name = "perfiles_notas_tecnicas_v2",
                  notas_tecnicas_nuevo,
                  overwrite = TRUE
                )
                # Se envia mensaje de que fueran actualizadas
                # las notas tecnicas
                opciones$notas_tecnicas_updated <- FALSE
                opciones$notas_tecnicas_updated <- TRUE
                showNotification(
                  ui = "La nota técnica se a guardado.",
                  type = "message"
                )
              }
            },
            error = function(e) {
              print(e)
              sendSweetAlert(
                session = session,
                title = "Error",
                text = e[1],
                type = "error"
              )
            }
          )
        }
        if (!password_correct) {
          showNotification(
            ui = "La contraseña no es correcta.",
            type = "error"
          )
        }
      })
    }
  )
}
