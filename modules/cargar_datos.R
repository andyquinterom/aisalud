# El módulo de cargar_datos tiene como proposito alimentar los datos de
# toda la aplicación. La varaible principal en la cual se almacenan los datos
# es la opciones. Esta se declara en el server.R y se muta dentro de este
# módulo. Si algun otro modulo desea cambiar algun dato global se puede hacer
# un request a través de la misma variable opciones. Sin embargo, los datos
# no deben ser mutados.

cargar_datos_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$script(type = "text/javascript", src = "reactiveJsonEdit.js"),
    fluidRow(
      box(
        style = "min-height: 692px;",
        width = 4,
        tabsetPanel(
          tabPanel(
            title = "Nube",
            tags$br(),
            tags$br(),
            selectizeInput(
              width = "100%",
              inputId = ns("tabla"),
              label = "Seleccionar datos",
              choices = "Ninguno"
            ),
            selectizeInput(
              inputId = ns("columna_valor"),
              label = "Columna de valor:",
              width = "100%",
              choices = "valor",
              selected = "valor"
            ),
            dateRangeInput(
              inputId = ns("fecha_rango"),
              label = "Fechas:",
              min = "1970-01-01", 
              max = NULL, 
              format = "dd/mm/yyyy",
              language = "es"),
            tags$style(HTML(".datepicker {z-index:99999 !important;}"))
          ),
          tabPanel(
            title = "Subir datos",
            fileInput(
              inputId = ns("file"),
              label = "", 
              buttonLabel = "Subir archivo",
              placeholder = "Ningún archivo"),
            radioButtons(
              inputId = ns("file_type"),
              label = "Tipo de archivo",
              inline = TRUE, 
              choices = c("csv", "feather")),
            actionButton(
              inputId = ns("file_options_open"),
              label = "Opciones",
              width = "100%"),
            tags$br(),
            tags$br(),
            dateRangeInput(
              inputId = ns("file_fecha_rango"),
              label = "Fechas:",
              min = NULL, 
              max = NULL, 
              format = "dd/mm/yyyy",
              language = "es"),
            tags$style(HTML(".datepicker {z-index:99999 !important;}")),
            textInput(
              inputId = ns("file_formato_fecha"),
              label = "Formato de Fecha",
              value = "%d/%m/%Y"),
            actionButton(inputId = ns("file_load"), label = "Aplicar",
                         width = "100%"),
            tags$br(),
            tags$br(),
            selectizeInput(
              inputId = ns("file_columna_valor"),
              label = "Columna de valor:",
              width = "100%",
              choices = "valor",
              selected = "valor")
          ),
          tabPanel(
            title = "Opciones",
            tags$br(),
            tags$br(),
            selectizeInput(
              inputId = ns("perfil"),
              width = "100%",
              "Perfil:",
              choices = "Ninguno"),
            tags$hr(),
            checkboxInput(
              inputId = ns("cantidad"),
              width = "100%", 
              label = "Prestaciones por cantidad", 
              value = FALSE
            )
          )
        )
      ),
      box(
        width = 8,
        style = "min-height: 650px;",
        tabsetPanel(
          tabPanel(
            title = "Resumen",
            plotlyOutput(outputId = ns("valor_con_tiempo"), height = "630px") %>%
              withSpinner()
          ),
          tabPanel(
            title = "Perfiles",
            reactiveJsonEditOutput(
              outputId = ns("perfil_editor"),
              height = "630px", 
              label = "Guardar"
            )
          ),
          tabPanel(
            title = "Notas técnicas",
            reactiveJsonEditOutput(
              outputId = ns("notas_tecnicas_editor"),
              height = "630px", 
              label = "Guardar"
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
              updateSelectizeInput(
                session = session,
                inputId = "columna_valor",
                # se selecciona valor por defecto
                selected = opciones$valor_costo,
                choices = prepara_opciones$colnames
              )
            },
            error = function(e) {
              print(e)
              sendSweetAlert(
                session = session,
                title = "Error",
                text = "No se pudo leer la tabla seleccionada. Valide que aun exista esta tabla o contacte a un administrador.",
                type = "error"
              )
            }
          )
        }
      })
      
      # Si se esta trabajando con datos locales se utilizan diferentes
      # inputs del valor
      observe({
        if (input$tabla != "Ninguno" &&
            input$tabla != "") {
          opciones$valor_costo <- ifelse(
            test = input$columna_valor != "",
            yes = input$columna_valor,
            no = opciones$valor_costo)
        }
        if (opciones$datos_cargados) {
          opciones$valor_costo <- ifelse(
            test = input$file_columna_valor != "",
            yes = input$file_columna_valor,
            no = opciones$valor_costo)
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
                choices = unique(c("Ninguno", tables_ais)) )
              print(e)
              sendSweetAlert(
                session = session,
                title = "Error",
                text = "No se pudo leer la tabla seleccionada. Valide que aun exista esta tabla o contacte a un administrador.",
                type = "error"
              )
            }
          )
        }
      })
      
      # Se actualiza la seleccion de columnas de valor
      observe({
        # Si los datos se seleccionan de la nube
        if (input$tabla != "Ninguno" &&
            input$tabla != "") {
          updateSelectizeInput(
            session = session,
            inputId = "columna_valor",
            choices = opciones$colnames_num,
            selected = "valor"
          )
        }
        # Si los datos son locales
        if (opciones$datos_cargados) {
          updateSelectizeInput(
            session = session,
            inputId = "file_columna_valor",
            choices = opciones$colnames_num,
            selected = "valor"
          )
        }
      })
      
      output$valor_con_tiempo <- renderPlotly({
        if (opciones$datos_cargados) {
            tabla <- opciones$tabla_nombre
            valor_costo <- opciones$valor_costo
            fecha_min <- opciones$fecha_rango[1]
            fecha_max <- opciones$fecha_rango[2]
            datos <- opciones$tabla %>%
              mutate(
                mes_temporal = year(fecha_prestacion)*100 +
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
              plot_ly(x = ~mes_label_temporal,
                      y = ~suma,
                      type = "bar") %>%
              config(locale = "es") %>%
              layout(
                title = "Suma del valor a mes",
                xaxis = list(
                  title = "Mes",
                  categoryorder = "array",
                  categoryarray = ~mes_temporal),
                yaxis = list(title = "Suma",
                             tickformat = ",.2f"))
        }
      })
      
      # Cargar datos locales -----------------
      
      file_opciones <- reactiveValues(
        "value_decimal" = ".",
        "value_delimitador" = ",",
        "value_sheet" = NULL,
        "value_range" = NULL,
        "enabled" = FALSE
      )
      
      observeEvent(input$file_options_open, {
        showModal(
          session = session,
          ui = modalDialog(
            title = "Opciones archivo",
            easyClose = TRUE,
            fade = TRUE,
            datos_opciones_ui(
              id = id,
              file_type = input$file_type,
              value_decimal = file_opciones$value_decimal,
              value_delimitador = file_opciones$value_delimitador,
              value_range = file_opciones$value_range,
              value_sheet = file_opciones$value_sheet,
              value_file = file_opciones$value_file),
            footer = actionButton(
              inputId = ns("file_opciones_guardar"),
              label = "Guardar")
          )
        )
      })
      
      observeEvent(input$file_opciones_guardar, {
        file_opciones$value_decimal <- input$value_decimal
        file_opciones$value_delimitador <- input$value_delimitador
        file_opciones$value_sheet <- input$value_sheet
        file_opciones$value_range <- input$value_range
        file_opciones$value_file <- input$value_file
        removeModal(session = session)
      })
      
      observeEvent(input$file_load, {
        tryCatch(expr = {
          if (!is.null(input$file)) {
            opciones$fecha_rango <- input$file_fecha_rango
            value_delimitador <- ifelse(
              test = file_opciones$value_delimitador == "Espacios",
              yes = "\t",
              no = file_opciones$value_delimitador)
            if (input$file_type == "csv") {
              datos_read <- readr::read_delim(
                file = input$file$datapath, 
                delim = value_delimitador, 
                locale = locale(
                  decimal_mark = file_opciones$value_decimal))
            } else if (input$file_type == "feather") {
              datos_read <- read_feather(path = input$file$datapath)
            }
            opciones$tabla_original <- datos_read %>%
              rename_with(tolower) %>%
              mutate(fecha_prestacion = as.Date(
                fecha_prestacion, format = input$file_formato_fecha)) %>%
              filter(fecha_prestacion >= as.Date(input$file_fecha_rango[1]) &
                       fecha_prestacion <= as.Date(input$file_fecha_rango[2]))
            opciones$tabla <- opciones$tabla_original
            opciones$colnames <- opciones$tabla %>%
              colnames()
            testfor_numeric <- opciones$tabla %>%
              summarise_all(class) == "numeric"
            opciones$colnames_num <- opciones$colnames[testfor_numeric]
            file_opciones$enabled <- TRUE
          }},
          error = function(e) {
            print(e[1])
            sendSweetAlert(
              session = session,
              title = "Error",
              text = e[1],
              type = "error"
            )
          })
      })
      
      
      # Enable tabla
      
      observe({
        if (input$tabla != "Ninguno" &&
            input$tabla != "") {
          opciones$datos_cargados <- TRUE
        } else if (input$tabla == "Ninguno" &&
                   file_opciones$enabled) {
          opciones$datos_cargados <- TRUE
        } else {
          opciones$datos_cargados <- FALSE
        }
      })
      
      # Perfiles ----------------------------------------------------
      
      observe({
        updateSelectizeInput(
          session = session,
          inputId = "perfil",
          selected = opciones$perfil_selected
        )
      })
      
      observe({
        opciones$perfil_updated
        perfil_raw <- tbl(conn, "perfiles_usuario") %>%
          pull(perfiles)
        tryCatch(
          expr = {
            opciones$perfil_raw <- perfil_raw %>%
              prettify()
            opciones$perfil_lista <- opciones$perfil_raw %>%
              parse_json(simplifyVector = TRUE)
            updateSelectizeInput(
              session = session,
              inputId = "perfil",
              selected = opciones$perfil_selected,
              choices = c("Ninguno", names(opciones$perfil_lista))
            )
          },
          error = function(e) {
            opciones$perfil_raw <- perfil_raw
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
      
      observe({
        if (input$perfil != "Ninguno") {
          opciones$perfil_enable <- TRUE
          opciones$perfil_selected <- input$perfil
        } else {
          opciones$perfil_enable <- FALSE
        }
      })

      output$perfil_editor <- renderJsonedit({
        jsonedit(
          opciones$perfil_raw,
          language = "es",
          languages = "es",
          name = "Perfiles",
          enableTransform = FALSE,
          schema = read_json("json_schemas/perfiles.json"),
          templates = read_json("json_schemas/perfiles_template.json")
        )
      })
      
      observeEvent(input$perfil_editor_save, {
        showModal(
          modalDialog(
            title = "Contraseña", size = "s", easyClose = TRUE,fade = TRUE,
            passwordInput(ns("perfil_actualizar_pw"), label = NULL),
            footer = actionButton(
              inputId = ns("perfil_actualizar_conf"),
              label = "Guardar perfiles")
          )
        )
      })

      observe({
        print(input$test)
      })
      
      observeEvent(input$perfil_actualizar_conf, {
        if (input$perfil_actualizar_pw == Sys.getenv("CONF_PW")) {
          perfil_nuevo <- data.frame("perfiles" = input$perfil_editor_edit)
          removeModal()
          tryCatch(
            expr = {
              validado <- json_validate(
                json = input$perfil_editor_edit,
                schema = "json_schemas/perfiles.json" 
              ) 
              if (validado) {
                perfil_nuevo$perfiles %>%
                  prettify()
                dbWriteTable(
                  conn = conn,
                  name = "perfiles_usuario",
                  perfil_nuevo,
                  overwrite = TRUE
                )
                opciones$perfil_updated <- FALSE
                opciones$perfil_updated <- TRUE
                showNotification(
                  ui = "El perfil se a guardado.",
                  type = "message"
                )
              } else {
                sendSweetAlert(
                  session = session,
                  title = "Error",
                  text = "No se ha podido guardar. Valida que todos los parametros esten presentes y completos.",
                  type = "error"
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
        } else {
          showNotification(
            ui = "La contraseña no es correcta.",
            type = "error"
          )
        }
      })
      
      # Cantidad
      
      observe({
        cantidad_enable <- NULL
        if (opciones$perfil_enable) {
          cantidad_enable <- opciones$perfil_lista[[
            opciones$perfil_selected]][["cantidad"]]
        }
        if (!is.null(cantidad_enable)) {
          updateCheckboxInput(
            inputId = "cantidad",
            value = cantidad_enable
          )
        }
      })
      
      observe({
        opciones$cantidad <- input$cantidad
      })
      
      # Notas tecnicas

      observe({
        opciones$notas_tecnicas_updated
        opciones$notas_tecnicas_raw <- tbl(conn, "perfiles_notas_tecnicas") %>%
          pull(notas_tecnicas)
        tryCatch(
          expr = {
            opciones$notas_tecnicas_lista <- opciones$notas_tecnicas_raw %>%
              parse_json(simplifyVector = TRUE)
            opciones$notas_tecnicas <- opciones$notas_tecnicas_lista %>%
              parse_nt()
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

      output$notas_tecnicas_editor <- renderJsonedit({
        jsonedit(
          opciones$notas_tecnicas_raw,
          language = "es",
          languages = "es",
          name = "Notas técnicas",
          enableTransform = FALSE,
          schema = read_json("json_schemas/nota_tecnica.json"),
          templates = read_json("json_schemas/nota_tecnica_template.json")
        )
      })
      
      observeEvent(input$notas_tecnicas_editor_save, {
        showModal(
          modalDialog(
            title = "Contraseña", size = "s", easyClose = TRUE,fade = TRUE,
            passwordInput(ns("notas_tecnicas_pw"), label = NULL),
            footer = actionButton(
              inputId = ns("notas_tecnicas_conf"),
              label = "Guardar notas técnicas")
          )
        )
      })
      
      observeEvent(input$notas_tecnicas_conf, {
        if (input$notas_tecnicas_pw == Sys.getenv("CONF_PW")) {
          notas_tecnicas_nuevo <- data.frame(
            "notas_tecnicas" = input$notas_tecnicas_editor_edit)
          removeModal()
          tryCatch(
            expr = {
              validado <- json_validate(
                json = input$notas_tecnicas_editor_edit,
                schema = "json_schemas/nota_tecnica.json" 
              ) 
              if (validado) {
                dbWriteTable(
                  conn = conn,
                  name = "perfiles_notas_tecnicas",
                  notas_tecnicas_nuevo,
                  overwrite = TRUE
                )
                opciones$notas_tecnicas_updated <- FALSE
                opciones$notas_tecnicas_updated <- TRUE
                showNotification(
                  ui = "La nota técnica se a guardado.",
                  type = "message"
                )
              } else {
                sendSweetAlert(
                  session = session,
                  title = "Error",
                  text = "No se ha podido guardar. Valida que todos los parametros esten presentes y completos.",
                  type = "error"
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
        } else {
          showNotification(
            ui = "La contraseña no es correcta.",
            type = "error"
          )
        }
      })
 
    }
  )
}

# Funciones --------------------------------------------------------------------

dbListNumericFields <- function(conn, table_name) {
  sql <- "select
       col.column_name
from information_schema.columns col
join information_schema.tables tab on tab.table_schema = col.table_schema
                                   and tab.table_name = col.table_name
                                   and tab.table_type = 'BASE TABLE'
where col.data_type in ('smallint', 'integer', 'bigint', 
                        'decimal', 'numeric', 'real', 'double precision',
                        'smallserial', 'serial', 'bigserial', 'money')
      and col.table_schema not in ('information_schema', 'pg_catalog')
      and col.table_name in (?id)
order by col.table_schema,
         col.table_name,
         col.ordinal_position"
  query <- sqlInterpolate(conn, sql, id = table_name)
  dbGetQuery(conn, str_replace_all(query, "#####", table_name)) %>%
    unlist() %>%
    unname()
}

datos_opciones_ui <- function(
  id, file_type, value_decimal, value_delimitador, value_sheet, value_range,
  value_file) {
  
  if (file_type == "csv") {
    return(
      datos_opciones_csv_ui(
        id = id,
        value_decimal = value_decimal,
        value_delimitador = value_delimitador)
    )
  }
  
  if (file_type == "datos didacticos") {
    return(
      datos_opciones_cloud_ui(
        id = id,
        value_file = value_file
      )
    )
  }
  
}

datos_opciones_csv_ui <- function(id, value_delimitador, value_decimal) {
  ns <- NS(id)
  
  tagList(
    radioButtons(
      inputId = ns("value_delimitador"),
      choices = c(",", ";", "|", "Espacios"),
      label = "Delimitador",
      inline = TRUE,
      selected = value_delimitador
    ),
    
    radioButtons(
      inputId = ns("value_decimal"),
      choiceNames = c("Punto", "Coma"),
      choiceValues = c(".", ","),
      label = "Separador decimal",
      inline = TRUE,
      selected = value_decimal
    )
  )
  
}

datos_opciones_cloud_ui <- function(id, value_file) {
  ns <- NS(id)
  
  tagList(
    selectizeInput(
      inputId = ns("value_file"),
      choices = list.files("datos/saved/"),
      label = "Archivo:",
      selected = value_file
    )
  )
  
}
