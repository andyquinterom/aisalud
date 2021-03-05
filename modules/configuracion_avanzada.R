configuracion_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        tabsetPanel(
          tabPanel(
            title = "Perfiles",
            aceEditor(
              outputId = ns("perfil_editor"),
              mode = "json",
              value = "", 
              height = "70vh", 
            ),
            actionButton(
              inputId = ns("perfil_actualizar"),
              label = "Guardar perfiles"
            )
          ),
          tabPanel(
            title = "Notas técnicas"
          )
        )
      )
    )
  )
  
}

configuracion_server <- function(id, opciones) {
  
  ns <- NS(id)
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      # Perfiles ----------------
      
      observe({
        updateAceEditor(
          session = session,
          editorId = "perfil_editor",
          value = opciones$perfil_raw
        )
      })
      
      observeEvent(input$perfil_actualizar, {
        perfil_nuevo <- data.frame("perfiles" = input$perfil_editor)
        
        tryCatch(
          expr = {
            parse_json(input$perfil_editor, simplifyVector = TRUE)
            
            dbWriteTable(
              conn = conn,
              Id(schema = "config", table = "perfiles_usuario"),
              perfil_nuevo,
              overwrite = TRUE
            )
            
            opciones$perfil_raw <- tbl(conn, "perfiles_usuario") %>%
              pull(perfiles) %>%
              prettify()
            
            opciones$perfil_lista <- opciones$perfil_raw %>%
              parse_json(simplifyVector = TRUE)
            
            opciones$perfil_updated <- FALSE
            opciones$perfil_updated <- TRUE
            
            updateAceEditor(
              session = session,
              editorId = "perfil_editor",
              value = opciones$perfil_raw
            )
            
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
      
    }
  )
  
}