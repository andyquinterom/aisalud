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
            title = "Notas técnicas",
            aceEditor(
              outputId = ns("notas_tecnicas_editor"),
              mode = "json",
              value = "", 
              height = "70vh", 
            ),
            actionButton(
              inputId = ns("notas_tecnicas_actualizar"),
              label = "Guardar notas técnicas"
            )
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
            
            perfil_nuevo$perfiles %>%
              prettify()
            
            dbWriteTable(
              conn = conn,
              Id(schema = "config", table = "perfiles_usuario"),
              perfil_nuevo,
              overwrite = TRUE
            )
            
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
      
      
      # Nota técnicas 
      
      # Perfiles ----------------
      
      observe({
        updateAceEditor(
          session = session,
          editorId = "notas_tecnicas_editor",
          value = opciones$notas_tecnicas_raw
        )
      })
      
      observeEvent(input$notas_tecnicas_actualizar, {
        notas_tecnicas_nuevo <- data.frame(
          "notas_tecnicas" = input$notas_tecnicas_editor)
        
        tryCatch(
          expr = {
            
            notas_tecnicas_nuevo$notas_tecnicas  %>%
              prettify()
            
            dbWriteTable(
              conn = conn,
              Id(schema = "config", table = "notas_tecnicas"),
              notas_tecnicas_nuevo,
              overwrite = TRUE
            )
            
            opciones$notas_tecnicas_updated <- FALSE
            opciones$notas_tecnicas_updated <- TRUE
            
            updateAceEditor(
              session = session,
              editorId = "notas_tecnicas_editor",
              value = opciones$notas_tecnicas_raw, 
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