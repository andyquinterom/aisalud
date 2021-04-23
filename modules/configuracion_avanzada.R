configuracion_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        tabsetPanel(
          tabPanel(
            title = "Perfiles",
            reactiveJsonEditOutput(
              outputId = ns("perfil_editor"),
              height = "70vh", 
              label = "Guardar"
            )
          ),
          tabPanel(
            title = "Notas técnicas",
            reactiveJsonEditOutput(
              outputId = ns("notas_tecnicas_editor"),
              height = "70vh", 
              label = "Guardar"
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
      
      output$perfil_editor <- renderJsonedit({
        jsonedit(
          opciones$perfil_raw,
          language = "es",
          languages = "es",
          name = "Perfiles",
          enableTransform = FALSE,
          schema = read_json("json_schemas/perfiles.json")
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
      
      observeEvent(input$perfil_actualizar_conf, {
        
        if (input$perfil_actualizar_pw == Sys.getenv("CONF_PW")) {
          perfil_nuevo <- data.frame("perfiles" = input$perfil_editor_edit)
          removeModal()
          tryCatch(
            expr = {
              
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
      
      
      # Nota técnicas 
      
      # Perfiles ----------------
      
      output$notas_tecnicas_editor <- renderJsonedit({
        jsonedit(
          opciones$notas_tecnicas_raw,
          language = "es",
          languages = "es",
          name = "Notas técnicas",
          enableTransform = FALSE
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
              
              notas_tecnicas_nuevo$notas_tecnicas  %>%
                prettify()
              
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