reactiveJsonEditOutput <- function(outputId = NULL, label = NULL, height = "400px",
                                   width = "100%") {
  
  script <- 'document.getElementById("button_id").onclick = function() {
      var listdata = HTMLWidgets.find("#outputId").editor.getText();
       Shiny.onInputChange("edit_id", listdata);
      };'
  script <- script %>% 
    str_replace("button_id", paste0(outputId, "_save")) %>% 
    str_replace("outputId", outputId) %>% 
    str_replace("edit_id", paste0(outputId, "_edit"))
  
  tagList(
    jsoneditOutput(
      outputId = outputId,
      height = height,
      width = width
    ),
    tags$br(),
    actionButton(
      inputId = paste0(outputId, "_save"),
      label = label
    ),
    htmltools::tags$script(script)
  )
  
}

