reactiveJsonEditOutput <- function(outputId = NULL, label = NULL, height = "400px",
                                   width = "100%") {
  
  script <- '
  $(document).on("shiny:value", function(e) {
    console.log(e.name);
    if (e.name === "###outputId###") {
      reactiveJsonEditMod(e)
    }
  });
  '
  script <- script %>% 
    str_replace("###outputId###", outputId)
  
  tagList(
    jsoneditOutput(
      outputId = outputId,
      height = height,
      width = width
    ),
    htmltools::tags$script(script)
  )
  
}

