caja_de_bigotes_agrupador <- function(
  data, columna_numeros, columnas_sep) {
  
  box_plot <- plot_ly(type = "box")
  
  lapply(
    X = 1:nrow(columnas_sep),
    FUN = function(i) {
      data_temp <- merge.data.table(
        x = columnas_sep[i],
        y = data
      )[[columna_numeros]]
      box_plot <<- box_plot %>%
        add_trace(y = data_temp, name = paste(
          unlist(columnas_sep[i]), collapse = "\n"
        ))
        
    }
  )
  
  return(box_plot %>%
           config(locale = "es") %>%
           layout(xaxis = list(title = "Agrupador"),
                  yaxis = list(title = "Valor",
                               tickformat = ",.2f"),
                  showlegend = FALSE))
}

