separar_spanish <- function(word_array) {
  if (!is.null(word_array)) {
    paste(
      lapply(
        X = length(word_array):1,
        word_array = word_array,
        function(i, word_array) {
          if (i == 2) {
            return(
              paste0(word_array[i], " y ")
            )
          } else if (i == 1) {
            return(word_array[i])
          } else {
            return(
              paste0(word_array[i], ", ")
            )
          }
        }
      ),
      collapse = ""
    )
  }
}