# Analítica Integrada Salud

Analítica Integrada Salud (AIS) es una aplicación shiny que encapsula los procesos de MD&CO - BPMIC. Esta aplicación esta diseñada para funcionar con diversos conjuntos de datos, por lo cual sus funciones deben ser generales y amplias. Además, se busca que el uso sea intuitivo y facil para los usuarios.

## Diseño y manual del código

### Shiny

Para garantizar el futuro mantenimiento de la aplicación, la aplicación se estructurará con módulos diferenciados y divergentes. Si dos módulos son demasiado parecidos, puede ser necesario considerar juntar sus funcionalidades.

Si existen inputs que se repiten constantemente a través de los módulos, puede ser necesario dedicar una función a este widget. Además, si se evalua que un usuario pueda llegar a utilizar este input en parelo en varios módulos, puede ser necesario ingeniar una manera de comunicar a los módulos.

La comunicación entre módulos se dará solo a través de la variable `opciones`. Este variable se declara en el `server.R` y se encarga de comunicar las opciones globales a todos los módulos que la puedan necesitar. Si se requieren valores preterminados para esta variable estos se deben declarar directamente en el `server.R`.

### Funciones de R

#### Nomenclatura (naming conventios)

Los nombres de las variables y funciones deben ser explicitas en su rol en la aplicación. Se debe intentar que las variables lean cómo texto para un humano lo más posible.

Se deben evitar nombres de variables muy largas. Si esto no es posible, se puede intentar agrupar en una lista almenos documentar muy bien con comentarios.

##### Funciones de UI

La gran mayoria de las funciones de Shiny (especialmente las de UI) están escritas en **camelCase**. Por esta razón, las funciones de UI y los nombres de módulos se escribirán en **camelCase**.

##### Funciones y variables de R

Las funciones y variables de R (no Shiny) se escribirán **snake_case**. Esto también aplica para los nombres de las columnas en una tabla y los elementos de una lista o arból.

##### Variables globales y de ambiente

Las variables globales (del sistema o del ambiente no generadas por R) se escribirán en **SCREAMING_SNAKE_CASE**. Esto ayudará a evitar confusión con las variables creadas por R.

##### Funciones de JavaScript

El estandar de Nomenclatura para JS es **camelCase**.

##### CSS y HTML

El nombre de class, id, y otros atributos de HTML se escribirán en **snake_case**.

#### Database Queries

Una gran porción de las funciones de AIS están dedicadas a hacer un query a PostgreSQL. Estás funciones se construrán mayormente con extensiones del tidyverse (dbplyr). Dado esto, es necesario tomar en cuenta las siguientes decisiones al momento de escribir un query en forma de pipeline.

**Evitar repetir código**

Para evitar repetir código en un pipeline se puede utilizar un truco con las llaves para continuar o modificar pipeline con un if-else statement.

```r
# Ejemplo de código con repetición
if (ignore_mpg) {
  datos <- mtcars %>%
   select(-mpg)
} else {
  datos <- mtcars
}

# En un solo pipeline
datos <- mtcars %>%
  {if (ignore_mpg) select(., -mpg)
   else . }

# Si son multilinea

datos <- mtcars %>%
  {if (ignore_mpg) {
    select(., -mpg)
  } else {.}}

```

#### Evitar loops sobre datos

El rendimiento de loops y funciones recursivas en R no es el mejor. Se debe intentar delegar este trabajo a C++ con alguna librería o evitar lo por completo.

Buscar algoritmos que utilicen tablas y funciones que se puedan delegar a PostgreSQL o a las una librería cómo `dplyr` o `data.table` son óptimos.

#### Evitar errorer y catching

Las funciones básicas de manipulación de datos nunca deben asumir un valid input. Siempre se debe intentar catch este tipo de errores antes de ejecutar código.

```r
test_error <- function(x, y, z = 1) {
  if (is.null(x) || is.na(x)) stop("x vacio")
  if (is.null(x) || is.na(z)) stop("y vacio")
  if (!identical(class(z), "numeric")) stop("z no es numérico")
  response <- rep(paste(x, y), z)
  return(reponse)
}
```

Evitar este tipo de errores no es necesario para todos los parametros y situaciones, solo en casos que se especule que pueden ocurrir. Este tipo de errores explicitos son necesarios dado que en las funciones que llamamos directamente desde un módulo en Shiny estarán wrapped en un `tryCatch`, lo cual logeará y mostrará el error al usuario. Entre más se pueda guiar a este mejor.

```r
observe({
  tryCatch(
    expr = {
      test_error(
        x = input$x,
        y = input$y,
        z = input$z)
    },
    error = function(e) {
      print(e)
      sendSweetAlert(
        session = session,
        title = "Error", 
        type = "error",
        text = e[1])
    }
  )
})
```

#### Evitar else

Al momento de escribir if statmente, evitar else en arboles de decisión extensos. Es mejor intentar generalizar funciones para que no requieran ifelse statements.

```r
# Ejemplo de código confuso con if-else
if (respuesta == "paciente") {
  extraer_pacientes()
} else if (respuesta == "factura") {
  extraer_facturas()
} else if (respuesta == "prestaciones") {
  extraer_prestaciones()
}
```

Si se quisieran agregar más posibilidades podría ser dificil de implementar. Si se puede generalizar de la siguiente manera, sería más facil de implementar a través de la aplicación.

```r
extraer_unidad <- function(uni) {
  uni_validas <- c("paciente", "factura", "prestaciones")
  # Validar que la unidad no este vacia
  if (is.null(uni) || is.na(uni)) stop ("Unidad vacia")
  # Validar que sea una unidad valida
  if (!unidad %in% uni_validas) stop("Unidad invalida")
  # Ejecutar código
  if (unidad == "paciente") extraer_pacientes(uni)
  if (unidad == "factura") extraer_facturas(uni)
  if (unidad == "prestaciones") extraer_prestaciones(uni)
}
```

Esta función puede aplicarse en cualquier lugar de la aplicación. Además de ser segura y más facil de mantener. En caso de querer agregarse más unidades es tan simple cómo agregarlo a `uni_validas` y agregar el if.
