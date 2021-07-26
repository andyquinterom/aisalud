# Analítica Integrada Salud - Lineamiento de contribuidores

Analítica Integrada Salud (AIS) es una aplicación shiny que encapsula los
procesos de MD&CO - BPMIC. Esta aplicación esta diseñada para funcionar con
diversos conjuntos de datos, por lo cual sus funciones deben ser generales y
amplias. Además, se busca que el uso sea intuitivo y facil para los usuarios.

Este manual de lineamiento garantiza que el proyecto mantenga orden y sea facil
de mantener. El estilo de código, estructura de directorios y manual de commits
se encuentra en este manual.

## Diseño y manual del código

### Shiny

Para garantizar el futuro mantenimiento de la aplicación, la aplicación se
estructurará con módulos diferenciados y divergentes. Si dos módulos son
demasiado parecidos, puede ser necesario considerar juntar sus funcionalidades.

Si existen inputs que se repiten constantemente a través de los módulos, puede
ser necesario dedicar una función a este widget. Además, si se evalua que un
usuario pueda llegar a utilizar este input en parelo en varios módulos, puede
ser necesario ingeniar una manera de comunicar a los módulos.

La comunicación entre módulos se dará solo a través de la variable `opciones`.
Este variable se declara en el `server.R` y se encarga de comunicar las
opciones globales a todos los módulos que la puedan necesitar. Si se requieren
valores preterminados para esta variable estos se deben declarar directamente
en el `server.R`.

### Funciones y Variables

#### Nomenclatura (naming conventios)

Los nombres de las variables y funciones deben ser explicitas en su rol en la
aplicación. Se debe intentar que las variables lean cómo texto para un humano
lo más posible.

Se deben evitar nombres de variables muy largas. Si esto no es posible, se
puede intentar agrupar en una lista almenos documentar muy bien con comentarios.

##### Funciones de UI

La gran mayoria de las funciones de Shiny (especialmente las de UI) están
escritas en **camelCase**. Por esta razón, las funciones de UI y los nombres
de módulos se escribirán en **camelCase**.

##### Funciones y variables de R

Las funciones y variables de R (no Shiny) se escribirán **snake_case**. Esto
también aplica para los nombres de las columnas en una tabla y los elementos
de una lista o árbol.

Como referencia utilizar http://adv-r.had.co.nz/Style.html.

##### Variables globales y de ambiente

Las variables globales (del sistema o del ambiente no generadas por R) se
escribirán en **SCREAMING_SNAKE_CASE**. Esto ayudará a evitar confusión con las
variables creadas por R.

##### Funciones de JavaScript

El estandar de Nomenclatura para JS es **camelCase**.

##### CSS y HTML

El nombre de class, id, y otros atributos de HTML se escribirán en **snake_case**.

#### Database Queries

Una gran porción de las funciones de AIS están dedicadas a hacer un query a
PostgreSQL. Estás funciones se construirán mayormente con extensiones del
tidyverse (dbplyr). Dado esto, es necesario tomar en cuenta las siguientes
decisiones al momento de escribir un query en forma de pipeline.

**Evitar repetir código**

Para evitar repetir código en un pipeline se puede utilizar un truco con las
llaves para continuar o modificar pipeline con un if-else statement.

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

El rendimiento de loops y funciones recursivas en R no es el mejor. Se debe
intentar delegar este trabajo a C++ con alguna librería o evitar loops por
completo.

Buscar algoritmos que utilicen tablas y funciones que se puedan delegar a
PostgreSQL o utilizar librerías óptimas para este procesamiento como `dplyr` o
`data.table`.

#### Evitar errorer y catching

Las funciones básicas de manipulación de datos nunca deben asumir un valid
input. Siempre se debe intentar catch este tipo de errores antes de ejecutar
código.

```r
test_error <- function(x, y, z = 1) {
  if (is.null(x) || is.na(x)) stop("x vacio")
  if (is.null(x) || is.na(z)) stop("y vacio")
  if (!identical(class(z), "numeric")) stop("z no es numérico")
  response <- rep(paste(x, y), z)
  return(reponse)
}
```

Evitar este tipo de errores no es necesario para todos los parametros y
situaciones, solo en casos que se especule que pueden ocurrir. Este tipo de
errores explicitos son necesarios dado que en las funciones que llamamos
directamente desde un módulo en Shiny estarán wrapped en un `tryCatch`,
lo cual logeará y mostrará el error al usuario. Entre más se pueda guiar a
este mejor.

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

Al momento de escribir if statement, evitar else en árboles de decisión
extensos. Es mejor intentar generalizar funciones para que no requieran ifelse
statements.

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

Si se quisieran agregar más posibilidades podría ser dificil de implementar.
Si se puede generalizar de la siguiente manera, sería más facil de implementar
a través de la aplicación.

```r
extraer_unidad <- function(uni) {
  uni_validas <- c("paciente", "factura", "prestaciones")
  # Validar que la unidad no este vacia
  if (is.null(uni) || is.na(uni)) stop ("Unidad vacia")
  # Validar que sea una unidad valida
  if (!unidad %in% uni_validas) stop("Unidad invalida")
  # Ejecutar código
  if (unidad == "paciente") extraer_pacientes()
  if (unidad == "factura") extraer_facturas()
  if (unidad == "prestaciones") extraer_prestaciones()
}

extraer_unidad(respuesta)
```

Esta función puede aplicarse en cualquier lugar de la aplicación. Además de
ser segura y más facil de mantener. En caso de querer agregarse más unidades es
tan simple cómo agregarlo a `uni_validas` y agregar el if.

#### Bloques de código

Para facilitar la navegación en VI los bloques de código deben mantenerse lo
más corto posibles. Separar los bloques de código por una sola linea en blanco. 

Dentro del server-side de los módulos, pueden existir bloques dentro observers. 

Todos los outputs deben ser su propio bloque.

#### Comentarios

Cada función debe llevar un comentario con una breve explicación de su
funcionalidad y propósito. Dentro del server side de los módulos, un comentario
por bloque es necesario.

En pipelines, si existen complejidades, puede ser de gran ayudar incluir una
breve documentación.

#### Nested functions

Para evitar highly nested functions, se puede utilizar el pipe para ayudar a
resolver este problema. Sin embargo, no debe ser necesario para funciones con
2 o 3 niveles de profunidad.

```r
# Función que requiere utilizar el pipe
as.numeric(
  str_replace(
    obtener_codigo(input$objeto),
    "A", ""
  )
)

# Tranformar a --->
input$objeto %>%
  obtener_codigo() %>%
  str_replace("A", "") %>%
  as.numeric()

# Función que NO requiere pipe
print(tbl(conn, "tabla_ejemplo"))
```

### Archivos y directorios

El proyecto se manejará con una sencilla estructura de archivos y directorios.
```
root/
├─ global.R
├─ ui.R
├─ server.R
├─ R/
│  ├─ R Functions
├─ modules/
│  ├─ Shiny Modules
├─ widgets/
│  ├─ Widgets y UI Elements
├─ markdown/
│  ├─ Markdown para UI
├─ json_schemas/
│  ├─ JSON Schemas y templates
├─ www/
├─ man/
│  ├─ Manuales de uso
```

## Documentación y manuales

Cada módulo debe llevar su propia documentación / manual en la carpeta `man`.
Esta docuemantación debe ser de uso no de código. El código será documentado en
comentarios dentro del archivo respectivo.

Cada nuevo feature debe llevar su cambio correspondiente en el manual con el
fin de evitar temporadas pesadas de escribir documentación antes de un nuevo
release.

## Commits

Se seguira la especificación de conventional commits.
https://www.conventionalcommits.org/en/v1.0.0/#specification
