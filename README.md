#Analítica Integrada Salud

Analítica Integrada Salud (AIS) es una aplicación shiny que encapsula los procesos de MD&CO - BPMIC. Esta aplicación esta diseñada para funcionar con diversos conjuntos de datos, por lo cual sus funciones deben ser generales y amplias. Además, se busca que el uso sea intuitivo y facil para los usuarios.

##Diseño y manual del código

### Shiny

Para garantizar el futuro mantenimiento de la aplicación, la aplicación se estructurará con módulos diferenciados y divergentes. Si dos módulos son demasiado parecidos, puede ser necesario considerar juntar sus funcionalidades.

Si existen inputs que se repiten constantemente a través de los módulos, puede ser necesario dedicar una función a este widget. Además, si se evalua que un usuario pueda llegar a utilizar este input en parelo en varios módulos, puede ser necesario ingeniar una manera de comunicar a los módulos.

La comunicación entre módulos se dará solo a través de la variable `opciones`. Este variable se declara en el `server.R` y se encarga de comunicar las opciones globales a todos los módulos que la puedan necesitar. Si se requieren valores preterminados para esta variable estos se deben declarar directamente en el `server.R`.

### Funciones de R

#### Nomenclatura (naming conventios)

Los nombres de las variables y funciones deben ser explicitas en su rol en la aplicación. Se debe intentar que las variables lean cómo texto para un humano lo más posible.

Se deben evitar nombres de variables muy largas. Si esto no es posible, se puede intentar agrupar en una lista almenos documentar muy bien con comentarios.

#####Funciones de UI

La gran mayoria de las funciones de Shiny (especialmente las de UI) están escritas en **camelCase**. Por esta razón, las funciones de UI y los nombres de módulos se escribirán en **camelCase**.

#####Funciones y variables de R

Las funciones y variables de R (no Shiny) se escribirán **snake_case**. Esto también aplica para los nombres de las columnas en una tabla y los elementos de una lista o arból.

#####Variables globales y de ambiente

Las variables globales (del sistema o del ambiente no generadas por R) se escribirán en **SCREAMING_SNAKE_CASE**. Esto ayudará a evitar confusión con las variables creadas por R.

#####Funciones de JavaScript

El estandar de Nomenclatura para JS es **camelCase**.

#####CSS y HTML

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
