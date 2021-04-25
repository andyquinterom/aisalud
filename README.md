#Analítica Integrada Salud

Analítica Integrada Salud (AIS) es una aplicación shiny que encapsula los procesos de MD&CO - BPMIC. Esta aplicación esta diseñada para funcionar con diversos conjuntos de datos, por lo cual sus funciones deben ser generales y amplias. Además, se busca que el uso sea intuitivo y facil para los usuarios.

##Diseño y manual del código

### Shiny

Para garantizar el futuro mantenimiento de la aplicación, la aplicación se estructurará con módulos diferenciados y divergentes. Si dos módulos son demasiado parecidos, puede ser necesario considerar juntar sus funcionalidades.

Si existen inputs que se repiten constantemente a través de los módulos, puede ser necesario dedicar una función a este widget. Además, si se evalua que un usuario pueda llegar a utilizar este input en parelo en varios módulos, puede ser necesario ingeniar una manera de comunicar a los módulos.

La comunicación entre módulos se dará solo a través de la variable `opciones`. Este variable se declara en el `server.R` y se encarga de comunicar las opciones globales a todos los módulos que la puedan necesitar. Si se requieren valores preterminados para esta variable estos se deben declarar directamente en el `server.R`.

### Funciones de R

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
