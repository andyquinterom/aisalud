# Carga de paquetes y opciones -------------------------------------------------

library(shiny)
library(dbplyr)
library(shinyWidgets)
library(feather)
library(data.table)
library(writexl)
library(varhandle)
library(Cairo)
library(lubridate)
library(stringr)
library(scales)
library(readr)
library(plotly)
library(tidyr)
library(DT)
library(markdown)
library(tableHTML)
library(withr)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjqui)
library(DBI)
library(RPostgres)
library(dplyr)
library(shinycssloaders)
library(promises)
library(future)
library(maps)
library(htmltools)
library(sparklyr)
library(dbplot)
library(jsonlite)
library(shinyAce)
library(listviewer)
library(jsonvalidate)
library(mapview)

# Si el administrador define el maxRequestSize en las variables de ambiente
# entonces esta se utilizará. De esta manera se puede limitar cuantos
# datos puede subir un usuario. Por defecto este valor son 300 MB.

maxRequestSize <- 300 * 1024 ^ 2
if (Sys.getenv("maxRequestSize") != "") {
  maxRequestSize <- 
    as.numeric(as.character(Sys.getenv("maxRequestSize"))) * 1024^2
}

options(shiny.maxRequestSize = maxRequestSize)
options(spinner.color = "#222d32")

# Carga de funciones -----------------------------------------------------------

source_dir <- function(path) {
  purrr::map(file.path(path, list.files(path)), source)
}

source_dir("R")
source_dir("modules")
source_dir("widgets")
# Carga de datos ---------------------------------------------------------------

# Se establece conexión a una instancia de PostgrSQL

conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DATABASE_NAME"),
  user = Sys.getenv("DATABASE_USER"),
  password = Sys.getenv("DATABASE_PW"),
  host = Sys.getenv("DATABASE_HOST"),
  port = Sys.getenv("DATABASE_PORT"),
  bigint = "integer",
  sslmode = "allow")

# Lista de las tablas disponibles en PostgreSQL

tabla_perfiles <- dbListTables(conn = conn)

# Si la tabla de perfiles no existe se lee y se sube.

if ("perfiles_usuario" %notin% tabla_perfiles) {
  dbWriteTable(
    conn = conn,
    name = "perfiles_usuario",
    data.frame(
      "perfiles" = read_file("json_schemas/perfiles_default.json")
    )
  )
}

# Si la tabla de notas tecncias no existe se genera.

if ("perfiles_notas_tecnicas" %notin% tabla_perfiles) {
  dbWriteTable(
    conn = conn,
    name = "perfiles_notas_tecnicas",
    data.frame(
      "notas_tecnicas" = read_file("json_schemas/nota_tecnica_defualt.json")
    )
  )
}

# Se lee RData de departamentos de colombia
departamentos <- readRDS("rds/departamentos_simplificado.rds")
