#
# Analítica Integrada Salud
#
# Derechos de autor 2021 por MD&CO Consulting Group (NIT 901.119.781-5)
# Copyright (C) 2021 by MD&CO Consulting Group
#
# Este programa es software libre: puede redistribuirlo o modificarlo bajo
# los términos de la licencia Affero General Public License tal cual
# publicada por la Free Software Foundation, sea la versión 3 de la licencia
# o cualquier versión posterior. Este programa se distribuye SIN GARANTÍA
# EXPERSA O IMPLÍCITA, INCLUIDAS LAS DE NO INFRACCIÓN, COMERCIABILIDAD O
# APTITUD PARA UN PROPÓSITO PARTICULAR. Referir a la
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) para más detalles.
#

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
library(digest)
library(shinyCache)

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
check_nt_v2 <- "perfiles_notas_tecnicas_v2" %notin% tabla_perfiles
notas_tecnicas_v2 <- NULL
if (check_nt_v2 & ("perfiles_notas_tecnicas" %in% tabla_perfiles)) {
  tryCatch(
    expr = {
      notas_tecnicas_raw <- tbl(conn, "perfiles_notas_tecnicas") %>%
        pull(notas_tecnicas)
      notas_tecnicas_lista <- notas_tecnicas_raw %>%
        parse_json(simplifyVector = TRUE)
      notas_tecnicas_v2 <- notas_tecnicas_lista %>%
        parse_nt_v1_v2()
    },
    error = function(e) {
      print(e)
      notas_tecnicas_v2 <- NULL
    }
  )
}

if (check_nt_v2) {
  if (is.null(notas_tecnicas_v2)) {
    notas_tecnicas_v2 <-  read_file(
      file = "json_schemas/nota_tecnica_default.json")
  }
  dbWriteTable(
    conn = conn,
    name = "perfiles_notas_tecnicas_v2",
    data.frame(
      "notas_tecnicas" = notas_tecnicas_v2
    )
  )
}

# Se lee RData de departamentos de colombia
departamentos <- readRDS("rds/departamentos_simplificado.rds")

# DT Spanish json
dt_spanish <- "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"
