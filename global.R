# Carga de paquetes y opciones -------------------------------------------------

library(shiny)
library(dbplyr)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(feather)
library(data.table)
library(writexl)
library(ggplot2)
library(varhandle)
library(Cairo)
library(lubridate)
library(stringr)
library(ggiraph)
library(scales)
library(readr)
library(plotly)
library(googlesheets4)
library(DT)
library(markdown)
library(tableHTML)
library(withr)
library(shinydashboardPlus)
library(shinyjqui)
library(googledrive)
library(DBI)
library(RPostgres)
library(dplyr)
library(readxl)
library(shinycssloaders)
library(promises)
library(future)
library(leaflet)
library(maps)
library(htmltools)
library(sparklyr)
library(dbplot)
library(jsonlite)
library(shinyAce)


if (!dir.exists("datos")) {
  dir.create("datos")
}

if (!dir.exists("secrets")) {
  dir.create("secrets")
}

if (!dir.exists(file.path("datos", "saved"))) {
  dir.create(file.path("datos", "saved"))
}

if (!dir.exists(file.path("datos", "nts"))) {
  dir.create(file.path("datos", "nts"))
}


if (Sys.getenv("maxRequestSize") != "") {
  maxRequestSize <- 
    as.numeric(as.character(Sys.getenv("maxRequestSize")))*1024^2
} else {
  maxRequestSize <- 30 * 1024 ^ 3
}
options(shiny.maxRequestSize = maxRequestSize)
options(spinner.color = "#222d32")

# Carga de funciones -----------------------------------------------------------

for (i in paste0("R/", list.files("R/"))) {
  source(i)
}

for (i in paste0("modules/", list.files("modules/"))) {
  source(i)
}

# Carga de datos ---------------------------------------------------------------

conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DATABASE_NAME"),
  user = Sys.getenv("DATABASE_USER"),
  password = Sys.getenv("DATABASE_PW"),
  host = Sys.getenv("DATABASE_HOST"),
  port = Sys.getenv("DATABASE_PORT"),
  options = paste0("-c search_path=", Sys.getenv("DATABASE_SCHEMA")),
  bigint = "integer",
  sslmode = "require")

dbGetQuery(
  conn,
  str_replace_all("SET search_path = public, config, ######;",
                  "######", Sys.getenv("DATABASE_SCHEMA"))
)

tabla_perfiles <- dbGetQuery(
  conn,
  paste0("SELECT table_name FROM information_schema.tables
       WHERE table_schema='config'")) %>%
  unlist() %>%
  unname()

if ("perfiles_usuario" %notin% tabla_perfiles) {
  dbWriteTable(
    conn = conn,
    Id(schema = "config", table = "perfiles_usuario"),
    data.frame(
      "perfiles" = '
      {
      "Perfil de ejemplo": {
          "jerarquia": {
             "episodio": [
               "HOSPITALARIO"
             ],
             "paciente": [
               "AMBULATORIO"
              ]
          }
        }
      }'
    )
  )
}

if ("notas_tecnicas" %notin% tabla_perfiles) {
  dbWriteTable(
    conn = conn,
    Id(schema = "config", table = "notas_tecnicas"),
    data.frame(
      "notas_tecnicas" = 
'{
  "Ambito": {
      "poblacion": 40000,
      "prestador": "MD&CO Cali",
      "departamento": "Valle del Cauca",
      "cod_departamento": 76,
      "ciudades": "Cali, Palmira",
      "vigente": 1,
      "agrupadores": {
          "HOSPITALARIO": [4230, 150000],
          "AMBULATORIO": [504, 278714],
          "URGENCIAS": [153, 42235]
      }
  },
  "Tipo de servicio": {
      "poblacion": 40000,
      "prestador": "MD&CO Bogotá",
      "departamento": "Bogotá D.C",
      "cod_departamento": 11,
      "ciudades": "Bogotá D.C",
      "vigente": 0,
      "agrupadores": {
          "APOYO_D": [110, 183425],
          "PRC": [1199, 85912],
          "TERAPIAS": [131, 16823],
          "LABORATORIO": [322, 16688],
          "MEDICAMENTOS": [1534, 111809],
          "HONORARIOS": [240, 46624],
          "PARTO": [2, 173236],
          "ESTANCIA": [224, 317270],
          "INSUMO": [858, 30591],
          "CIRUGIA": [147, 1150662],
          "OXIGENO": [52, 63063],
          "QUIMIOTERAPIA": [53, 579845],
          "BANCO DE SANGRE": [1, 373880],
          "RADIOTERAPIA": [10, 6049302],
          "TRANSLADOS": [1, 102200],
          "AMBULANCIA": [1, 230534],
          "CUIDADO DOMICILIARIO": [2, 153030]
      }
  }
}'
    )
  )
}

print(dbGetQuery(
  conn,
  "SHOW client_encoding;"
))
