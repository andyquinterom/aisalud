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
library(leaflet)
library(maps)
library(htmltools)
library(sparklyr)
library(dbplot)
library(jsonlite)
library(shinyAce)
library(listviewer)

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
  bigint = "integer",
  sslmode = "allow")

tabla_perfiles <- dbListTables(conn = conn)

if ("perfiles_usuario" %notin% tabla_perfiles) {
  dbWriteTable(
    conn = conn, 
    name = "perfiles_usuario",
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

if ("perfiles_notas_tecnicas" %notin% tabla_perfiles) {
  dbWriteTable(
    conn = conn,
    name = "perfiles_notas_tecnicas",
    data.frame(
      "notas_tecnicas" = 
'{
  "Ambito": {
      "poblacion": 40000,
      "prestador": "MD&CO Cali",
      "departamento": "Valle del Cauca",
      "cod_departamento": 76,
      "ciudades": "Cali, Palmira",
      "vigente": true,
      "agrupadores": {
          "HOSPITALARIO": {"n": 4230, "cm": 150000},
          "AMBULATORIO": {"n": 504, "cm": 278714},
          "URGENCIAS": {"n": 153, "cm": 42235}
      }
  },
  "Tipo de servicio": {
      "poblacion": 40000,
      "prestador": "MD&CO Bogotá",
      "departamento": "Bogotá D.C",
      "cod_departamento": 11,
      "ciudades": "Bogotá D.C",
      "vigente": false,
      "agrupadores": {
          "APOYO_D": {"n": 110, "cm": 183425},
          "PRC": {"n": 1199, "cm": 85912},
          "TERAPIAS": {"n": 131, "cm": 16823},
          "LABORATORIO": {"n": 322, "cm": 16688},
          "MEDICAMENTOS": {"n": 1534, "cm": 111809},
          "HONORARIOS": {"n": 240, "cm": 46624},
          "PARTO": {"n": 2, "cm": 173236},
          "ESTANCIA": {"n": 224, "cm": 317270},
          "INSUMO": {"n": 858, "cm": 30591},
          "CIRUGIA": {"n": 147, "cm": 1150662},
          "OXIGENO": {"n": 52, "cm": 63063},
          "QUIMIOTERAPIA": {"n": 53, "cm": 579845},
          "BANCO DE SANGRE": {"n": 1, "cm": 373880},
          "RADIOTERAPIA": {"n": 10, "cm": 6049302},
          "TRANSLADOS": {"n": 1, "cm": 102200},
          "AMBULANCIA": {"n": 1, "cm": 230534},
          "CUIDADO DOMICILIARIO": {"n": 2, "cm": 153030}
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
