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


dir.create("datos")
dir.create("secrets")
dir.create(file.path("datos", "saved"))
dir.create(file.path("datos", "nts"))


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

# Authentication google -------------------------------------------------------

if (Sys.getenv("SERVICE_ACCOUNT") != "") {
  if (file.exists(file.path("secrets", "serviceAccount.json"))) {
    file.remove(file.path("secrets", "serviceAccount.json"))
  }
  write_lines(
    x = Sys.getenv("SERVICE_ACCOUNT"),
    path = file.path("secrets", "serviceAccount.json")
  )
  googledrive::drive_auth(path = "secrets/serviceAccount.json")
  googlesheets4::gs4_auth(path = "secrets/serviceAccount.json")
}

if (!file.exists(file.path("datos", "saved", "oncologia.feather"))) {
  googledrive::drive_download(
    file = as_id("1h6T9p3Di5vNmL2wPkFq5HzR8sAdun7ef"),
    path = file.path("datos", "saved", "oncologia.feather"))
}

if (Sys.getenv("NTS_PATH") == "") {
  
  nts_path <- "1hmVLybaBfgJvmXlUNRp0_0eygKG7mRtxmQNH5VGsr00"
  
} else {
  
  nts_path <- Sys.getenv("NTS_PATH")
  
}

# Notas técnicas  ------------------------------------------------------------- 

if (Sys.getenv("NTS_INCLUIDO") == "") {
  
  NTS_INCLUIDO <- FALSE
  
} else {
  
  NTS_INCLUIDO <- TRUE
  
  dash_nt_indice <- as.data.table(sheets_read(nts_path, 
                                              sheet = "indice", 
                                              col_types = "ccdcccd"))
  dash_nt_inclusiones <- as.data.table(sheets_read(nts_path, 
                                                   sheet = "inclusiones", 
                                                   col_types = "ccdc"))
  dash_nt_datos <- as.data.table(sheets_read(nts_path,
                                             sheet = "notas_tecnicas",
                                             col_types = "ccddd"))
  dash_nt_codigos <- unique(dash_nt_datos$COD_NT)
  
}


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

print(dbGetQuery(
  conn,
  "SHOW client_encoding;"
))
