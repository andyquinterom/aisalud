# Carga de paquetes y opciones -------------------------------------------------

library(shiny)
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
library(colmaps2)
library(maps)
library(withr)
library(shinydashboardPlus)
library(shinyjqui)
library(googledrive)
library(DBI)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(readxl)
library(shinycssloaders)

enableBookmarking(store = "server")


if (Sys.getenv("maxRequestSize") != "") {
  maxRequestSize <- 
    as.numeric(as.character(Sys.getenv("maxRequestSize")))*1024^2
} else {
  maxRequestSize <- 30 * 1024 ^ 3
}
options(shiny.maxRequestSize = maxRequestSize)
options(spinner.color = "#222d32")

# Carga de funciones -----------------------------------------------------------

source("config.R")

for (i in paste0("R/", list.files("R/"))) {
  source(i)
}

for (i in paste0("modules/", list.files("modules/"))) {
  source(i)
}

# Carga de datos ---------------------------------------------------------------

# Authentication google -------------------------------------------------------
googledrive::drive_auth(path = "secrets/serviceAccount.json")
googlesheets4::gs4_auth(path = "secrets/serviceAccount.json")



if (!dir.exists("datos/paquetes")) {
  dir.create("datos/paquetes")
}

if (!dir.exists("datos/pricing")) {
  dir.create("datos/pricing")
}

if (!dir.exists("datos/nts")) {
  dir.create("datos/nts")
}



if (Sys.getenv("pricing_path") == "") {
  
  pricing_path <- "1qUG1yQpF5vPDGE4KUDOK5lx8S7ZoRiJP"
  
} else {
  
  pricing_path <- Sys.getenv("pricing_path")
  
}

if (Sys.getenv("paquete_path") == "") {
  
  paquete_path <- "1xR5w_c8puXRqqMtPIphRUogF7q5AAeExUrCb6U5s4i8"
  
} else {
  
  paquete_path <- Sys.getenv("paquete_path")
  
}

if (Sys.getenv("nts_path") == "") {
  
  nts_path <- "1hmVLybaBfgJvmXlUNRp0_0eygKG7mRtxmQNH5VGsr00"
  
} else {
  
  nts_path <- Sys.getenv("nts_path")
  
}

# Paquetes -------------------------------------------------------------------- 

if (Sys.getenv("PAQUETES_INCLUIDO") == "") {
  
  PAQUETES_INCLUIDO <- FALSE
  
} else {
  
  PAQUETES_INCLUIDO <- TRUE
  
  if (!(file.exists("datos/paquetes/paquetes.feather") &&
        file.exists("datos/paquetes/referente-paquetes.feather") && 
        file.exists("datos/paquetes/referente.feather"))) {
    
    write_feather(sheets_read(paquete_path, sheet = "paquetes",
                              col_types = "cccdcccccccdd"), 
                  path = "datos/paquetes/paquetes.feather")
    
    write_feather(sheets_read(paquete_path, sheet = "referente_paquetes"),
                  path = "datos/paquetes/referente-paquetes.feather")
    
    write_feather(sheets_read(paquete_path, sheet = "referente"),
                  path = "datos/paquetes/referente.feather")
    
  }
  
  
  paquetes <- 
    as.data.table(read_feather("datos/paquetes/paquetes.feather"))
  
  paquetes_ref <- 
    as.data.table(read_feather("datos/paquetes/referente-paquetes.feather"))
  
  paquetes_ref_cups <- 
    as.data.table(read_feather("datos/paquetes/referente.feather"))
  
  paquetes_paquetes <- 
    paquetes[componente == "PAQUETE"]
  
  paquetes_cups <- 
    paquetes[componente != "PAQUETE"]
}

# Pricing  -------------------------------------------------------------------- 

if (Sys.getenv("PRICING_INCLUIDO") == "") {
  
  PRICING_INCLUIDO <- FALSE
  
} else {
  
  PRICING_INCLUIDO <- TRUE
  
  pricingList <- drive_ls(path = as_id(pricing_path))
  
  archivos_pricing <- 
    length(dir(path = "datos/pricing",
               all.files = TRUE
    )[-which(dir(path = "datos/pricing", 
                 all.files = TRUE) %in% c(".", "..", ".DS_Store")
    )
    ]
    )
  
  if (archivos_pricing == 0) {
    for (i in 1:length(pricingList$id)) {
      drive_download(file = as_id(pricingList$id[i]),
                     path = paste0("datos/pricing/", pricingList$name[i]),
                     overwrite = T)
    }
  }
  
}

# Notas técnicas  ------------------------------------------------------------- 

if (Sys.getenv("NT_INCLUIDO") == "") {
  
  NT_INCLUIDO <- FALSE
  
} else {
  
  NT_INCLUIDO <- TRUE
  
  if (!(file.exists("datos/nts/notas_tecnicas.feather") &&
        file.exists("datos/nts/indice.feather") && 
        file.exists("datos/nts/inclusiones.feather") &&
        file.exists("datos/nts/nt_mapa.rds"))) {
    
    write_feather(sheets_read(nts_path,
                              sheet = "notas_tecnicas",
                              col_types = "ccddd"),
                  "datos/nts/notas_tecnicas.feather")
    
    write_feather(sheets_read(nts_path, 
                              sheet = "indice", 
                              col_types = "ccdcccd"),
                  "datos/nts/indice.feather")
    
    write_feather(sheets_read(nts_path, 
                              sheet = "inclusiones", 
                              col_types = "ccdc") ,
                  "datos/nts/inclusiones.feather")
    
    saveRDS(
      mapaValoresNT(
        as.data.table(
          sheets_read(nts_path,
                      sheet = "indice",
                      col_types = "ccdcccd"
          )
        )
      ) %>% 
        layout(autosize = TRUE),
      "datos/nts/nt_mapa.rds")
    
  }
  
  dash_nt_mapa <- readRDS("datos/nts/nt_mapa.rds")
  dash_nt_indice <- as.data.table(read_feather("datos/nts/indice.feather"))
  dash_nt_inclusiones <- as.data.table(
    read_feather("datos/nts/inclusiones.feather"))
  dash_nt_datos <- as.data.table(
    read_feather("datos/nts/notas_tecnicas.feather"))
  dash_nt_codigos <- unique(dash_nt_datos$COD_NT)
  
}

