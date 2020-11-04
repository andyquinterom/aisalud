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




enableBookmarking(store = "server")

require(googlesheets4)
require(googledrive)
require(data.table)
require(feather)

if (Sys.getenv("maxRequestSize") != "") {
  maxRequestSize <- 
    as.numeric(as.character(Sys.getenv("maxRequestSize")))*1024^2
} else {
  maxRequestSize <- 30 * 1024 ^ 3
}
options(shiny.maxRequestSize = maxRequestSize)

# Carga de funciones -----------------------------------------------------------

source("config.R")

for (i in paste0("source/", list.files("source/"))) {
  source(i)
}

for (i in paste0("modules/", list.files("modules/"))) {
  source(i)
}

# Carga de datos ---------------------------------------------------------------

if (file.exists("datos/paquetes/paquetes.feather") &&
    file.exists("datos/paquetes/referente-paquetes.feather") && 
    file.exists("datos/paquetes/referente.feather")) {
  
  paquetes <- 
    as.data.table(read_feather("datos/paquetes/paquetes.feather"))
  
  paquetes_ref_cups <- 
    as.data.table(read_feather("datos/paquetes/referente-paquetes.feather"))
  
  paquetes_ref <- 
    as.data.table(read_feather("datos/paquetes/referente.feather"))
  
  paquetes_paquetes <- 
    paquetes[`COMPONENTE` == "PAQUETE"]
  
  paquetes_cups <- 
    paquetes[`COMPONENTE` != "PAQUETE"]
  
}

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
  
  paquete_path <- "1-_uwyGQspW--DhG6PAC4DUi6NTjMzGqy5ldzhcvkO0U"
  
} else {
  
  paquete_path <- Sys.getenv("paquete_path")
  
}

if (Sys.getenv("nts_path") == "") {
  
  nts_path <- "1zTmNGV5mgFqvNJK-g68D9sBTOTmhYBTvz8ECuDuSjmU"
  
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
    
    write_feather(sheets_read(paquete_path, sheet = "PAQUETES",
                              col_types = "cccdcccccccdd"), 
                  path = "datos/paquetes/paquetes.feather")
    
    write_feather(sheets_read(paquete_path, sheet = "REFERENTE-PAQUETES"),
                  path = "datos/paquetes/referente-paquetes.feather")
    
    write_feather(sheets_read(paquete_path, sheet = "REFERENTE"),
                  path = "datos/paquetes/referente.feather")
    
  }
  
  
  paquetes <- 
    as.data.table(read_feather("datos/paquetes/paquetes.feather"))
  
  paquetes_ref_cups <- 
    as.data.table(read_feather("datos/paquetes/referente-paquetes.feather"))
  
  paquetes_ref <- 
    as.data.table(read_feather("datos/paquetes/referente.feather"))
  
  paquetes_paquetes <- 
    paquetes[`COMPONENTE` == "PAQUETE"]
  
  paquetes_cups <- 
    paquetes[`COMPONENTE` != "PAQUETE"]
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
                              sheet = "NTs",
                              col_types = "ccddd"),
                  "datos/nts/notas_tecnicas.feather")
    
    write_feather(sheets_read(nts_path, 
                              sheet = "INDICE", 
                              col_types = "ccdcccd"),
                  "datos/nts/indice.feather")
    
    write_feather(sheets_read(nts_path, 
                              sheet = "INCLUSIONES", 
                              col_types = "ccdc") ,
                  "datos/nts/inclusiones.feather")
    
    saveRDS(
      mapaValoresNT(
        as.data.table(
          sheets_read(nts_path,
                      sheet = "INDICE",
                      col_types = "ccdcccd"
          )
        )
      ) %>% 
        layout(autosize = TRUE),
      "datos/nts/nt_mapa.rds")
    
  }
  
}

dash_nt_mapa <- readRDS("datos/nts/nt_mapa.rds")
dash_nt_indice <- as.data.table(read_feather("datos/nts/indice.feather"))
dash_nt_inclusiones <- as.data.table(
  read_feather("datos/nts/inclusiones.feather"))
dash_nt_datos <- as.data.table(read_feather("datos/nts/notas_tecnicas.feather"))
dash_nt_codigos <- unique(dash_nt_datos$COD_NT)