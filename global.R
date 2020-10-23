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

# Carga de datos ---------------------------------------------------------------

if (file.exists("datos/PAQUETES/PAQUETES.feather") &&
    file.exists("datos/PAQUETES/REFERENTE-PAQUETES.feather") && 
    file.exists("datos/PAQUETES/REFERENTE.feather")) {
  
  PAQUETES <- 
    as.data.table(read_feather("datos/PAQUETES/PAQUETES.feather"))
  
  REF_PAQUETES <- 
    as.data.table(read_feather("datos/PAQUETES/REFERENTE-PAQUETES.feather"))
  
  REF <- 
    as.data.table(read_feather("datos/PAQUETES/REFERENTE.feather"))
  
  PAQUETE_PP <- 
    PAQUETES[`COMPONENTE` == "PAQUETE"]
  
  PAQUETES_CC <- 
    PAQUETES[`COMPONENTE` != "PAQUETE"]
  
}

# Authentication google -------------------------------------------------------
googledrive::drive_auth(path = "secrets/serviceAccount.json")
googlesheets4::gs4_auth(path = "secrets/serviceAccount.json")



if (!dir.exists("datos/PAQUETES")) {
  dir.create("datos/PAQUETES")
}

if (!dir.exists("datos/PAQUETES")) {
  dir.create("datos/PRICING")
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
  
  if (!(file.exists("datos/PAQUETES/PAQUETES.feather") &&
        file.exists("datos/PAQUETES/REFERENTE-PAQUETES.feather") && 
        file.exists("datos/PAQUETES/REFERENTE.feather"))) {
    
    write_feather(sheets_read(paquete_path, sheet = "PAQUETES",
                              col_types = "cccdcccccccdd"), 
                  path = "datos/PAQUETES/PAQUETES.feather")
    
    write_feather(sheets_read(paquete_path, sheet = "REFERENTE-PAQUETES"),
                  path = "datos/PAQUETES/REFERENTE-PAQUETES.feather")
    
    write_feather(sheets_read(paquete_path, sheet = "REFERENTE"),
                  path = "datos/PAQUETES/REFERENTE.feather")
    
  }
  
  
  PAQUETES <-
    as.data.table(read_feather("datos/PAQUETES/PAQUETES.feather"))
  
  REF_PAQUETES <-
    as.data.table(read_feather("datos/PAQUETES/REFERENTE-PAQUETES.feather"))
  
  REF <-
    as.data.table(read_feather("datos/PAQUETES/REFERENTE.feather"))

  PAQUETE_PP <- PAQUETES[`COMPONENTE` == "PAQUETE"]
  PAQUETES_CC <- PAQUETES[`COMPONENTE` != "PAQUETE"]
}

# Pricing  -------------------------------------------------------------------- 

if (Sys.getenv("PRICING_INCLUIDO") == "") {
  
  PRICING_INCLUIDO <- FALSE
  
} else {
  
  PRICING_INCLUIDO <- TRUE
  
  pricingList <- drive_ls(path = as_id(pricing_path))
  
  archivos_pricing <- 
    length(dir(path = "datos/PRICING",
               all.files = TRUE
    )[-which(dir(path = "datos/PRICING", 
                 all.files = TRUE) %in% c(".", "..", ".DS_Store")
    )
    ]
    )
  
  if (archivos_pricing == 0) {
    for (i in 1:length(pricingList$id)) {
      drive_download(file = as_id(pricingList$id[i]),
                     path = paste0("datos/PRICING/", pricingList$name[i]),
                     overwrite = T)
    }
  }
  
}

# Notas técnicas  ------------------------------------------------------------- 

if (Sys.getenv("NT_INCLUIDO") == "") {
  
  NT_INCLUIDO <- FALSE
  
} else {
  
  NT_INCLUIDO <- TRUE
  
  if (!(file.exists("datos/NTs/NTs.feather") &&
        file.exists("datos/NTs/INDICE.feather") && 
        file.exists("datos/NTs/INCLUSIONES.feather") &&
        file.exists("datos/NTs/NTmapa.rds"))) {
    
    write_feather(sheets_read(nts_path,
                              sheet = "NTs",
                              col_types = "ccddd"),
                  "datos/NTs/NTs.feather")
    
    write_feather(sheets_read(nts_path, 
                              sheet = "INDICE", 
                              col_types = "ccdcccd"),
                  "datos/NTs/INDICE.feather")
    
    write_feather(sheets_read(nts_path, 
                              sheet = "INCLUSIONES", 
                              col_types = "ccdc") ,
                  "datos/NTs/INCLUSIONES.feather")
    
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
      "datos/NTmapa.rds")
    
  }
  
}

MAPA_NT <- readRDS("datos/NTs/NTmapa.rds")
NTs_INDICE <- as.data.table(read_feather("datos/NTs/INDICE.feather"))
NTs_INCLUSIONES <- as.data.table(read_feather("datos/NTs/INCLUSIONES.feather"))
NTs_NT <- as.data.table(read_feather("datos/NTs/NTs.feather"))
NTs_UniqueCod <- unique(NTs_INDICE$COD_NT)