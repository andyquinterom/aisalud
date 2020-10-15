enableBookmarking(store = "server")

require(colmaps2)
require(googlesheets4)
require(googledrive)
require(data.table)
require(feather)
source("config.R")

googledrive::drive_auth(cache = "token", email = "operaciones@mdco.com.co")
googlesheets4::sheets_auth(cache = "token", email = "operaciones@mdco.com.co")

dir.create("PAQUETES")
dir.create("PRICING")

if (Sys.getenv("maxRequestSize") != "") {
	maxRequestSize = as.numeric(as.character(Sys.getenv("maxRequestSize")))*1024^2
} else {
	maxRequestSize = 30*1024^3
}

options(shiny.maxRequestSize=maxRequestSize)


if (Sys.getenv("pricing_path") == "") {
	pricing_path = "1qUG1yQpF5vPDGE4KUDOK5lx8S7ZoRiJP"
} else {
	pricing_path = Sys.getenv("pricing_path")
}

if (Sys.getenv("paquete_path") == "") {
	paquete_path = "1-_uwyGQspW--DhG6PAC4DUi6NTjMzGqy5ldzhcvkO0U"
} else {
	paquete_path = Sys.getenv("paquete_path")
}

if (Sys.getenv("nts_path") == "") {
	nts_path = "1zTmNGV5mgFqvNJK-g68D9sBTOTmhYBTvz8ECuDuSjmU"
} else {
	nts_path = Sys.getenv("nts_path")
}

###Paquetes

if (Sys.getenv("PAQUETES_INCLUIDO") == "") {
	PAQUETES_INCLUIDO = FALSE
} else {
	PAQUETES_INCLUIDO = TRUE
	if (!("PAQUETES.feather" %in% dir(path = "PAQUETES" ,all.files=TRUE) && "REFERENTE-PAQUETES.feather" %in% dir(path = "PAQUETES" ,all.files=TRUE) && "REFERENTE.feather" %in% dir(path = "PAQUETES" ,all.files=TRUE))) {
		write_feather(sheets_read(paquete_path, sheet = "PAQUETES", col_types = "cccdcccccccdd") 
									,"PAQUETES/PAQUETES.feather")
		write_feather(sheets_read(paquete_path, sheet = "REFERENTE-PAQUETES") 
									,"PAQUETES/REFERENTE-PAQUETES.feather")
		write_feather(sheets_read(paquete_path, sheet = "REFERENTE") 
									,"PAQUETES/REFERENTE.feather")
	}
	
	
	PAQUETES = as.data.table(read_feather("PAQUETES/PAQUETES.feather"))
	REF_PAQUETES = as.data.table(read_feather("PAQUETES/REFERENTE-PAQUETES.feather"))
	REF = as.data.table(read_feather("PAQUETES/REFERENTE.feather"))
	
	PAQUETE_PP = PAQUETES[`COMPONENTE` == "PAQUETE"]
	PAQUETES_CC = PAQUETES[`COMPONENTE` != "PAQUETE"]
}

###Pricing

if (Sys.getenv("PRICING_INCLUIDO") == "") {
	PRICING_INCLUIDO = FALSE
} else {
	PRICING_INCLUIDO = TRUE
	
	
	if (length(dir(path = "PRICING" ,all.files=TRUE)[-which(dir(path = "PRICING" ,all.files=TRUE) %in% c(".", "..", ".DS_Store"))]) == 0) {
		pricingList = drive_ls(path = as_id(pricing_path))
		i = 1
		while (i <= length(pricingList$id)) {
			drive_download(file = as_id(pricingList$id[i]), path = paste0("PRICING/", pricingList$name[i]), overwrite = T)
			i = i + 1
		}
		i = NULL
	}
	
}

###Notas tecnicas

if (Sys.getenv("NT_INCLUIDO") == "") {
	NT_INCLUIDO = FALSE
} else {
	NT_INCLUIDO = TRUE
	
	if (!(all(c("NTs.feather", "INDICE.feather", "NTmapa.rds", "INCLUSIONES.feather") %in% dir(path = "NTs" ,all.files=TRUE)))) {
		write_feather(sheets_read(nts_path, sheet = "NTs", col_types = "ccddd") 
									,"NTs/NTs.feather")
		write_feather(sheets_read(nts_path, sheet = "INDICE", col_types = "ccdcccd") 
									,"NTs/INDICE.feather")
		write_feather(sheets_read(nts_path, sheet = "INCLUSIONES", col_types = "ccdc") 
									,"NTs/INCLUSIONES.feather")
		saveRDS(mapaValoresNT(as.data.table(sheets_read(nts_path, sheet = "INDICE", col_types = "ccdcccd"))) %>% layout(autosize = TRUE), "NTs/NTmapa.rds")
	}
	
}

MAPA_NT = readRDS("NTs/NTmapa.rds")
NTs_INDICE = as.data.table(read_feather("NTs/INDICE.feather"))
NTs_INCLUSIONES = as.data.table(read_feather("NTs/INCLUSIONES.feather"))
NTs_NT = as.data.table(read_feather("NTs/NTs.feather"))

NTs_UniqueCod = unique(NTs_INDICE$COD_NT)

