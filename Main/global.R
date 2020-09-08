enableBookmarking(store = "server")

require(googlesheets4)
require(googledrive)
require(data.table)
require(feather)
source("config.R")

googledrive::drive_auth(cache = "token", email = "operaciones@mdco.com.co")
googlesheets4::sheets_auth(cache = "token", email = "operaciones@mdco.com.co")

dir.create("PAQUETES")
dir.create("PRICING")

if (!("PAQUETES.feather" %in% dir(path = "PAQUETES" ,all.files=TRUE) && "REFERENTE-PAQUETES.feather" %in% dir(path = "PAQUETES" ,all.files=TRUE) && "REFERENTE.feather" %in% dir(path = "PAQUETES" ,all.files=TRUE))) {
	write_feather(sheets_read(paquete_path, sheet = "PAQUETES", col_types = "cccdcccccccdd") 
								,"PAQUETES/PAQUETES.feather")
	write_feather(sheets_read(paquete_path, sheet = "REFERENTE-PAQUETES") 
								,"PAQUETES/REFERENTE-PAQUETES.feather")
	write_feather(sheets_read(paquete_path, sheet = "REFERENTE") 
								,"PAQUETES/REFERENTE.feather")
}

pricingList = drive_ls(path = as_id(drive_path))

if (length(dir(path = "PRICING" ,all.files=TRUE)[-which(dir(path = "PRICING" ,all.files=TRUE) %in% c(".", "..", ".DS_Store"))]) == 0) {
	i = 1
	while (i <= length(pricingList$id)) {
		drive_download(file = as_id(pricingList$id[i]), path = paste0("PRICING/", pricingList$name[i]), overwrite = T)
		i = i + 1
	}
	i = NULL
}

PAQUETES = as.data.table(read_feather("PAQUETES/PAQUETES.feather"))
REF_PAQUETES = as.data.table(read_feather("PAQUETES/REFERENTE-PAQUETES.feather"))
REF = as.data.table(read_feather("PAQUETES/REFERENTE.feather"))


# PAQUETES = as.data.table(sheets_read("1MhZBZj9PD-ecAxQixq-FsFfEQdcLHU9sgpMLlpcEYqQ", sheet = "PAQUETES", col_types = "cccccccccccdd"))
# REF = as.data.table(sheets_read("1MhZBZj9PD-ecAxQixq-FsFfEQdcLHU9sgpMLlpcEYqQ", sheet = "REFERENTE"))
# REF_PAQUETES = as.data.table(sheets_read("1MhZBZj9PD-ecAxQixq-FsFfEQdcLHU9sgpMLlpcEYqQ", sheet = "REFERENTE-PAQUETES"))

PAQUETE_PP = PAQUETES[`COMPONENTE` == "PAQUETE"]
PAQUETES_CC = PAQUETES[`COMPONENTE` != "PAQUETE"]

