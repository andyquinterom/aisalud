#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(stringr)
library(data.table)
library(ggiraph)
library(plotly)
library(markdown)
library(tableHTML)

if ("PAQUETES.feather" %in% dir(path = "PAQUETES" ,all.files=TRUE) && "REFERENTE-PAQUETES.feather" %in% dir(path = "PAQUETES" ,all.files=TRUE) && "REFERENTE.feather" %in% dir(path = "PAQUETES" ,all.files=TRUE)) {
	PAQUETES = as.data.table(read_feather("PAQUETES/PAQUETES.feather"))
	REF_PAQUETES = as.data.table(read_feather("PAQUETES/REFERENTE-PAQUETES.feather"))
	REF = as.data.table(read_feather("PAQUETES/REFERENTE.feather"))
	PAQUETE_PP = PAQUETES[`COMPONENTE` == "PAQUETE"]
	PAQUETES_CC = PAQUETES[`COMPONENTE` != "PAQUETE"]
}

shinyUI(
	function(request) {
	tagList(
			tags$head(
				tags$script(type="text/javascript", src = "code.js"),
				tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
				includeHTML("googleanalytics.html"),
				tags$script(HTML(
				"$(document).one('shiny:idle', 
			      function() {
			        ga('set','userId', Shiny.user);
			        ga('send', 'pageview');
			      }
			     );"
				)),
				tags$script(HTML(
					"$(document).on('shiny:inputchanged', function(event) {
			         ga('send','event', 'input', 
			            'updates', event.name, event.value);
			     });"
				))
				# tags$style(make_css(list('.btn', 'white-space', 'pre-wrap')))
				),
	    dashboardPage(skin = "black",
	    					dashboardHeader(title = "Salud - Analítica"
	    													, tags$li(class = "dropdown", actionLink("home", label = "", icon = icon("home", lib="font-awesome"), onclick ="location.href='https://users.indexmic.com/menu';"))),
	    					dashboardSidebar(
	    						sidebarMenu(
	    							HTML('
	    									 <img src="logoblanco.png" width="100%"/>
	    									 '),
	    							menuItem(
	    								  text = "Opciones"
	    								, icon = icon("cog", lib = "font-awesome")
	    								, tabName = "opciones"
	    							),
	    							menuItem(
	    								  text = "Descriptiva"
	    								, icon = icon("table", lib = "font-awesome")
	    								, tabName = "descriptiva"
	    							),
	    							menuItem(
	    								  text = "Pacientes Outliers"
	    								, icon = icon("search-minus", lib = "font-awesome")
	    								, tabName = "outliers"
	    							),
	    							menuItem(
	    								  text = "Gráficos"
	    								, icon = icon("chart-area", lib = "font-awesome")
	    								, tabName = "graficos"
	    								, menuSubItem(  text = "Histogramas y barras"
	    															, tabName = "histogramasbarras")
	    								, menuSubItem(  text = "Caja de bigotes"
	    																, tabName = "cajadebigotes")
	    								, menuSubItem(  text = "Linea de tiempo"
	    																, tabName = "lineadetiempo")
	    							),
	    							menuItem(
	    								  text = "Nota técnica"
	    								, icon = icon("search-dollar", lib = "font-awesome")
	    								, tabName = "notatecnica"
	    							),
	    							menuItem(
	    								  text = "Paquetes - Episodio"
	    								, icon = icon("chart-pie", lib = "font-awesome")
	    								, tabName = "paquetes"
	    								, radioButtons("DASHBOARD_valorcosto", "Graficar:", choices = c("VALOR", "COSTO"))
	    								, actionButton("DASHBOARD_actualizar", "Actualizar")
	    								, menuSubItem(  text = "Índice"
	    															, tabName = "indicepaquete")
	    								, menuSubItem(  text = "Dashboard"
	    															, tabName = "dashboardpaquete")
	    								, tags$br()
	    							),
	    							menuItem(
	    							  	text = "Pricing"
	    								, icon = icon("tags", lib = "font-awesome")
	    								, tabName = "pricingOpciones"
	    								, tags$br()
	    								, actionButton("pricingActualizar", "Actualizar")
	    								, menuSubItem(  text = "Informes"
	    																, tabName = "pricing")
	    								, tags$br()
	    							),
	    							bookmarkButton("Bookmark", width = "85%")
	    						)
	    					),
	    					dashboardBody(
	    						tabItems(
	    							tabItem(tabName = "opciones",
	    											chooseSliderSkin("Square"),
	    											fluidRow(
	    												column(5,
	    															 fileInput("file", label = "Base de datos", accept = c(".feather", ".rds"), buttonLabel = "Subir", placeholder = "Ningún archivo selecionado")
	    												)
	    											),
	    											fluidRow(
	    												column(2,
	    															 dateInput("fechamin", "Fecha Inicial:", value = NULL, min = NULL, max = NULL, format = "dd-mm-yyyy", language = "es"),
	    															 dateInput("fechamax", "Fecha Final:", value = NULL, min = NULL, max = NULL, format = "dd-mm-yyyy", language = "es"),
	    															 tags$style(HTML(".datepicker {z-index:99999 !important;}")),
	    															 textInput("formatoFecha", label = "Formato de Fecha", value = "%d/%m/%Y"),
	    															 actionButton("exeOpciones", "Aplicar")	    												),
	    												column(3,
	    															 # textInput("colDX", label = "Columna de diagnostico", value = "CODDXPPAL"),
	    															 radioButtons("valorCosto", "Calcular estadísticas por:",
	    															 						 choiceNames = list(
	    															 						 	"VALOR","COSTO"
	    															 						 ),
	    															 						 choiceValues = list(
	    															 						 	"VALOR", "COSTO"
	    															 						 ), inline = TRUE, width='75%'),
	    															 radioButtons("pacientesPrestaciones", "Calcular estadísticas por:",
	    															 						 choiceNames = list(
	    															 						 	"PACIENTE","PRESTACIÓN"
	    															 						 ),
	    															 						 choiceValues = list(
	    															 						 	"FALSE", "TRUE"
	    															 						 ), inline = TRUE, width='75%')
	    												),
	    												column(6,
	    															 tags$h3("Filtros:"),
	    															 fluidRow(
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltro1", choices = c("NA"), width = "100%")
	    															 				 ),
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltroVAL1", choices = c("NA"), multiple = TRUE, width = "100%",
	    															 				 						options = list(
	    															 				 							`actions-box` = TRUE, `deselect-all-text` = "Deseleccionar todos",
	    															 				 							`select-all-text` = "Seleccionar todos",
	    															 				 							`live-search` = TRUE))
	    															 				 )
	    															 ),
	    															 fluidRow(
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltro2", choices = c("NA"), width = "100%")
	    															 	),
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltroVAL2", choices = c("NA"), multiple = TRUE, width = "100%",
	    															 				 						options = list(
	    															 				 							`actions-box` = TRUE, `deselect-all-text` = "Deseleccionar todos",
	    															 				 							`select-all-text` = "Seleccionar todos",
	    															 				 							`live-search` = TRUE))
	    															 	)
	    															 ),
	    															 fluidRow(
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltro3", choices = c("NA"), width = "100%")
	    															 	),
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltroVAL3", choices = c("NA"), multiple = TRUE, width = "100%",
	    															 				 						options = list(
	    															 				 							`actions-box` = TRUE, `deselect-all-text` = "Deseleccionar todos",
	    															 				 							`select-all-text` = "Seleccionar todos",
	    															 				 							`live-search` = TRUE))
	    															 	)
	    															 ),
	    															 fluidRow(
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltro4", choices = c("NA"), width = "100%")
	    															 	),
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltroVAL4", choices = c("NA"), multiple = TRUE, width = "100%",
	    															 				 						options = list(
	    															 				 							`actions-box` = TRUE, `deselect-all-text` = "Deseleccionar todos",
	    															 				 							`select-all-text` = "Seleccionar todos",
	    															 				 							`live-search` = TRUE))
	    															 	)
	    															 ),
	    															 fluidRow(
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltro5", choices = c("NA"), width = "100%")
	    															 	),
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltroVAL5", choices = c("NA"), multiple = TRUE, width = "100%",
	    															 				 						options = list(
	    															 				 							`actions-box` = TRUE, `deselect-all-text` = "Deseleccionar todos",
	    															 				 							`select-all-text` = "Seleccionar todos",
	    															 				 							`live-search` = TRUE))
	    															 	)
	    															 ),
	    															 fluidRow(
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltroNum1", choices = c("NA"), width = "100%")
	    															 	),
	    															 	column(width = 3,
	    															 				 numericInput("bdFiltroNum1VAL1", NULL, 0, min = 0, max = 0, width = "100%")
	    															 	),
	    															 	column(width = 3,
	    															 				 numericInput("bdFiltroNum1VAL2", NULL, 0, min = 0, max = 0, width = "100%")
	    															 	)
	    															 ),
	    															 fluidRow(
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltroNum2", choices = c("NA"), width = "100%")
	    															 	),
	    															 	column(width = 3,
	    															 				 numericInput("bdFiltroNum2VAL1", NULL, 0, min = 0, max = 0, width = "100%")
	    															 	),
	    															 	column(width = 3,
	    															 				 numericInput("bdFiltroNum2VAL2", NULL, 0, min = 0, max = 0, width = "100%")
	    															 	)
	    															 ),
	    															 fluidRow(
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltroLong1", choices = c("NA"), width = "100%")
	    															 	),
	    															 	column(width = 5,
	    															 				 textInput("bdFiltroLongVAL1", width = "100%", label = NULL)
	    															 	),
	    															 	column(width = 1,
	    															 				 checkboxInput("bdFiltroLongEx1", label = NULL)
	    															 				 )
	    															 ),
	    															 fluidRow(
	    															 	column(width = 6,
	    															 				 pickerInput("bdFiltroLong2", choices = c("NA"), width = "100%")
	    															 	),
	    															 	column(width = 5,
	    															 				 textInput("bdFiltroLongVAL2", width = "100%", label = NULL)
	    															 	),
	    															 	column(width = 1,
	    															 				 checkboxInput("bdFiltroLongEx2", label = NULL)
	    															 	)
	    															 ),
	    															 fluidRow(
	    															 	column(width = 12,
	    															 				 actionButton("aplicarFiltros", "Aplicar Filtros")
	    															 	)
	    															 ),
	    															 tags$style(HTML(".dropdown-menu {z-index:99999 !important;}"))
	    															 )
	    											),
	    											br(),
	    											fluidRow(
	    												column(width = 5,
	    															 tags$div()
	    												),
	    												column(width = 6,
	    															 box(width = "100%",
		    															 fluidRow(
		    															 	column(width=2,
		    															 				 tags$h3("Preview:")
		    															 	),
		    															 	column(width = 10,
		    															 				 br(),
		    															 				 tags$a("Si se genera un error, el archivo no es feather o tu base de datos no contiene las columnas: 
		              				 						 						 			 NRO_IDENTIFICACION, FECHA_PRESTACION, VALOR o COSTO."
		    															 				 			 , style = "color: black;")
		    															 	)
		    															 ),
		    															 DT::dataTableOutput("preview", width = "100%")
	    												)
	    												)
	    											)
	    							),
	    							tabItem(tabName = "descriptiva",
	    											fluidRow(
	    												column(width = 3,
	    													box(width = "100%",
	    															 pickerInput("descriptivaColumna", label = "Agrupar por:",choices = c("NA"),multiple = TRUE,
	    															 						options = list(
	    															 							`actions-box` = TRUE, `deselect-all-text` = "Deseleccionar todos",
	    															 							`select-all-text` = "Seleccionar todos",
	    															 							`live-search` = TRUE)
	    															 ),
	    															 sliderInput("filtroCoeficiente", label = "Coeficiente de variacion:", min = 0, max = 10, value = c(0,1)),
	    															 checkboxInput("filtroNULL", "Mostrar todos los valores", value = FALSE),
	    															 actionButton("exeOpcionesDescriptiva", "Confirmar"),
	    															 tags$br(),
	    															 tags$br(),
	    															 downloadButton("descargaDescriptivaCSV", "CSV", style = "width:100%;"),
	    															 tags$br(),
	    															 tags$br(),
	    															 downloadButton("descargaDescriptivaEXCEL", "Excel", style = "width:100%;"),
	    															 br(),
	    															 br(),
	    															 textOutput("sumasDescriptivaRegistros"),
	    															 textOutput("sumasDescriptivaPacientes"),
	    															 textOutput("sumasDescriptivaValorCosto"),
	    															 textOutput("sumasDescriptivaCUPS")
	    													)
	    												),
	    												column(width = 9,
	    															 box(width = "100%",
	    															 	div(DT::dataTableOutput("descriptivaFinal"), style = "font-size:90%")
	    															 )
	    												)
	    												
	    											)
	    							),
	    							tabItem(tabName = "outliers",
	    											fluidRow(
	    												column(width = 3,
	    															 box(width = "100%",
		    															 pickerInput("outliersColumna", label = "Agrupar por:",choices = c("NA"),
		    															 						options = list(
		    															 							`actions-box` = TRUE,
		    															 							`live-search` = TRUE)),
		    															 sliderInput("outliersPercentil", label = "Outliers por percentil:", min = 0.75, max = 0.99, value = c(0.9)),
		    															 radioButtons("outliersIQR", "Outliers por IQR",
		    															 						 choiceNames = list(
		    															 						 	"1.5","3.0"
		    															 						 ),
		    															 						 choiceValues = list(
		    															 						 	"1.5", "3.0"
		    															 						 ), inline = TRUE, width='75%'),
		    															 numericInput("outliersFiltroFrecuencia", label = "Frecuencia Mínima", min = 0, step = 1, value = 0),
		    															 actionButton("exeOpcionesOutliersPercentil", "Calcular por percentil", width = "100%"),
		    															 actionButton("exeOpcionesOutliersIQR", "Calcular por IQR", width = "100%"),
		    															 tags$br(),
		    															 tags$br(),
		    															 downloadButton("descargaOutliersCSV", "CSV", style = "width:100%;"),
		    															 tags$br(),
		    															 tags$br(),
		    															 downloadButton("descargaOutliersEXCEL", "Excel", style = "width:100%;")
	    															 )
	    												),
	    												column(width = 9,
	    															 box(width = "100%",
	    															 		div(DT::dataTableOutput("outliersFinal"), style = "font-size:90%")
	    															 )
	    												)
	    												
	    											)
	    							),
	    							tabItem(tabName = "histogramasbarras",
	    											fluidRow(
	    												column(width = 3,
	    															 box(width = "100%",
		    															 pickerInput("histogramaColumna", "Columna:", choices = "NA",
		    															 						options = list(
		    															 							`actions-box` = TRUE,
		    															 							`live-search` = TRUE)),
		    															 pickerInput("histogramaRelleno", "Relleno:", choices = "NA",
		    															 						options = list(
		    															 							`actions-box` = TRUE,
		    															 							`live-search` = TRUE)),
		    															 numericInput("histogramaFiltroX1", "X Min", value = 0),
		    															 numericInput("histogramaFiltroX2", "X Max", value = 1),
		    															 numericInput("histogramabinWidth", "Ancho del bin", value = 1),
		    															 numericInput("histogramaNumBins", "· de bins", value = 10),
		    															 actionButton("exeOpcionesHistograma", "Confirmar"),
		    															 br(), br(),
		    															 tags$p("Los valores aqui exhibidos pueden cambiar radicalmente 
		                                                    el rendimiento de la aplicación. Es recomendado tener 
		                                                    un numero bajo de bins y un mínimo y máximo realista.
		                                                    Para un número de bins ideal, se recomiendo utilizar 
		                                                    3.3*log(n) donde n es el numero de registros.")
	    															 )
	    												),
	    												column(width = 9,
	    															 box(width = "100%",
	    															 		plotlyOutput("histogramaFinal", height = "800px")
	    															 )
	    												)
	    											)
	    											),
	    							tabItem(tabName = "cajadebigotes",
	    											fluidRow(
	    												column(width = 3,
	    															 box(width = "100%",
		    															 pickerInput("bigotesColumna", "Columna:",choices = c("NA"),
		    															 						options = list(
		    															 							`actions-box` = TRUE,
		    															 							`live-search` = TRUE)),
		    															 numericInput("bigotesFiltroY1", "Y Min", value = 0),
		    															 numericInput("bigotesFiltroY2", "Y Max", value = 1),
		    															 pickerInput("bigotesSeleccionar", "Datos:", choices = c("NA"), multiple = TRUE,
		    															 						options = list(
		    															 							`actions-box` = TRUE, `deselect-all-text` = "Deseleccionar todos",
		    															 							`select-all-text` = "Seleccionar todos",
		    															 							`live-search` = TRUE)),
		    															 actionButton("exeOpcionesBigotes", "Confirmar")
	    															 )
	    												),
	    												column(width = 9,
	    															 box(width = "100%",
	    															 		plotlyOutput("bigotesFinal", height = "800px")
	    															 )
	    												)
	    											)
	    							),
	    							tabItem(tabName = "lineadetiempo",
	    											fluidRow(
	    												column(width = 3,
	    															 box(width = "100%",
	    															 		actionButton("exeLineaDeTiempo", "Generar gráfico")
	    															 )
	    												),
	    												column(width = 9,
	    															 box(width = "100%",
	    															 		plotOutput("lineadetiempoFinal", height = "800px")
	    															 )
	    												)
	    											)
	    							),
	    							tabItem(tabName = "notatecnica",
	    											fluidRow(
	    												column(3,
	    															 box(width = "100%",
		    															 numericInput("NTpoblacion", min = 1, value = 1, label = "Población:", step = 1),
		    															 numericInput("NTmeses", min = 1, value = 1, label = "Numero de meses:", step = 1),
		    															 actionButton("exeNTremoveRow", "Remover filas seleccionadas", width = "100%"),
		    															 br(),
		    															 br(),
		    															 actionButton("exeNT", "Crear nota técnica", width = "100%"),
		    															 br(),
		    															 br(),
		    															 downloadButton("descargaNTExcel", "Excel", style="width:100%;"),
		    															 br(),
		    															 br(),
		    															 tags$b("Primer escenario P75:"),
		    															 textOutput("sumasNtEsc1"),
		    															 textOutput("sumasNtEsc1Mes"),
		    															 textOutput("porcNtEsc1"),
		    															 br(),
		    															 tags$b("Segundo escenario media:"),
		    															 textOutput("sumasNtEsc2"),
		    															 textOutput("sumasNtEsc2Mes"),
		    															 textOutput("porcNtEsc2"),
		    															 br(),
		    															 tags$b("Tercer escenario media truncada 10%:"),
		    															 textOutput("sumasNtEsc3"),
		    															 textOutput("sumasNtEsc3Mes"),
		    															 textOutput("porcNtEsc3"),
		    															 br(),
		    															 tags$b("Cuarto escenario media truncada 5%:"),
		    															 textOutput("sumasNtEsc4"),
		    															 textOutput("sumasNtEsc4Mes"),
		    															 textOutput("porcNtEsc4"),
		    															 br(),
		    															 tags$b("Escenario combiando mayor:"),
		    															 textOutput("sumasNtEsc5"),
		    															 textOutput("sumasNtEsc5Mes"),
		    															 textOutput("porcNtEsc5"),
		    															 br(),
		    															 tags$b("Escenario combiando variabilidad y frecuencia:"),
		    															 textOutput("sumasNtEsc6"),
		    															 textOutput("sumasNtEsc6Mes"),
		    															 textOutput("porcNtEsc6"),
		    															 br(),
		    															 br(),
		    															 br(),
		    															 br()
	    															 )
	    												),
	    												column(9,
	    															 box(width = "100%",
	    															 		DT::dataTableOutput("NTtabla")
	    															 )
	    												)
	    											)
	    											),
	    							tabItem(tabName = "indicepaquete",
	    											fluidRow(
	    												box(width = 12,
	    														DT::dataTableOutput("paqueteIndexTable")
	    														)
	    											)
	    											),
	    							tabItem(tabName = "dashboardpaquete",
	    											fluidRow(
	    												column(width = 12,
	    															 valueBoxOutput("BOX_especialidad", width = "100%")
	    												)
	    											),
	    											fluidRow(
	    												column(width = 4,
	    															 valueBoxOutput("BOX_valortotal", width = "100%")
	    												),
	    												column(width = 4,
	    															 valueBoxOutput("BOX_costototal", width = "100%")
	    												),
	    												column(width = 4,
	    															 valueBoxOutput("BOX_codpaquete", width = "100%")
	    												)
	    											),
	    											fluidRow(
	    												column(width = 12,
	    															 valueBoxOutput("BOX_descripcion", width = "100%")
	    												)
	    											),
	    											fluidRow(
	    												column(width = 9,
	    															 box(width = "100%", height = "100%",
	    															 		selectInput("paquete", label = "Paquete:", choices = na.omit(unique(PAQUETES$`CODIGO PAQUETE`))),
	    															 		ggiraph::ggiraphOutput("PLOT_ref_paquete", width = "100%", height = "100%"),
	    															 		tags$p(style = "text-align: justify;",
	    															 					 tags$em(
	    															 					 	"Este gráfico corresponde a la comparación del valor o costo de la institución para el paquete seleccionado comparado con paquetes ofertados en otras instituciones hospitalarias. También, muestra la media del mercado, el valor mínimo y el máximo, que permite evaluar la diferencia entre el valor o costo ofertado por la institución frente a otros paquetes que existan actualmente en el mercado."
	    															 					 )
	    															 		)
	    															 )
	    												),
	    												column(width = 3,
	    															 box(
	    															 	title = "Servicio:",
	    															 	width = "100%",
	    															 	tags$h3(textOutput("BOX_servicio"))
	    															 ),
	    															 box(
	    															 	title = "Inclusiones:",
	    															 	width = "100%",
	    															 	height = "100%",
	    															 	tags$p(textOutput("BOX_inclusiones"))
	    															 ),
	    															 box(
	    															 	title = "Exclusiones:",
	    															 	width = "100%",
	    															 	height = "100%",
	    															 	tags$p(textOutput("BOX_exclusiones"))
	    															 )
	    												)
	    											),
	    											fluidRow(
	    												column(width = 6,
	    															 box(width = "100%",
	    															 		selectInput("COL_pie_componente", label = "Componente:", choices = unique(PAQUETES_CC$COMPONENTE), multiple = T),
	    															 		selectInput("COL_pie_componente_en", label = "Datos:", choices = c("PRESTACION", "TIPO DE COSTO")),
	    															 		ggiraph::ggiraphOutput("PLOT_pie_componente", width = "100%"),
	    															 		dataTableOutput("TABLA_pie_componente"),
	    															 		br(),
	    															 		tags$p(style = "text-align: justify;",
	    															 					 tags$em(
	    															 					 	"En la sección de componentes se podrán seleccionar uno o varios de los componentes que conforman el paquete. Al escoger varios, se pueden observar las prestaciones o los tipos de costo que hacen parte de esta categoría."
	    															 					 )
	    															 		)
	    															 )
	    												),
	    												column(width = 6,
	    															 box(width = "100%",
	    															 		selectInput("COL_pie_tipocosto", label = "Tipo de costo:", choices = unique(PAQUETES_CC$`TIPO DE COSTO`), multiple = T),
	    															 		selectInput("COL_pie_tipocosto_en", label = "Datos:", choices = c("PRESTACION", "COMPONENTE")),
	    															 		ggiraph::ggiraphOutput("PLOT_pie_tipocosto", width = "100%"),
	    															 		dataTableOutput("TABLA_pie_tipocosto"),
	    															 		br(),
	    															 		tags$p(style = "text-align: justify;",
	    															 					 tags$em(
	    															 					 	"En la sección de tipos de costo se podrán seleccionar todos los tipos de costo que conforman el paquete. Al escoger varios, se pueden observar las prestaciones o los componentes que hacen parte de esta categoría."
	    															 					 )
	    															 		)
	    															 )
	    												)
	    											),
	    											fluidRow(
	    												column(width = 7,
	    															 box(width = "100%",
	    															 		DT::dataTableOutput("TABLA_paquete", width = "100%")
	    															 )
	    												),
	    												column(width = 5,
	    															 box(width = "100%",
	    															 		ggiraph::girafeOutput("PLOT_ref_cumscups", width = "100%"),
	    															 		tags$p(style = "text-align: justify;",
	    															 					 tags$em(
	    															 					 	"La tabla de prestaciones discriminada permite seleccionar cada prestación que hace parte del paquete generando un gráfico comparativo de cada prestación individual con los valores de referencia de los cuales se tenga información en el análisis del mercado."
	    															 					 )
	    															 		)
	    															 ),
	    															 box(width = "100%",
	    															 		selectInput("COL_pie_paquete", label = "", choices = c("COMPONENTE", "TIPO DE COSTO")),
	    															 		ggiraph::girafeOutput("PLOT_pie_paquete", width = "100%"),
	    															 		dataTableOutput("TABLA_pie_paquete_resumen"),
	    															 		br(),
	    															 		tags$p(style = "text-align: justify;",
	    															 					 tags$em(
	    															 					 	'Al seleccionar "Tipo de cost" o "Componente" se genera un gráfico circular mostrando las diferentes partes de la categoría y sus proporciones.'
	    															 					 )
	    															 		)
	    															 )
	    												)
	    											)
	    							),
	    							tabItem(tabName = "pricing",
	    											selectInput("pricingSelect", label = "Informe:", choices = str_replace(list.files("PRICING/"), ".csv", "")),
	    											fluidRow(
	    												column(width = 3,
		    												box(width = "100%",
		    																		 uiOutput("pricingOutPrestacion"),
		    																		 uiOutput("pricingOutEntidad"),
		    																		 uiOutput("pricingOutObs"),
		    																		 uiOutput("pricingOutRel_media"),
		    																		 uiOutput("pricingOutRel_min"),
		    																		 # sliderInput("rel-min", "Relativo al mínimo", min = 0, max = 1, value = c(0,1)),
		    																		 actionButton("pricingEjecutar", "Aplicar")
		    																		 
		    												)							 
		    										  ),
	    												# Show a plot of the generated distribution
	    												column(width = 9,
	    													box(width = "100%",		 
		    													shiny::tabsetPanel(
		    														tabPanel("Entidades",
		    																		 ggiraphOutput("pricingEntidades", height = "600px", width = "100%")
		    														),
		    														tabPanel("Descriptiva por Prestación",
		    																		 ggiraphOutput("pricingPrestaciones", height = "600px", width = "100%")
		    														)
		    													)
	    													)
	    												)
	    											),
	    											box(width = "100%",
	    												DT::dataTableOutput("pricingCUPSCUMS", height = "300px")
	    											)
	    							)
	    						)
	    					)
	           )
		)
	}
)
