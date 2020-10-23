shinyUI(
  function(request) {
  tagList(
    tags$head(
      tags$script(type = "text/javascript", src = "code.js"),
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
      ))),
    dashboardPage(
      skin = "black",
      dashboardHeader(title = "Salud - Analítica"),
      dashboardSidebar(
        sidebarMenu(
          HTML('
               <img src="logoblanco.png" width="100%"/>
               '),
          menuItem(
              text = "Opciones",
              icon = icon("cog", lib = "font-awesome"),
              tabName = "opciones"),
          menuItem(
              text = "Descriptiva",
              icon = icon("table", lib = "font-awesome"),
              tabName = "descriptiva_eventos"),
          menuItem(
            text = "Episodios", 
            icon = icon("table", lib = "font-awesome"),
            tabName = "episodios"),
          menuItem(
              text = "Pacientes Outliers",
              icon = icon("search-minus", lib = "font-awesome"),
              tabName = "outliers"),
          menuItem(
              text = "Gráficos",
              icon = icon("chart-area", lib = "font-awesome"), 
              tabName = "graficos",
              menuSubItem(text = "Histogramas y barras",
                          tabName = "histogramas_barras"), 
              menuSubItem(text = "Caja de bigotes", 
                          tabName = "cajadebigotes"), 
              menuSubItem(text = "Linea de tiempo",
                          tabName = "lineadetiempo")),
          menuItem(
              text = "Nota técnica",
              icon = icon("search-dollar", lib = "font-awesome"),
              tabName = "notatecnica"),
          (
          if (NT_INCLUIDO) {
            menuItem(
              text = "Notas técnicas",
              icon = icon("dollar-sign", lib = "font-awesome"),
              tabName = "NTs",
              tags$br(),
              actionButton("dashNT_actualizar", "Actualizar"), 
              menuSubItem(text = "Índice", tabName = "indiceNT"),
              menuSubItem(text = "Dashboard", tabName = "dashboardNT"),
              menuSubItem(text = "Comparar", tabName = "compararNT"))
          }
          ),
          (
          if (PAQUETES_INCLUIDO) {
            menuItem(
              text = "Paquetes",
              icon = icon("chart-pie", lib = "font-awesome"),
              tabName = "paquetes",
              radioButtons("DASHBOARD_valorcosto", 
                           "Graficar:",
                           choices = c("VALOR", "COSTO")),
              actionButton("DASHBOARD_actualizar", "Actualizar"),
              menuSubItem(text = "Índice", tabName = "indicepaquete"), 
              menuSubItem(text = "Dashboard", tabName = "dashboardpaquete"),
              tags$br()
            )
          }
          ),
          (
          if (PRICING_INCLUIDO) {
            menuItem(
              text = "Pricing", 
              icon = icon("tags", lib = "font-awesome"),
              tabName = "pricingOpciones", 
              tags$br(),
              actionButton("pricingActualizar", "Actualizar"),
              menuSubItem(text = "Informes", tabName = "pricing"),
              tags$br()
            )
            }
          )
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "opciones",
              chooseSliderSkin("Square"),
              fluidRow(
                column(
                  width =5,
                  fileInput(
                    "file",
                    label = "Base de datos",
                    accept = c(".feather"),
                    buttonLabel = "Subir",
                    placeholder = "Ningún archivo selecionado"))),
              fluidRow(
                column(
                  width = 2,
                  dateInput(
                    inputId = "fecha_min",
                    label = "Fecha Inicial:",
                    value = NULL, 
                    min = NULL, 
                    max = NULL, 
                    format = "dd-mm-yyyy",
                    language = "es"),
                  dateInput(
                    inputId = "fecha_max",
                    label = "Fecha Final:",
                    value = NULL,
                    min = NULL,
                    max = NULL,
                    format = "dd-mm-yyyy", 
                    language = "es"),
                  tags$style(HTML(".datepicker {z-index:99999 !important;}")),
                  textInput(
                    inputId = "formato_fecha",
                    label = "Formato de Fecha",
                    value = "%d/%m/%Y"),
                  actionButton(inputId = "ejecutar_opciones", label = "Aplicar")),
                column(
                  width = 3,
                  radioButtons(
                    inputId = "valor_costo", 
                    label = "Calcular estadísticas por:",
                    choiceNames = list("VALOR","COSTO"),
                    choiceValues = list("VALOR", "COSTO"),
                    inline = TRUE, width='75%'),
                  radioButtons(
                    inputId = "analisis_prestacion",
                    label = "Calcular estadísticas por:",
                    choices = list("PACIENTE","PRESTACIÓN"),
                    inline = TRUE, width='75%')),
                column(
                  width = 6,
                  tags$h3("Filtros:"),
                  fluidRow(
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_1",
                        choices = c("NA"),
                        width = "100%")),
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_1_val", 
                        choices = c("NA"), 
                        multiple = TRUE, 
                        width = "100%",
                        options = list(
                          `actions-box` = TRUE,
                          `deselect-all-text` = "Deseleccionar todos",
                          `select-all-text` = "Seleccionar todos",
                          `live-search` = TRUE)))),
                  fluidRow(
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_2", 
                        choices = c("NA"), 
                        width = "100%")),
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_2_val", 
                        choices = c("NA"), 
                        multiple = TRUE,
                        width = "100%",
                        options = list(
                          `actions-box` = TRUE, 
                          `deselect-all-text` = "Deseleccionar todos",
                          `select-all-text` = "Seleccionar todos",
                          `live-search` = TRUE)))),
                  fluidRow(
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_3",
                        choices = c("NA"),
                        width = "100%")),
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_3_val",
                        choices = c("NA"),
                        multiple = TRUE, 
                        width = "100%",
                        options = list(
                          `actions-box` = TRUE,
                          `deselect-all-text` = "Deseleccionar todos",
                          `select-all-text` = "Seleccionar todos",
                          `live-search` = TRUE)))),
                  fluidRow(
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_4",
                        choices = c("NA"), 
                        width = "100%")),
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_4_val",
                        choices = c("NA"), 
                        multiple = TRUE, 
                        width = "100%",
                        options = list(
                          `actions-box` = TRUE,
                          `deselect-all-text` = "Deseleccionar todos",
                          `select-all-text` = "Seleccionar todos",
                          `live-search` = TRUE)))),
                  fluidRow(
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_5",
                        choices = c("NA"),
                        width = "100%")),
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_5_val",
                        choices = c("NA"),
                        multiple = TRUE,
                        width = "100%",
                        options = list(
                          `actions-box` = TRUE,
                          `deselect-all-text` = "Deseleccionar todos",
                          `select-all-text` = "Seleccionar todos",
                          `live-search` = TRUE)))),
                  fluidRow(
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_num_1",
                        choices = c("NA"),
                        width = "100%")),
                    column(
                      width = 3,
                      numericInput(
                        inputId = "filtro_num_1_min",
                        label = NULL,
                        value = 0,
                        min = 0,
                        max = 0, 
                        width = "100%")),
                    column(
                      width = 3,
                      numericInput(
                        inputId = "filtro_num_1_max",
                        label = NULL, 
                        value = 0,
                        min = 0,
                        max = 0,
                        width = "100%"))),
                  fluidRow(
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_num_2",
                        choices = c("NA"),
                        width = "100%")),
                    column(
                      width = 3,
                      numericInput(
                        inputId = "filtro_num_2_min",
                        label = NULL,
                        value = 0,
                        min = 0,
                        max = 0, 
                        width = "100%")),
                    column(
                      width = 3,
                      numericInput(
                        inputId = "filtro_num_2_max",
                        label = NULL, 
                        value = 0, 
                        min = 0, 
                        max = 0, 
                        width = "100%"))),
                  fluidRow(
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_largo_1",
                        choices = c("NA"),
                        width = "100%")),
                    column(
                      width = 5,
                      textInput(
                        inputId = "filtro_largo_1_val",
                        width = "100%",
                        label = NULL)),
                    column(
                      width = 1,
                      checkboxInput(
                        inputId = "filtro_largo_1_excluir",
                        label = NULL))),
                  fluidRow(
                    column(
                      width = 6,
                      pickerInput(
                        inputId = "filtro_largo_2",
                        choices = c("NA"),
                        width = "100%")),
                    column(
                      width = 5,
                      textInput(
                        inputId = "filtro_largo_2_val",
                        width = "100%",
                        label = NULL)),
                    column(
                      width = 1,
                      checkboxInput(
                        inputId = "filtro_largo_2_excluir",
                        label = NULL))),
                  fluidRow(
                    column(
                      width = 12,
                      actionButton(
                        inputId = "filtro_aplicar",
                        label = "Aplicar Filtros"))),
                  tags$style(HTML(".dropdown-menu {z-index:99999 !important;}")))),
            br(),
            fluidRow(
              column(
                width = 5,
                tags$div()),
              column(
                width = 6,
                box(
                  width = "100%",
                  fluidRow(
                    column(
                      width = 2,
                      tags$h3("Preview:")),
                    column(
                      width = 10,
                      br(),
                      tags$a(
                        "Si se genera un error, el archivo no es feather o tu base de datos no contiene las columnas: NRO_IDENTIFICACION, FECHA_PRESTACION, VALOR o COSTO.",
                        style = "color: black;"))),
                  DT::dataTableOutput(
                    outputId = "preview",
                    width = "100%"))))),
          tabItem(
            tabName = "descriptiva_eventos",
            fluidRow(
              column(
                width = 3,
                box(width = "100%",
                    pickerInput(
                      inputId = "descriptiva_cols",
                      label = "Agrupar por:",
                      choices = c("NA"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Deseleccionar todos",
                        `select-all-text` = "Seleccionar todos",
                        `live-search` = TRUE)),
                    actionButton(
                      inputId = "descriptiva_exe",
                      label = "Confirmar"),
                    tags$br(),
                    tags$br(),
                    downloadButton(
                      outputId = "descriptiva_descargar_csv",
                      label = "CSV",
                      style = "width:100%;"),
                    tags$br(),
                    tags$br(),
                    downloadButton(
                      outputId = "descriptiva_descargar_xlsx", 
                      label = "Excel",
                      style = "width:100%;"),
                    br(),
                    br(),
                    textOutput(outputId = "sumasDescriptivaRegistros"),
                    textOutput(outputId = "sumasDescriptivaPacientes"),
                    textOutput(outputId = "sumasDescriptivaValorCosto"),
                    textOutput(outputId = "sumasDescriptivaCUPS"))),
              column(
                width = 9,
                box(
                  width = "100%",
                  div(
                    DT::dataTableOutput(outputId = "descriptiva_tabla"),
                    style = "font-size:90%"))))),
          tabItem(
            tabName = "episodios",
            fluidRow(
              column(
                width = 3,
                box(width = "100%",
                    pickerInput(
                      inputId = "episodios_col_valor",
                      label = "Sumar valor por:",
                      choices = c("NA"),
                      multiple = FALSE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE)),
                    pickerInput(
                      inputId = "episodios_cols",
                      label = "Agrupar por:",
                      choices = c("NA"),
                      multiple = FALSE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE)),
                    pickerInput(
                      inputId = "episodios_cols_sep",
                      label = "Separar por:",
                      choices = c("NA"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `select-all-text` = "Seleccionar todos",
                        `deselect-all-text` = "Deseleccionar todos")),
                    actionButton("episodios_exe", "Confirmar"),
                    tags$br(),
                    tags$br(),
                    downloadButton(
                      outputId = "descargaDescriptivaEpCSV",
                      label = "CSV",
                      style = "width:100%;"),
                    tags$br(),
                    tags$br(),
                    downloadButton(
                      outputId = "descargaDescriptivaEpEXCEL",
                      label = "Excel",
                      style = "width:100%;"))),
              column(
                width = 9,
                box(
                  width = "100%",
                  div(
                    DT::dataTableOutput(outputId = "episodios_tabla"),
                    style = "font-size:90%"))))),
          tabItem(
            tabName = "outliers",
            fluidRow(
              column(
                width = 3,
                box(width = "12",
                    pickerInput(
                      inputId = "outliers_cols",
                      label = "Agrupar por:",
                      choices = c("NA"),
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE)),
                    sliderInput(
                      inputId = "outliers_percentil",
                      label = "Outliers por percentil:",
                      min = 0.75, 
                      max = 0.99, 
                      value = c(0.9)),
                    radioButtons(
                      inputId = "outliers_iqr",
                      "Outliers por IQR",
                      choiceNames = list("1.5","3.0"),
                      choiceValues = list(1.5, 3.0),
                      inline = TRUE,
                      width='75%'),
                    numericInput(
                      inputId = "outliers_frecuencia",
                      label = "Frecuencia Mínima",
                      min = 0,
                      step = 1, 
                      value = 0),
                    actionButton(
                      inputId = "outliers_percentil_exe",
                      label = "Calcular por percentil",
                      width = "100%"),
                    actionButton(
                      inputId = "outliers_iqr_exe",
                      label = "Calcular por IQR",
                      width = "100%"),
                    tags$br(),
                    tags$br(),
                    downloadButton(
                      outputId = "descargaOutliersCSV",
                      label = "CSV", 
                      style = "width:100%;"),
                    tags$br(),
                    tags$br(),
                    downloadButton(
                      outputId = "descargaOutliersEXCEL",
                      label = "Excel",
                      style = "width:100%;"))),
              column(
                width = 9,
                box(width = "100%",
                    div(DT::dataTableOutput("outliers_tabla"),
                        style = "font-size:90%"))))),
          tabItem(
            tabName = "histogramas_barras",
            fluidRow(
              column(
                width = 3,
                box(width = "100%",
                  pickerInput(
                    inputId = "histograma_col",
                    label = "Columna:",
                    choices = "NA",
                    options = list(
                      `actions-box` = TRUE,
                      `live-search` = TRUE)),
                  pickerInput(
                    inputId = "histograma_fill",
                    label = "Relleno:",
                    choices = "NA",
                    options = list(
                      `actions-box` = TRUE,
                      `live-search` = TRUE)),
                  numericInput(
                    inputId = "histograma_x_min",
                    label = "X Min",
                    value = 0),
                  numericInput(
                    inputId = "histograma_x_max",
                    label = "X Max", 
                    value = 1),
                  numericInput(
                    inputId = "histograma_width",
                    label = "Ancho del bin",
                    value = 1),
                  numericInput(
                    inputId = "histograma_bins",
                    label = "# de bins",
                    value = 10),
                  actionButton(
                    inputId = "exeOpcionesHistograma",
                    label = "Confirmar"),
                  br(),
                  br(),
                  tags$p("Los valores aqui exhibidos pueden cambiar radicalmente 
                         el rendimiento de la aplicación. Es recomendado tener 
                         un numero bajo de bins y un mínimo y máximo realista.
                         Para un número de bins ideal, se recomiendo utilizar 
                         3.3*log(n) donde n es el numero de registros."
                         ))),
              column(
                width = 9,
                box(
                  width = "12",
                  plotlyOutput(
                    outputId = "histograma_render",
                    height = "800px"))))),
          tabItem(
            tabName = "cajadebigotes",
            fluidRow(
              column(
                width = 3,
                box(
                  width = "12",
                  pickerInput(
                    inputId = "bigotes_col",
                    label = "Columna:",
                    choices = c("NA"),
                    options = list(
                      `actions-box` = TRUE,
                      `live-search` = TRUE)),
                  numericInput(
                    inputId = "bigotesFiltroY1", 
                    label = "Y Min", 
                    value = 0),
                  numericInput(
                    inputId = "bigotesFiltroY2", 
                    label = "Y Max", 
                    value = 1),
                  pickerInput(
                    inputId = "bigotesSeleccionar",
                    label = "Datos:", 
                    choices = c("NA"), 
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "Deseleccionar todos",
                      `select-all-text` = "Seleccionar todos",
                      `live-search` = TRUE)),
                  actionButton(
                    inputId = "exeOpcionesBigotes", 
                    label = "Confirmar"))),
              column(
                width = 9,
                box(
                  width = "100%",
                  plotlyOutput(
                    outputId = "bigotesFinal",
                    height = "800px"))))),
          tabItem(
            tabName = "lineadetiempo",
            fluidRow(
              column(
                width = 3,
                box(
                  width = "100%",
                  actionButton(
                    inputId = "exeLineaDeTiempo",
                    label = "Generar gráfico"))),
              column(
                width = 9,
                box(
                  width = "100%",
                  plotOutput(
                    outputId = "lineadetiempoFinal",
                    height = "800px"))))),
          tabItem(
            tabName = "notatecnica",
            fluidRow(
              column(
                width = 3,
                box(
                  width = "100%",
                  numericInput(
                    inputId = "crear_nt_poblacion",
                    min = 1,
                    value = 1,
                    label = "Población:", 
                    step = 1),
                  numericInput(
                    inputId = "crear_nt_meses", 
                    min = 1,
                    value = 1,
                    label = "Numero de meses:",
                    step = 1),
                  actionButton(
                    inputId = "crear_nt_remove",
                    label = "Remover filas seleccionadas",
                    width = "100%"),
                  br(),
                  br(),
                  actionButton(
                    inputId = "crear_nt_exe",
                    label = "Crear nota técnica",
                    width = "100%"),
                  br(),
                  br(),
                  downloadButton(
                    outputId = "descargaNTExcel",
                    "Excel", 
                    style="width:100%;"),
                  br(),
                  br(),
                  tags$b("Primer escenario P75:"),
                  textOutput(outputId = "sumasNtEsc1"),
                  textOutput(outputId = "sumasNtEsc1Mes"),
                  textOutput(outputId = "porcNtEsc1"),
                  br(),
                  tags$b("Segundo escenario media:"),
                  textOutput(outputId = "sumasNtEsc2"),
                  textOutput(outputId = "sumasNtEsc2Mes"),
                  textOutput(outputId = "porcNtEsc2"),
                  br(),
                  tags$b("Tercer escenario media truncada 10%:"),
                  textOutput(outputId = "sumasNtEsc3"),
                  textOutput(outputId = "sumasNtEsc3Mes"),
                  textOutput(outputId = "porcNtEsc3"),
                  br(),
                  tags$b("Cuarto escenario media truncada 5%:"),
                  textOutput(outputId = "sumasNtEsc4"),
                  textOutput(outputId = "sumasNtEsc4Mes"),
                  textOutput(outputId = "porcNtEsc4"),
                  br(),
                  tags$b("Escenario combiando mayor:"),
                  textOutput(outputId = "sumasNtEsc5"),
                  textOutput(outputId = "sumasNtEsc5Mes"),
                  textOutput(outputId = "porcNtEsc5"),
                  br(),
                  tags$b("Escenario combiando variabilidad y frecuencia:"),
                  textOutput(outputId = "sumasNtEsc6"),
                  textOutput(outputId = "sumasNtEsc6Mes"),
                  textOutput(outputId = "porcNtEsc6"),
                  br(),
                  br(),
                  br(),
                  br())),
              column(
                width = 9,
                box(width = "100%",
                    DT::dataTableOutput(outputId = "crear_nt_tabla"))))),
          tabItem(
            tabName = "indiceNT",
            fluidRow(
              box(
                width = 7,
                DT::dataTableOutput(outputId = "indiceNT", height = "80vh")),
              box(width = 5, plotlyOutput(
                outputId = "indiceNT_Mapa",
                height = "80vh")))),
          tabItem(
            tabName = "dashboardNT",
            fluidRow(
              column(
                width = 12,
                valueBoxOutput(outputId = "dashNTNombreEntidad", width = 12))),
            fluidRow(
              column(
                width = 12,
                valueBoxOutput(
                  outputId = "dashNT_ValorMes", width = 4),
                valueBoxOutput(
                  outputId = "dashNT_Poblacion",
                  width = 4),
                valueBoxOutput(
                  outputId = "dashNT_Departamento",
                  width = 4))),
            fluidRow(
              column(
                width = 12,
                box(
                  width = 12,
                  pickerInput(
                    inputId = "dashNT_select", 
                    width = "100%",
                    choices = NTs_UniqueCod,
                    label = "Nota técnica")),
                box(
                  width = 12,
                  title = "Nota técnica:",
                  fluidRow(
                    column(
                      width = 5,
                      DT::dataTableOutput(outputId = "dashNT_NT")),
                    column(
                      width = 7,
                      ggiraph::ggiraphOutput(
                        outputId = "dashNT_PLOT_Agrupadores",
                        width = "100%",
                        height = "100%")))),
                box(
                  title = "Inclusiones:",
                  width = 6,
                  DT::dataTableOutput(outputId = "dashNT_Inclusiones")),
                box(
                  title = "Exclusiones:",
                  width = 6,
                  DT::dataTableOutput(outputId = "dashNT_Exclusiones"))))),
          tabItem(
            tabName = "compararNT",
            fluidRow(
              column(
                width = 12,
                box(
                  width = 4,
                  pickerInput(
                    inputId = "compararNT_select",
                    width = "100%",
                    choices = NTs_UniqueCod, 
                    label = "Nota técnica"),
                  pickerInput(
                    inputId = "comparar_nt_col",
                    width = "100%",
                    choices = c("NA"), 
                    label = "Columna de agrupador"),
                  actionButton(
                    inputId = "compararNT_ejecutar",
                    "Ejecutar",
                    width = "100%")),
                box(
                  width = 8,
                  DT::dataTableOutput(
                    outputId = "compararNT_totales",
                    width = "100%",
                    height = "100%")))),
            fluidRow(
              column(
                width = 12,
                box(
                  width = 12, 
                  title = "Resultados a mes",
                  fluidRow(
                    column(
                      width = 6,
                      tags$h3("Totales RIPS"),
                      DT::dataTableOutput(
                        outputId = "compararNT_totalMesRIPS",
                        width = "100%")),
                    column(
                      width = 6,
                      tags$h3("Totales CME"),
                      DT::dataTableOutput(
                        outputId = "compararNT_totalMesCME",
                        width = "100%")))),
                box(
                  width = 12,
                  title = "Resultados por agrupador",
                  fluidRow(
                    column(
                      width = 6,
                      tags$h3("Totales RIPS"),
                      DT::dataTableOutput(
                        outputId = "compararNT_totalAgrupadorRIPS",
                        width = "100%")),
                    column(
                      width = 6,
                      tags$h3("Totales CME"),
                      DT::dataTableOutput(
                        outputId = "compararNT_totalAgrupadorCME",
                        width = "100%")))),
                box(
                  width = 12, 
                  title = "Suma de valor a mes",
                  DT::dataTableOutput(
                    outputId = "compararNT_descBSumas",
                    width = "100%")),
                box(
                  width = 12, 
                  title = "Frecuencias a mes",
                  DT::dataTableOutput(
                    outputId = "compararNT_descBFrecs",
                    width = "100%")),
                box(
                  width = 12, 
                  title = "Diferencias de valor con RIPS",
                  DT::dataTableOutput(
                    outputId = "compararNT_difsValorRIPS",
                    width = "100%"),
                  DT::dataTableOutput(
                    outputId = "compararNT_difsValorRIPSperc", 
                    width = "100%")),
                box(
                  width = 12,
                  title = "Diferencias de valor con CME",
                  DT::dataTableOutput(
                    outputId = "compararNT_difsValorCME",
                    width = "100%"),
                  DT::dataTableOutput(
                    outputId = "compararNT_difsValorCMEperc",
                    width = "100%"))))),
          tabItem(
            tabName = "indicepaquete",
            fluidRow(
              box(
                width = 12,
                DT::dataTableOutput(outputId = "paqueteIndexTable")))),
          tabItem(
            tabName = "dashboardpaquete",
            fluidRow(
              column(
                width = 12,
                valueBoxOutput(outputId = "BOX_especialidad", width = "100%"))),
            fluidRow(
              column(
                width = 4,
                valueBoxOutput(outputId = "BOX_valortotal", width = "100%")),
              column(
                width = 4,
                valueBoxOutput(outputId = "BOX_costototal", width = "100%")),
              column(
                width = 4,
                valueBoxOutput(outputId = "BOX_codpaquete", width = "100%"))),
            fluidRow(
              column(
                width = 12,
                valueBoxOutput(outputId = "BOX_descripcion", width = "100%"))),
            fluidRow(
              column(
                width = 9,
                box(
                  width = 12,
                  height = "100%",
                  selectInput(
                    inputId = "paquete",
                    label = "Paquete:",
                    choices = na.omit(unique(PAQUETES$`CODIGO PAQUETE`))),
                  ggiraph::ggiraphOutput(
                    outputId = "PLOT_ref_paquete",
                    width = "100%",
                    height = "100%"),
                  tags$p(style = "text-align: justify;",
                         tags$em("Este gráfico corresponde a la comparación del valor o costo de la institución para el paquete seleccionado comparado con paquetes ofertados en otras instituciones hospitalarias. También, muestra la media del mercado, el valor mínimo y el máximo, que permite evaluar la diferencia entre el valor o costo ofertado por la institución frente a otros paquetes que existan actualmente en el mercado."
                                 )))),
              column(
                width = 3,
                box(
                  title = "Servicio:",
                  width = "100%",
                  tags$h3(textOutput(outputId = "BOX_servicio"))),
                box(
                  title = "Inclusiones:",
                  width = "100%",
                  height = "100%",
                  tags$p(textOutput(outputId = "BOX_inclusiones"))),
                box(
                  title = "Exclusiones:",
                  width = "100%",
                  height = "100%",
                  tags$p(textOutput("BOX_exclusiones"))))),
            fluidRow(
              column(
                width = 6,
                box(
                  width = "100%",
                  selectInput(
                    inputId = "COL_pie_componente",
                    label = "Componente:",
                    choices = unique(PAQUETES_CC$COMPONENTE),
                    multiple = T),
                  selectInput(
                    inputId = "COL_pie_componente_en",
                    label = "Datos:",
                    choices = c("PRESTACION", "TIPO DE COSTO")),
                  ggiraph::ggiraphOutput(
                    outputId = "PLOT_pie_componente",
                    width = "100%"),
                  dataTableOutput(outputId = "TABLA_pie_componente"),
                  br(),
                  tags$p(style = "text-align: justify;",
                         tags$em(
                           "En la sección de componentes se podrán seleccionar uno o varios de los componentes que conforman el paquete. Al escoger varios, se pueden observar las prestaciones o los tipos de costo que hacen parte de esta categoría."
                           )))),
              column(
                width = 6,
                box(
                  width = "100%",
                  selectInput(
                    inputId = "COL_pie_tipocosto",
                    label = "Tipo de costo:",
                    choices = unique(PAQUETES_CC$`TIPO DE COSTO`),
                    multiple = T),
                  selectInput(
                    inputId = "COL_pie_tipocosto_en",
                    label = "Datos:",
                    choices = c("PRESTACION", "COMPONENTE")),
                  ggiraph::ggiraphOutput(
                    outputId = "PLOT_pie_tipocosto",
                    width = "100%"),
                  dataTableOutput(outputId = "TABLA_pie_tipocosto"),
                  br(),
                  tags$p(style = "text-align: justify;",
                         tags$em(
                           "En la sección de tipos de costo se podrán seleccionar todos los tipos de costo que conforman el paquete. Al escoger varios, se pueden observar las prestaciones o los componentes que hacen parte de esta categoría."
                           ))))),
            fluidRow(
              column(
                width = 7,
                box(
                  width = "100%",
                  DT::dataTableOutput(outputId = "TABLA_paquete", width = "100%"))),
              column(
                width = 5,
                box(
                  width = "100%",
                  ggiraph::girafeOutput(
                    outputId = "PLOT_ref_cumscups",
                    width = "100%"),
                  tags$p(style = "text-align: justify;",
                         tags$em(
                           "La tabla de prestaciones discriminada permite seleccionar cada prestación que hace parte del paquete generando un gráfico comparativo de cada prestación individual con los valores de referencia de los cuales se tenga información en el análisis del mercado."
                           ))),
                box(width = "100%",
                    selectInput(
                      inputId = "COL_pie_paquete",
                      label = "",
                      choices = c("COMPONENTE", "TIPO DE COSTO")),
                    ggiraph::girafeOutput(
                      outputId = "PLOT_pie_paquete",
                      width = "100%"),
                    dataTableOutput(outputId = "TABLA_pie_paquete_resumen"),
                    br(),
                    tags$p(style = "text-align: justify;",
                           tags$em(
                             'Al seleccionar "Tipo de cost" o "Componente" se genera un gráfico circular mostrando las diferentes partes de la categoría y sus proporciones.'
                             )))))),
          tabItem(
            tabName = "pricing",
            selectInput(
              inputId = "pricingSelect",
              label = "Informe:",
              choices = str_replace(list.files("datos/PRICING/"), ".csv", "")),
            fluidRow(
              column(
                width = 3,
                box(width = "100%",
                    uiOutput(outputId = "pricingOutPrestacion"),
                    uiOutput(outputId = "pricingOutEntidad"),
                    uiOutput(outputId = "pricingOutObs"),
                    uiOutput(outputId = "pricingOutRel_media"),
                    uiOutput(outputId = "pricingOutRel_min"),
                    actionButton(inputId = "pricingEjecutar", label = "Aplicar"))),
              column(
                width = 9,
                box(
                  width = "100%",
                  shiny::tabsetPanel(
                    tabPanel(
                      title = "Entidades",
                      ggiraphOutput(
                        outputId = "pricingEntidades",
                        height = "600px",
                        width = "100%")),
                    tabPanel(
                      title = "Descriptiva por Prestación",
                      ggiraphOutput(
                        outputId = "pricingPrestaciones",
                        height = "600px",
                        width = "100%")))))),
            box(
              width = "100%",
              DT::dataTableOutput(outputId = "pricingCUPSCUMS", height = "300px")
              ))
        )
      )
     )
    )
  }
)
