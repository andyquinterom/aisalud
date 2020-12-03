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
    dashboardPagePlus(
      collapse_sidebar = TRUE,
      skin = "black",
      dashboardHeaderPlus(
        fixed = TRUE,
        title = "",
        enable_rightsidebar = TRUE),
      sidebar = dashboardSidebar(
        collapsed = TRUE,
        sidebarMenu(
          HTML('
               <img src="logoblanco.png" width="100%"/>
               '),
          if (Sys.getenv("DATABASE_ACCESS") != "") {
            menuItem(
              text = "Carga de datos",
              icon = icon("cog", lib = "font-awesome"),
              menuSubItem(
                text = "Subir datos",
                tabName = "prepara",
                icon = icon("upload", lib = "font-awesome")),
              menuItem(
                text = "Cargar de la nube",
                tabName = "prepara_base_de_datos",
                icon = icon("cloud", lib = "font-awesome")))
          } else {
            menuItem(
              text = "Prepara",
              icon = icon("cog", lib = "font-awesome"),
              tabName = "prepara")
          },
          # menuItem(
          #   text = tags$b("- Modulos -")
          # ),
          # menuItem(
          #   text = "Descriptiva clásica",
          #   tabName = "descriptiva_modulo",
          #    icon = icon("table", lib = "font-awesome")),
          menuItem(
            text = "Análisis",
            icon = icon("chart-area", lib = "font-awesome"),
            menuSubItem(
              text = "Descriptiva",
              tabName = "episodios_modulo", 
              icon = icon("table", lib = "font-awesome")),
            menuSubItem(
              text = "Nota técnica", 
              tabName = "nota_tecnica_modulo",
              icon = icon("tags", lib = "font-awesome")),
            menuSubItem(
              text = "Outliers",
              icon = icon("search-minus", lib = "font-awesome"),
              tabName = "outliers_modulo"),
            menuSubItem(
              text = "Otros gráficos",
              icon = icon("chart-bar", lib = "font-awesome"),
              tabName = "otros_graficos_modulo"
            )
          ),
          if (NTS_INCLUIDO) {
            menuItem(
              text = "Seguimiento",
              icon = icon("dollar-sign", lib = "font-awesome"),
              menuSubItem(
                text = "Índice",
                tabName = "seguimiento_modulo_indice"),
              menuSubItem(
                text = "Dashboard",
                tabName = "seguimiento_modulo_dash"),
              menuSubItem(
                text = "Comparación",
                tabName = "seguimiento_modulo_comparar")
            )
          },
          if (PAQUETES_INCLUIDO) {
            menuItem(
              text = "Paquetes",
              icon = icon("chart-pie", lib = "font-awesome"),
              menuSubItem(text = "Índice", tabName = "paquetes_modulo_indice"),
              menuSubItem(text = "Dashboard", tabName = "paquetes_modulo_dashboard")
            )
          }
        )
      ),
      rightsidebar = rightSidebar(
        width = 700,
        rightSidebarTabContent(
          id = "filtros_sideBar",
          active = TRUE,
          filtros_ui("filtros_sideBar")
        )
      ),
      dashboardBody(
  # Modulos -------------------------------------------------------------------
        tags$div(
          style = "min-height: 50px;"
        ),
        tabItems(
          tabItem(
            tabName = "prepara",
            prepara_ui("prepara_modulo")
          ),
          tabItem(
            tabName = "prepara_base_de_datos",
            base_de_datos_ui("prepara_base_de_datos")
          ),
          tabItem(
            tabName = "outliers_modulo",
            outliers_ui("outliers_modulo")
          ),
          tabItem(
            tabName = "episodios_modulo",
            episodios_ui("episodios_modulo")
          ),
          tabItem(
            tabName = "nota_tecnica_modulo",
            nota_tecnica_ui("nota_tecnica_modulo")
          ),
          tabItem(
            tabName = "otros_graficos_modulo",
            otros_graficos_ui("otros_graficos_modulo")
          ),
          tabItem(
            tabName = "seguimiento_modulo_indice",
            seguimiento_notas_indice_ui("seguimiento_notas_indice")
          ),
          tabItem(
            tabName = "seguimiento_modulo_dash",
            seguimiento_notas_dashboard_ui("seguimiento_notas_dash")
          ),
          tabItem(
            tabName = "seguimiento_modulo_comparar",
            seguimiento_notas_comparar_ui("seguimiento_notas_comparar")
          ),
          tabItem(
            tabName = "paquetes_modulo_indice",
            paquetes_indice_ui("paquetes_modulo_indice")
          ),
          tabItem(
            tabName = "paquetes_modulo_dashboard",
            paquetes_dashboard_ui("paquetes_modulo_dashboard")
          ),
  # Opciones -------------------------------------------------------------------
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
                        "Si se genera un error, el archivo no es feather o tu base de datos no contiene las columnas: NRO_IDENTIFICACION, FECHA_PRESTACION o VALOR.",
                        style = "color: black;"))),
                  DT::dataTableOutput(
                    outputId = "preview",
                    width = "100%"))))),
  
 # Histogramas ----------------------------------------------------------------
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
                    inputId = "histograma_exe",
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
 # Caja de bigotes -------------------------------------------------------------
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
                    inputId = "bigotes_y1", 
                    label = "Y Min", 
                    value = 0),
                  numericInput(
                    inputId = "bigotes_y2", 
                    label = "Y Max", 
                    value = 1),
                  pickerInput(
                    inputId = "bigotes_seleccionar",
                    label = "Datos:", 
                    choices = c("NA"), 
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "Deseleccionar todos",
                      `select-all-text` = "Seleccionar todos",
                      `live-search` = TRUE)),
                  actionButton(
                    inputId = "bigotes_exe", 
                    label = "Confirmar"))),
              column(
                width = 9,
                box(
                  width = "100%",
                  plotlyOutput(
                    outputId = "bigotes_render",
                    height = "800px"))))),
         # Pricing -----------------------------------------------------------
          
          tabItem(
            tabName = "pricing",
            selectInput(
              inputId = "pricing_select",
              label = "Informe:",
              choices = str_replace(list.files("datos/pricing/"), ".csv", "")),
            fluidRow(
              column(
                width = 3,
                box(width = "100%",
                    uiOutput(outputId = "pricing_ui_prestacion"),
                    uiOutput(outputId = "pricing_ui_entidad"),
                    uiOutput(outputId = "pricing_ui_observacion"),
                    uiOutput(outputId = "pricing_ui_rel_media"),
                    uiOutput(outputId = "pricing_ui_rel_min"),
                    actionButton(inputId = "pricing_exe", label = "Aplicar"))),
              column(
                width = 9,
                box(
                  width = "100%",
                  shiny::tabsetPanel(
                    tabPanel(
                      title = "Entidades",
                      ggiraphOutput(
                        outputId = "pricing_entidades",
                        height = "600px",
                        width = "100%")),
                    tabPanel(
                      title = "Descriptiva por Prestación",
                      ggiraphOutput(
                        outputId = "pricing_prestaciones",
                        height = "600px",
                        width = "100%")))))),
            box(
              width = "100%",
              DT::dataTableOutput(outputId = "pricing_cups", height = "300px")
              ))
        )
      )
     )
    )
  }
)
