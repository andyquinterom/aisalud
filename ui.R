shinyUI(
  function(request) {
  tagList(
    tags$head(
      tags$script(type = "text/javascript", src = "code.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    dashboardPagePlus(
      collapse_sidebar = TRUE,
      skin = "blue-light",
      dashboardHeaderPlus(
        fixed = TRUE,
        title = tagList(
          span(class = "logo-lg", "Analítica Integrada Salud"), 
          img(src = "logo.svg")),
        dropdownMenu(
          icon = icon("info-circle"),
          type = "notifications",
          badgeStatus = "info",
          notificationItem(
            text = "Version: 2.1.1.9000",
            icon = icon("code-branch"),
            status = "info"
          )
        ),
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "filter"),
      sidebar = dashboardSidebar(
        collapsed = TRUE,
        sidebarMenu(
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
          menuItem(
            text = "Análisis",
            icon = icon("chart-area", lib = "font-awesome"),
            menuSubItem(
              text = "Descriptiva",
              tabName = "episodios_modulo", 
              icon = icon("table", lib = "font-awesome")),
            menuSubItem(
              text = "Frecuencias",
              tabName = "frecuencias_modulo", 
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
            tabName = "frecuencias_modulo",
            frecuencias_ui("frecuencias_modulo")
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
          )
        )
      )
     )
    )
  }
)
