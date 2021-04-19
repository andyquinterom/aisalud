shinyUI(
  function(request) {
  tagList(
    tags$head(
      tags$script(type = "text/javascript", src = "code.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    shinydashboardPlus::dashboardPage(
      options = list(sidebarExpandOnHover = TRUE),
      skin = "blue-light",
      shinydashboardPlus::dashboardHeader(
        title = tagList(
          span(class = "logo-lg", "Analítica Integrada Salud"),
          img(src = "logo.svg")),
        dropdownMenu(
          icon = icon("info-circle"),
          type = "notifications",
          badgeStatus = "info",
          notificationItem(
            text = "Version: 2.4.0",
            icon = icon("code-branch"),
            status = "info"
          )
        ),
        controlbarIcon = icon("filter")),
      sidebar = shinydashboardPlus::dashboardSidebar(
        collapsed = TRUE,
        sidebarMenu(
          menuItem(
            text = "Cargar datos",
            tabName = "prepara_base_de_datos",
            icon = icon("cloud", lib = "font-awesome")),
          menuItem(
            text = "Descriptiva",
            tabName = "episodios_modulo",
            icon = icon("table", lib = "font-awesome")),
          menuItem(
            text = "Nota técnica",
            tabName = "nota_tecnica_modulo",
            icon = icon("tags", lib = "font-awesome")),
          menuItem(
            text = "Outliers",
            icon = icon("search-minus", lib = "font-awesome"),
            tabName = "outliers_modulo"),
          menuItem(
            text = "Otros gráficos",
            icon = icon("chart-bar", lib = "font-awesome"),
            tabName = "otros_graficos_modulo"
          ),
          menuItem(
            text = "Composición",
            icon = icon("object-group", lib = "font-awesome"),
            tabName = "composicion_modulo"
          ),
          menuItem(
            text = "Seguimiento",
            icon = icon("dollar-sign", lib = "font-awesome"),
            tabName = "seguimiento_modulo_dash"),
          menuItem(
            text = "Configuración",
            icon = icon("cog", lib = "font-awesome"),
            tabName = "configuracion")
        )
      ),
      controlbar = dashboardControlbar(
        width = 700,
        controlbarItem(
          id = "filtros_sideBar",
          active = TRUE,
          filtros_ui("filtros_sideBar")
        ) %>% controlbarMenu()
      ),
      dashboardBody(
  # Modulos -------------------------------------------------------------------
        tabItems(
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
            tabName = "composicion_modulo",
            composicion_ui("composicion_modulo")
          ),
          tabItem(
            tabName = "seguimiento_modulo_dash",
            seguimiento_notas_dashboard_ui("seguimiento_notas_dash")
          ),
          tabItem(
            tabName = "configuracion",
            configuracion_ui("configuracion")
          )
        )
      )
     )
    )
  }
)
