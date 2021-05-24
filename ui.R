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
        fixed = TRUE,
        title = tagList(
          span(class = "logo-lg", "Analítica Integrada Salud"),
          img(src = "logo.svg")),
        dropdownMenu(
          icon = icon("info-circle"),
          type = "notifications",
          badgeStatus = "info",
          notificationItem(
            text = "Version: 2.4.1",
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
            tabName = "cargar_datos",
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
            text = "Dashboard Contratos",
            icon = icon("dollar-sign", lib = "font-awesome"),
            tabName = "nt_dashboard")
        )
      ),
      controlbar = dashboardControlbar(
        width = 700,
        controlbarMenu(
          controlbarItem(
            id = "filtros_sideBar",
            title = "Filtros",
            filtros_ui("filtros_sideBar")
          )
        )
      ),
      dashboardBody(
  # Modulos -------------------------------------------------------------------
        tabItems(
          tabItem(
            tabName = "cargar_datos",
            cargar_datos_ui("cargar_datos")
          ),
          tabItem(
            tabName = "outliers_modulo",
            outliers_ui("outliers_modulo")
          ),
          tabItem(
            tabName = "episodios_modulo",
            descriptiva_ui("episodios_modulo")
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
            tabName = "nt_dashboard",
            nt_dashboard_ui("nt_dashboard")
          )
        )
      )
     )
    )
  }
)
