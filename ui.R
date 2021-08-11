#
# Analítica Integrada Salud
#
# Derechos de autor 2021 por MD&CO Consulting Group (NIT 901.119.781-5)
# Copyright (C) 2021 by MD&CO Consulting Group
#
# Este programa es software libre: puede redistribuirlo o modificarlo bajo
# los términos de la licencia Affero General Public License tal cual
# publicada por la Free Software Foundation, sea la versión 3 de la licencia
# o cualquier versión posterior. Este programa se distribuye SIN GARANTÍA
# EXPERSA O IMPLÍCITA, INCLUIDAS LAS DE NO INFRACCIÓN, COMERCIABILIDAD O
# APTITUD PARA UN PROPÓSITO PARTICULAR. Referir a la
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) para más detalles.
#


shinyUI(
  function(request) {
  tagList(
    tags$head(
      tags$script(type = "text/javascript", src = "code.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "github-markdown.css")
      ),
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
            text = "Version: 21.08.3",
            icon = icon("code-branch"),
            status = "info"
          )
        ),
        controlbarIcon = icon("cogs")),
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
            tabName = "nt_dashboard"
          ),
          menuItem(
            text = "Seguimiento Contratos",
            icon = icon("chart-line", lib = "font-awesome"),
            tabName = "seguimiento"),
          menuItem(
            text = "Manuales",
            icon = icon("book-open", lib = "font-awesome"),
            tabName = "manuales")
        )
    ),
      controlbar = dashboardControlbar(
        width = 700,
        controlbarMenu(
          controlbarItem(
            id = "filtros_sideBar",
            title = "Filtros",
            filtros_ui("filtros_sideBar")
          ),
          controlbarItem(
            id = "opciones_sideBar",
            title = "Opciones adicionales",
            opciones_adicionales_ui("opciones_adicionales")
          ),
          controlbarItem(
            id = "manuales_sideBar",
            title = "Manuales",
            manuales_ui("manuales_sideBar"),
            tags$div(style =  "height: 50px")
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
          ),
          tabItem(
            tabName = "seguimiento",
            seguimiento_ui("seguimiento")
          ),
          tabItem(
            tabName = "manuales",
            manuales_ui("manuales")
          )
        )
      )
     )
    )
  }
)
